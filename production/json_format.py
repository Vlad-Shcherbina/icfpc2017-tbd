"""
parse_zzz()
    Converts JSON object to the corresponding datatype from bot_interface.
    Fails early on the unsupported entries, to make it easier to adapt
    to the spec updates.

format_zzz()
    bot_interface.Zzz -> JSON object
"""

import copy
import json

from production.bot_interface import *


class InvalidResponseError(Exception):
    def __init__(self, msg):
        self.msg = msg


REPORT_UNKNOWN_FIELDS = False

def parse_map(d) -> Map:
    d = copy.deepcopy(d)
    raw_map = copy.deepcopy(d)

    sites = d.pop('sites')
    rivers = d.pop('rivers')
    mines = d.pop('mines')

    g = {}
    site_coords = {}

    for site in sites:
        id = site.pop('id')
        # x and y fields are not documented, but they appear to be present
        # on all official maps
        x = site.pop('x', 0.0)
        y = site.pop('y', 0.0)
        if REPORT_UNKNOWN_FIELDS: assert not site, site
        g[id] = set()
        site_coords[id] = x, y

    assert isinstance(mines, list), mines
    for mine in mines:
        assert mine in g, mine

    for river in rivers:
        source = river.pop('source')
        target = river.pop('target')
        if REPORT_UNKNOWN_FIELDS: assert not river, river
        assert target not in g[source]
        g[source].add(target)
        assert source not in g[target]
        g[target].add(source)

    return Map(g=g, mines=set(mines), site_coords=site_coords, raw_map=raw_map)


def parse_settings(d) -> Settings:
    raw_settings = copy.deepcopy(d)
    d = copy.deepcopy(d)
    futures = d.pop('futures', False)
    splurges = d.pop('splurges', False)
    options = d.pop('options', False)
    if REPORT_UNKNOWN_FIELDS: assert not d, d
    return Settings(futures=futures, splurges=splurges, options=options, raw_settings=raw_settings)


def format_settings(s: Settings) -> dict:
    return dict(futures=s.futures, options=s.options, splurges=s.splurges)


def parse_setup_request(d) -> SetupRequest:
    d = copy.deepcopy(d)
    punter = d.pop('punter')
    punters = d.pop('punters')
    map = d.pop('map')
    settings = d.pop('settings', {})
    state = d.pop('state', {})
    if REPORT_UNKNOWN_FIELDS: assert not d, d
    assert 0 <= punter < punters, (punter, punters)
    return SetupRequest(
        punter=punter, punters=punters,
        map=parse_map(map),
        settings=parse_settings(settings))


def parse_setup_response(d, ID=None) -> SetupResponse:
    # Got from player - check for any possible inconsistency
    if not isinstance(d, dict): 
        raise InvalidResponseError('not a valid setup response')
    if (not 'ready' in d):
        raise InvalidResponseError('not a valid setup response')

    if (not 'futures' in d):
        return SetupResponse(ready=ID, state=None, futures={})

    if not isinstance(d['futures'], list):
        raise InvalidResponseError('not a valid setup response')
    futures = {}
    for f in d['futures']:
        if (not isinstance(f, dict) 
                or not 'source' in f 
                or not 'target' in f
                or not isinstance(f['source'], int)
                or not isinstance(f['target'], int)):
            raise InvalidResponseError('not a valid setup response')
        futures[f['source']] = f['target']
    return SetupResponse(ready=ID, state=None, futures=futures)


def format_setup_response(r: SetupResponse):
    d = dict(ready=r.ready, state=r.state)
    if r.futures:
        d['futures'] = [dict(source=k, target=v) for k, v in r.futures.items()]
    return d


def parse_move(d, ID=None) -> Move:
    # Probably got from player - check for any possible inconsistency
    if not isinstance(d, dict):
        raise InvalidResponseError('not a valid move')
    for k in ('pass', 'claim', 'option', 'splurge'):
        if k in d: 
            key = k
            break
    else:
        raise InvalidResponseError('not a valid move')

    d = copy.deepcopy(d)
    p = d.pop(key)
    if REPORT_UNKNOWN_FIELDS: assert not d, d
    if not isinstance(p, dict) or not 'punter' in p:
        raise InvalidResponseError('not a valid move')

    punter = p.pop('punter')
    if ID is not None: punter = ID

    if key == 'pass':
        if REPORT_UNKNOWN_FIELDS: assert not p, p
        return PassMove(punter=punter)
    elif key == 'claim' or key == 'option':
        source = p.pop('source')
        target = p.pop('target')
        if REPORT_UNKNOWN_FIELDS: assert not p, p
        if not isinstance(source, int) or not isinstance(target, int):
            raise InvalidResponseError('not a valid move')
        SomeMove = ClaimMove if key == 'claim' else OptionMove
        return SomeMove(punter=punter, source=source, target=target)
    elif key == 'splurge':
        route = p.pop('route')
        if REPORT_UNKNOWN_FIELDS: assert not p, p
        if not isinstance(route, list):
            raise InvalidResponseError('not a valid move')
        return SplurgeMove(punter=punter, route=route)
    else:
        assert False, key


def format_move(m: Move, error=None, timespan=None, original=None, moveno=None):
    result = { m.key() : {'punter': m.punter}}
    if isinstance(m, PassMove):
        pass
    elif isinstance(m, ClaimMove) or  isinstance(m, OptionMove):
        result[m.key()].update({'source': m.source, 'target': m.target})
    elif isinstance(m, SplurgeMove):
        result[m.key()].update({'route': m.route})
    else:
        assert False, m
    
    if error is not None:
        result[m.key()].update({'error': error})
    elif timespan is not None:
        result[m.key()].update({'timespan': timespan})
    if original is not None:
        result[m.key()].update({'original': format_move(original)})
    if moveno is not None:
        result[m.key()].update({'moveno' : moveno})
    return result


def parse_gameplay_request(d) -> GameplayRequest:
    d = copy.deepcopy(d)
    m = d.pop('move')
    state = d.pop('state', None)
    if REPORT_UNKNOWN_FIELDS: assert not d, d
    moves = m.pop('moves')
    if REPORT_UNKNOWN_FIELDS: assert not m, m
    raw_moves = copy.deepcopy(moves)
    moves = [parse_move(move) for move in moves]
    return GameplayRequest(moves=moves, state=state, raw_moves=raw_moves)


def parse_gameplay_response(d, ID=None) -> GameplayResponse:
    # Got from player - check for any possible inconsistency
    if not isinstance(d, dict) or not 'move' in d:
        raise InvalidResponseError('not a valid move')
    move = parse_move(d['move'], ID)
    return GameplayResponse(move=move, state='')


def format_gameplay_response(r: GameplayResponse):
    d = format_move(r.move)
    assert 'state' not in d
    d['state'] = r.state
    return d


def parse_score_request(d) -> ScoreRequest:
    d = copy.deepcopy(d)
    s = d.pop('stop')
    state = d.pop('state', None)
    if REPORT_UNKNOWN_FIELDS: assert not d, d
    moves = s.pop('moves')
    scores = s.pop('scores')
    if REPORT_UNKNOWN_FIELDS: assert not s, s

    moves = [parse_move(move) for move in moves]

    score_by_punter = {}
    for score in scores:
        punter = score.pop('punter')
        points = score.pop('score')
        if REPORT_UNKNOWN_FIELDS: assert not score, score
        assert punter not in score_by_punter
        score_by_punter[punter] = points

    return ScoreRequest(
        moves=moves, score_by_punter=score_by_punter, state=state)


def format_total_score(totals, names):
    d = []
    for i in range(len(names)):
        d.append(dict(punter=i, 
                      nickname=names[i], 
                      score=sum(totals[i]), 
                      futures=[totals[i][1:]]))


def parse_any_request(d) -> Union[SetupRequest, GameplayRequest, ScoreRequest]:
    if 'map' in d:
        return parse_setup_request(d)
    elif 'move' in d:
        return parse_gameplay_request(d)
    elif 'stop' in d:
        return parse_score_request(d)
    else:
        assert False, d
