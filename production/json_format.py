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
from production.server.gameholder import Gameboard


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

def parse_board(d) -> Gameboard:
    m = parse_map(d)
    return Gameboard(adj=m.g, mines=m.mines)

def parse_settings(d) -> Settings:
    raw_settings = copy.deepcopy(d)
    d = copy.deepcopy(d)
    futures = d.pop('futures', False)
    splurges = d.pop('splurges', False)
    options = d.pop('options', False)
    if REPORT_UNKNOWN_FIELDS: assert not d, d
    return Settings(futures=futures, splurges=splurges, options=options, raw_settings=raw_settings)


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


def format_setup_response(r: SetupResponse):
    d = dict(ready=r.ready, state=r.state)
    if r.futures:
        d['futures'] = [dict(source=k, target=v) for k, v in r.futures.items()]
    return d


def parse_move(d) -> Move:
    d = copy.deepcopy(d)
    if 'claim' in d:
        p = d.pop('claim')
        if REPORT_UNKNOWN_FIELDS: assert not d, d
        punter = p.pop('punter')
        source = p.pop('source')
        target = p.pop('target')
        if REPORT_UNKNOWN_FIELDS: assert not p, p
        return ClaimMove(punter=punter, source=source, target=target)
    elif 'pass' in d:
        p = d.pop('pass')
        if REPORT_UNKNOWN_FIELDS: assert not d, d
        punter = p.pop('punter')
        if REPORT_UNKNOWN_FIELDS: assert not p, p
        return PassMove(punter=punter)
    elif 'splurge' in d:
        p = d.pop('splurge')
        if REPORT_UNKNOWN_FIELDS: assert not d, d
        punter = p.pop('punter')
        route = p.pop('route')
        if REPORT_UNKNOWN_FIELDS: assert not p, p
        return SplurgeMove(punter=punter, route=route)
    elif 'option' in d:
        p = d.pop('option')
        if REPORT_UNKNOWN_FIELDS: assert not d, d
        punter = p.pop('punter')
        source = p.pop('source')
        target = p.pop('target')
        if REPORT_UNKNOWN_FIELDS: assert not p, p
        return OptionMove(punter=punter, source=source, target=target)

    else:
        assert False, d

def format_move(m: Move, add_error=False):
    result = { m.key() : {'punter': m.punter}}
    if isinstance(m, PassMove):
        if add_error:
            result['pass'].update({'error': m.error})
    elif isinstance(m, ClaimMove) or  isinstance(m, OptionMove):
        result[m.key()].update({'source': m.source, 'target': m.target})
    elif isinstance(m, SplurgeMove):
        result[m.key()].update({'route': m.route})
    else:
        assert False, m
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


def parse_any_request(d) -> Union[SetupRequest, GameplayRequest, ScoreRequest]:
    if 'map' in d:
        return parse_setup_request(d)
    elif 'move' in d:
        return parse_gameplay_request(d)
    elif 'stop' in d:
        return parse_score_request(d)
    else:
        assert False, d
