# !python3

# source:
# https://github.com/Vlad-Shcherbina/icfpc2017-tbd/blob/master/production/json_format.py

import copy
import json

import logging;
logger = logging.getLogger(__name__)

from server.server_interface import *


def parse_map(d) -> Board:
    d = copy.deepcopy(d)
    raw_map = copy.deepcopy(d)

    sites = d.pop('sites')
    rivers = d.pop('rivers')
    mines = d.pop('mines')

    adj = {}
    site_coords = {}

    for site in sites:
        id = site.pop('id')
        # x and y fields are not documented, but they appear to be present
        # on all official maps
        # x = site.pop('x', 0.0)
        # y = site.pop('y', 0.0)
        adj[id] = set()
        #site_coords[id] = x, y

    assert isinstance(mines, list), mines
    for mine in mines:
        assert mine in adj, mine

    for river in rivers:
        source = river.pop('source')
        target = river.pop('target')
        assert target not in adj[source]
        adj[source].add(target)
        assert source not in adj[target]
        adj[target].add(source)

    return Board(adj=adj, mines=set(mines))


def parse_settings(d) -> Settings:
    raw_settings = copy.deepcopy(d)
    d = copy.deepcopy(d)
    futures = d.pop('futures', False)
    splurges = d.pop('splurges', False)
    options = d.pop('options', False)
    return Settings(futures=futures, splurges=splurges, options=options, raw_settings=raw_settings)


# def parse_setup_request(d) -> SetupRequest:
#     d = copy.deepcopy(d)
#     punter = d.pop('punter')
#     punters = d.pop('punters')
#     map = d.pop('map')
#     settings = d.pop('settings', {})
#     state = d.pop('state', {})
#     assert 0 <= punter < punters, (punter, punters)
#     return SetupRequest(
#         punter=punter, punters=punters,
#         map=parse_map(map),
#         settings=parse_settings(settings))


# def format_setup_response(r: SetupResponse):
#     d = dict(ready=r.ready, state=r.state)
#     if r.futures:
#         d['futures'] = [dict(source=k, target=v) for k, v in r.futures.items()]
#     return d


def parse_move(d) -> Move:
    d = copy.deepcopy(d)
    if 'claim' in d:
        p = d.pop('claim')
        punter = p.pop('punter')
        source = p.pop('source')
        target = p.pop('target')
        return ClaimMove(punter=punter, source=source, target=target)
    elif 'pass' in d:
        p = d.pop('pass')
        punter = p.pop('punter')
        return PassMove(punter=punter, msg='')
    elif 'splurge' in d:
        p = d.pop('splurge')
        punter = p.pop('punter')
        route = p.pop('route')
        return SplurgeMove(punter=punter, route=route)
    elif 'option' in d:
        p = d.pop('option')
        punter = p.pop('punter')
        source = p.pop('source')
        target = p.pop('target')
        return OptionMove(punter=punter, source=source, target=target)

    else:
        assert False, d

def format_move(m: Move, extended=False):
    key = m.key()
    result = {key: dict(punter=m.punter)}

    if isinstance(m, ClaimMove) or isinstance(m, OptionMove):
        result[key].update(dict(source=m.source, target=m.target))
    elif isinstance(m, PassMove):
        if extended: 
            result[key].update(dict(msg=m.msg))
    elif isinstance(m, SplurgeMove):
        result[key].update(dict(route=m.route))
    else:
        assert False, m
    if extended:
        result[key].update(dict(timespan=m.timespan))
    return result


# def parse_gameplay_request(d) -> GameplayRequest:
#     d = copy.deepcopy(d)
#     m = d.pop('move')
#     state = d.pop('state', None)
#     moves = m.pop('moves')
#     raw_moves = copy.deepcopy(moves)
#     moves = [parse_move(move) for move in moves]
#     return GameplayRequest(moves=moves, state=state, raw_moves=raw_moves)


# def format_gameplay_response(r):
#     d = format_move(r.move)
#     assert 'state' not in d
#     d['state'] = r.state
#     return d


# def parse_score_request(d) -> ScoreResponse:
#     d = copy.deepcopy(d)
#     s = d.pop('stop')
#     state = d.pop('state', None)
#     moves = s.pop('moves')
#     scores = s.pop('scores')

#     moves = [parse_move(move) for move in moves]

#     score_by_punter = {}
#     for score in scores:
#         punter = score.pop('punter')
#         points = score.pop('score')
#         assert punter not in score_by_punter
#         score_by_punter[punter] = points

#     return ScoreRequest(
#         moves=moves, score_by_punter=score_by_punter, state=state)


# def parse_any_request(d) -> Union[SetupRequest, GameplayRequest]: #, ScoreRequest]:
#     if 'map' in d:
#         return parse_setup_request(d)
#     elif 'move' in d:
#         return parse_gameplay_request(d)
#     elif 'stop' in d:
#         return parse_score_request(d)
#     else:
#         assert False, d
