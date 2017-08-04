"""
parse_zzz()
    Converts JSON object to the corresponding datatype from bot_interface.
    Destroys the JSON object in the process.
    Fails early on the unsupported entries, to make it easier to adapt
    to the spec updates.

format_zzz()
    bot_interface.Zzz -> JSON object
"""

import json

from production.bot_interface import *


def parse_map(d) -> Map:
    sites = d.pop('sites')
    rivers = d.pop('rivers')
    mines = d.pop('mines')

    g = {}
    site_coords = {}

    for site in sites:
        id = site.pop('id')
        # x and y fields are not documented, but they appear to be present
        # on all official maps
        x = site.pop('x')
        y = site.pop('y')
        assert not site, site
        g[id] = set()
        site_coords[id] = x, y

    assert isinstance(mines, list), mines
    for mine in mines:
        assert mine in g, mine

    for river in rivers:
        source = river.pop('source')
        target = river.pop('target')
        assert not river, river
        assert target not in g[source]
        g[source].add(target)
        assert source not in g[target]
        g[target].add(source)

    return Map(g=g, mines=set(mines), site_coords=site_coords)


def parse_setup_request(d) -> SetupRequest:
    punter = d.pop('punter')
    punters = d.pop('punters')
    map = d.pop(map)
    assert not d, d
    return SetupRequest(punter=punter, punters=punters, map=map)


def format_setup_response(r: SetupResponse):
    return dict(ready=r.ready, state=r.state)


def parse_move(d) -> Move:
    if 'claim' in d:
        claim = d.pop('claim')
        assert not d, d
        punter = claim.pop('punter')
        source = claim.pop('source')
        target = claim.pop('target')
        assert not claim, claim
        return ClaimMove(punter=punter, source=source, target=target)
    elif 'pass' in d:
        p = d.pop('pass')
        assert not d, d
        punter = p.pop('punter')
        assert not p, p
        return PassMove(punter=punter)
    else:
        assert False, d

def format_move(m: Move):
    if isinstance(m, ClaimMove):
        return dict(
            claim=dict(punter=m.punter, source=m.source, target=m.target))
    elif isinstance(m, PassMove):
        return {'pass': dict(punter=m.punter)}
    else:
        assert False, m


def parse_gameplay_request(d) -> GameplayRequest:
    m = d.pop('move')
    state = d.pop('state')
    assert not d, d
    moves = m.pop('moves')
    assert not m, m
    moves = [parse_move(move) for move in moves]
    return GameplayRequest(moves=moves, state=state)


def format_gameplay_response(r: GameplayResponse):
    d = format_move(r.move)
    assert 'state' not in d
    d['state'] = r.state
    return d
