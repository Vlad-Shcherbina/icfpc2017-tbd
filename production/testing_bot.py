import socket
import time

from production.server.connection import NetworkConnection
from production.server.db_connection import connect_to_db
from production.bot_interface import *
import production.json_format as json_format
import production.server.config as config

import logging
logger = logging.getLogger(__name__)


'''Testing bot mostly claims illegal moves and asserts they are not taken.

Run it along with other bots, so that server gets enough players 
to start games.
Don't forget to add it to database before launching!'''


botname = 'zzz_wannacry'   # add it to database before launching!


#--------------------------- AUXILIARY ---------------------------------#

def connect(host, port):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((host, port))
    s.settimeout(15)
    return NetworkConnection(s)

def handshake(conn, token):
    conn.send({ 'me' : token })
    resp = conn.receive(time.time() + 65)
    assert 'you' in resp


def assertpass(request, punter):
    for mv in request.moves:
        if mv.punter == punter:
            assert mv.key() == 'pass', mv.key()


def unexistant_river(g: Graph):
    for source in g.keys():
        for target in g.keys():
            if source == target: continue
            if target in g[source]: continue
            return (source, target)
    return None


def river_not_claimed(g, claimed_by_me, claimed_by_others):
    for source in g.keys():
        for target in g[source]:
            if (pair(source, target) not in claimed_by_me
                and pair(source, target) not in claimed_by_others) :
                return source, target


def pair(a, b):
    return (min(a, b), max(a, b))


def make_move(conn, move, me, claimed_by_others, optioned_by_others):
    conn.send(move)
    req = json_format.parse_gameplay_request(conn.receive(time.time() + 1000))
    for move in req.moves:
        if move.punter == me:
            continue
        if move.key() == 'claim':
            claimed_by_others.add(pair(move.source, move.target))
        elif move.key() == 'option':
            optioned_by_others.add(pair(move.source, move.target))
        elif move.key() == 'splurge':
            for s, t in zip(move.route, move.route[1:]):
                if pair(s, t) in claimed_by_others:
                    optioned_by_others.add(pair(s, t))
                else:
                    claimed_by_others.add(pair(s, t))
    return req
    

#----------------------------- TESTS -----------------------------------#

def testing_bad_handshake():
    conn = connect(config.GAME_SERVER_ADDRESS, config.GAME_SERVER_PORT)
    conn.send({ 'hello' : 'there' })
    response = conn.receive(time.time() + 1)
    assert isinstance(response, dict) and 'kicked' in response, response
    conn.close()

    conn = connect(config.GAME_SERVER_ADDRESS, config.GAME_SERVER_PORT)
    conn.send({ 'me' : botname })
    response = conn.receive(time.time() + 1)
    assert isinstance(response, dict) and 'kicked' in response, response
    conn.close()
    logger.info('bad handshakes: PASSED')


def testing_format(token):
    bad_requests = [
        { 'hello' : 'there' },                      # no move in request
        [ 'claim', 'pass', { 'don-t' : 'worry' }],  # not a dict
        'optioned moves also',                      # not a dict again
    ]
    for r in bad_requests:
        logger.debug('...testing: ' + str(r))
        conn = connect(config.GAME_SERVER_ADDRESS, config.GAME_SERVER_PORT)
        handshake(conn, token)
        logger.debug('waiting for a game')
        setup = json_format.parse_setup_request(conn.receive(time.time() + 1000))
        conn.send(r)
        response = conn.receive(time.time() + 1000)
        logger.debug(response)
        assert 'kicked' in response, (r, response)
        conn.close()
    logger.info('invalid format: PASSED')


def testing_claims(token):
    conn = connect(config.GAME_SERVER_ADDRESS, config.GAME_SERVER_PORT)
    handshake(conn, token)
    logger.debug('waiting for a game')
    setup = json_format.parse_setup_request(conn.receive(time.time() + 1000))
    me = setup.punter
    m = setup.map
    claimed_by_me = set()
    claimed_by_others = set()

    make_move(conn, {'ready' : me}, me, claimed_by_others, set())

    # claim unexistant site
    logger.debug('...testing: claiming unexistant site')
    site = 0
    while site in m.g: site += 1
    move = {'claim' : {'punter' : me, 'source' : site, 'target' : 0}}
    request = make_move(conn, move, me, claimed_by_others, set())
    assertpass(request, me)

    # claim same_site river
    logger.debug('...testing: claiming loop edge')
    site = next(iter(m.g))
    move = {'claim' : {'punter' : me, 'source' : site, 'target' : site}}
    request = make_move(conn, move, me, claimed_by_others, set())
    assertpass(request, me)

    # claim unexistant river
    logger.debug('...testing: claiming unexistant river')
    source, target = unexistant_river(m.g)
    move = {'claim' : {'punter' : me, 'source' : source, 'target' : target}}
    request = make_move(conn, move, me, claimed_by_others, set())
    assertpass(request, me)

    # claim river claimed by me
    logger.debug('...testing: claiming river claimed by me')
    source, target = river_not_claimed(m.g, claimed_by_me, claimed_by_others)
    move = {'claim' : {'punter' : -1, 'source' : source, 'target' : target}}
    make_move(conn, move, me, claimed_by_others, set())
    request = make_move(conn, move, me, claimed_by_others, set())
    assertpass(request, me)

    # claim river claimed by others
    logger.debug('...testing: claiming river claimed by others')
    if claimed_by_others:
        source, target = next(iter(claimed_by_others))
        move = {'claim' : {'punter' : me, 'source' : source, 'target' : target}}
        request = make_move(conn, move, me, claimed_by_others, set())
        assertpass(request, me)

    conn.close()
    logger.info('illegal claims: PASSED')


def testing_wrong_settings(token):
    # option edge where no options in settings
    pass
    

def testing_options(token):
    while True:
        conn = connect(config.GAME_SERVER_ADDRESS, config.GAME_SERVER_PORT)
        handshake(conn, token)
        logger.debug('waiting for a game')
        setup = json_format.parse_setup_request(conn.receive(time.time() + 1000))
        if setup.settings.options:
            break
        conn.close()
    me = setup.punter
    m = setup.map
    claimed_by_me = set()
    claimed_by_others = set()
    optioned_by_me = set()
    optioned_by_others = set()

    make_move(conn, {'ready' : me}, me, claimed_by_others, set())

    # option unexistant edge
    logger.debug('...testing: optioning unexistant edge')
    source, target = unexistant_river(m.g)
    move = {'option' : {'punter' : me, 'source' : source, 'target' : target}}
    request = make_move(conn, move, me, claimed_by_others, optioned_by_others)
    assertpass(request, me)

    # option unclaimed edge
    logger.debug('...testing: optioning unclaimed edge')
    source, target = river_not_claimed(m.g, claimed_by_me, claimed_by_others)
    move = {'option' : {'punter' : me, 'source' : source, 'target' : target}}
    request = make_move(conn, move, me, claimed_by_others, optioned_by_others)
    assertpass(request, me)

    # option edge claimed by me
    logger.debug('...testing: optioning edge claimed by me')
    source, target = river_not_claimed(m.g, claimed_by_me, claimed_by_others)
    move = {'claim' : {'punter' : me, 'source' : source, 'target' : target}}
    claimed_by_me.add(pair(source, target))
    make_move(conn, move, me, claimed_by_others, optioned_by_others)

    move = {'option' : {'punter' : me, 'source' : source, 'target' : target}}
    request = make_move(conn, move, me, claimed_by_others, optioned_by_others)
    assertpass(request, me)

    # option optioned edge
    logger.debug('...testing: optioning edge already optioned')
    if optioned_by_others:
        # already have optioned
        source, target = next(iter(optioned_by_others))
        move = {'option' : {'punter' : me, 'source' : source, 'target' : target}}
    else:
        # nobody options - option myself first
        for s in claimed_by_others:
            if not s in optioned_by_others:
                break
        source, target = s
        move = {'option' : {'punter' : me, 'source' : source, 'target' : target}}
        optioned_by_me.add(pair(source, target))
        make_move(conn, move, me, claimed_by_others, optioned_by_others)
    request = make_move(conn, move, me, claimed_by_others, optioned_by_others)
    assertpass(request, me)

    # option edge when no left options
    logger.debug('...testing: optioning edge when no options left')
    for _ in range(len(m.mines) + 1):
        source, target = river_not_claimed(m.g, claimed_by_me, claimed_by_others)
        move = {'claim' : {'punter' : me, 'source' : source, 'target' : target}}
        claimed_by_me.add(pair(source, target))
        make_move(conn, move, me, claimed_by_others, optioned_by_others)
        move = {'option' : {'punter' : me, 'source' : source, 'target' : target}}
        optioned_by_me.add(pair(source, target))  # last one is erroneous
        request = make_move(conn, move, me, claimed_by_others, optioned_by_others)
    assertpass(request, me)
    
    conn.close()
    logger.info('illegal options: PASSED')


def main():
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT token from players WHERE name=%s;''', (botname,))
        token = cursor.fetchone()[0]

    testing_bad_handshake()
    #testing_format(token)
    testing_claims(token)
    testing_options(token)


    # splurge when there are no splurges

    # splurge unexistant edge

    # splurge illegal options, see before

    # splurge river twice

    # splurge site twice

    # futures where no futures

    # futures not from mine








if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG,
        format='%(levelname)1s %(message)s',
        )
    main()