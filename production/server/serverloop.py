from typing import NamedTuple, List, Optional
from itertools import compress
from collections import defaultdict
import socket
import time
import threading
import sys
from random import randrange

from production.server.connection import NetworkConnection, Timeout, Dead
from production.server.matchmaker import make_match, PlayerRating, Rating
from production.server.gameloop import gameloop
from production.bot_interface import Settings, Map

import logging
logger = logging.getLogger(__name__)


'''
Local usage:
serverloop()

from cmd:
production\botscript.py john_doe dumb -c
production\botscript.py jane_doe random -c
production\botscript.py batman cpp -c

Warning! Using names is temporary and will be replaced by tokens.
'''


HANDSHAKE_TIMELIMIT = 0.5
CONNECTIONS_PER_TOKEN = 10


class WaitingPlayer(NamedTuple):
    token: str
    name: str
    rating: str             # TEMP!!!
    conn: NetworkConnection
    deadline: int

    def short(self):
        '''Shorter version for matchmaker.'''
        return PlayerRating(name=self.name, 
                            rating=self.rating, 
                            deadline=self.deadline)


class ServerStatistics():
    ''''''

    def __init__(self):
        self.STATRANGE = 100
        self.connected = 0
        self.disconnected_at = [None] * self.STATRANGE
        self.disc_i = 0
        self.connected_at = [None] * self.STATRANGE
        self.conn_i = 0
        self.last_connection = time.time()
        self.started_at = [None] * self.STATRANGE
        self.start_i = 0

    def reg_connect(self):
        self.connected += 1
        self.connected_at[self.conn_i] = time.time() - self.last_connection
        self.last_connection = time.time()
        self.conn_i = (self.conn_i + 1) % self.STATRANGE

    def reg_disconnect(self, deadline):
        self.disconnected_at[self.disc_i] = deadline - time.time() + 60
        self.disc_i = (self.disc_i + 1) % self.STATRANGE

    def reg_start(self, deadline):
        self.started_at[self.start_i] = deadline - time.time() + 60
        self.start_i = (self.start_i + 1) % self.STATRANGE

    def connection_rate(self):
        N = self.STATRANGE if self.connected_at[-1] is not None else self.conn_i
        if N == 0: return 1e-6
        return N / sum(self.connected_at[:N])

    def avg_connected(self):
        N = self.STATRANGE if self.connected_at[-1] is not None else self.conn_i
        if N == 0: return 0.0
        return sum(self.connected_at[:N]) / N

    def avg_disconnected(self):
        N = self.STATRANGE if self.disconnected_at[-1] is not None else self.disc_i
        if N == 0: return 0.0
        return sum(self.disconnected_at[:N]) / N

    def avg_started(self):
        N = self.STATRANGE if self.started_at[-1] is not None else self.start_i
        if N == 0: return 0.0
        return sum(self.started_at[:N]) / N


class GameThread(threading.Thread):
    '''Thread derivative class to start gameloop and return result.'''
    def __init__(self, ID: int, 
                       players: List[WaitingPlayer], 
                       m: Map, 
                       settings: Settings):
        threading.Thread.__init__(self)
        self.ID = ID
        self.players = players
        self.m = m
        self.settings = settings

        self.replay = None
        self.scores = None
        self.running = False

    def run(self):
        connections = [p.conn for p in self.players]
        names = [p.name for p in self.players]
        logger.info(f'Game {self.ID} started with ' + str(names))
        self.running = True
        self.replay, self.scores = gameloop(self.m, self.settings, connections, names)
        logger.info(f'Game {self.ID} finished.')
        self.running = False


def connectserver():
    '''Initial server connection. Return server socket.'''
    address = '127.0.0.1', 42424
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind(address)
    server.settimeout(5)
    server.listen(10)
    return server


def handshake(conn: socket.socket, conns_by_token) -> Optional[WaitingPlayer]:
    '''Handshake phase. Exchange messages and get player stats from DB.

    Return waiting player if protocol was satisfied, None otherwise.'''
    conn = NetworkConnection(conn)
    r = conn.receive(time.time() + HANDSHAKE_TIMELIMIT)
    if isinstance(r, Timeout) or isinstance(r, Dead):
        conn.close()
        return None
    if 'me' not in r:
        conn.zombify('bad handshake request')
        return None

    token = str(r['me'])
    if conns_by_token[token] >= CONNECTIONS_PER_TOKEN:
        conn.zombify('connections limit exceeded')
        return None

    conns_by_token[token] += 1
    name = token                # temp!
    # get name and rating from DB
    conn.send({'you': name})
    return WaitingPlayer(token, name, '', conn, time.time() + 60)


def _remove_from_conns(conns_by_token, token):
    # Player disconnected; if no connections are open, delete from dictionary
    conns_by_token[token] -= 1
    if conns_by_token[token] <= 0:
        del conns_by_token[token]


def _add_new_connection(server, waiting, conns_by_token, stats):
    try:
        conn, addr = server.accept()
    except socket.timeout:
        logger.debug('timed out')
        return
    player = handshake(conn, conns_by_token)
    if player: 
        stats.reg_connect()
        waiting.append(player)


def _check_disconnected(waiting, conns_by_token, stats):
    # return revised waiting list
    for p in waiting:
        p.conn.recheck()
        if not p.conn.alive: 
            stats.reg_disconnect(p.deadline)
            _remove_from_conns(conns_by_token, p.token)
    return list(compress(waiting, (p.conn.alive for p in waiting)))


def _call_match_maker(waiting, games, stats):
    # return revised waiting list
    match = make_match([p.short() for p in waiting], stats.connection_rate())
    if match is None: 
        return waiting
    players = list(compress(waiting, match.participants))
    game = GameThread(randrange(100), players, match.map, match.settings)
    for p in players:
        stats.reg_start(p.deadline)
    game.start()
    games.append(game)
    return list(compress(waiting, (not x for x in match.participants)))


def _resolve_ended_games(games, conns_by_token, stats):
    # return revised game list
    games_running = []
    for g in games:
        if g.running:
            games_running.append(g)
            continue
        g.join()
        for p in g.players:
            _remove_from_conns(conns_by_token, p.token)
        # replay and rating to DB
    return games_running


def serverloop():
    '''Main loop. Accept connections, gather players for match and start games.'''
    server = connectserver()
    logger.info('Server started successfully')
    waiting = []
    conns_by_token = defaultdict(int)
    games = []
    stats = ServerStatistics()
    while True:
        # if there is new connection request, accept it
        _add_new_connection(server, waiting, conns_by_token, stats)

        # if smb's already disconnected, remove them
        waiting = _check_disconnected(waiting, conns_by_token, stats)

        # make match and start a new game
        waiting = _call_match_maker(waiting, games, stats)

        # if any game ended, clean up
        games = _resolve_ended_games(games, conns_by_token, stats)
    

if __name__ == '__main__':
    logging.basicConfig(
            level=logging.DEBUG,
            stream=sys.stdout,
            format='%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')
    #logging.getLogger('production.server.connection').setLevel(logging.INFO)
    serverloop()
