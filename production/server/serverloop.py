from typing import NamedTuple, List, Optional
from itertools import compress
from collections import defaultdict
import socket
import time
import threading
import sys
from random import randrange, random

from production.server.connection import NetworkConnection, Timeout, Dead
from production.server.gameloop import gameloop
from production.bot_interface import Settings, Map
from production.json_format import parse_handshake_response, InvalidResponseError
import production.server.matchmaker as matchmaker

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

SC_STOP = 'stop server'
SERVER_COMMANDS = (SC_STOP, )


#------------------------ AUXILIARY CLASSES ----------------------------#

class WaitingPlayer(NamedTuple):
    token: str
    name: str
    rating: matchmaker.PlayerStats
    conn: NetworkConnection
    deadline: int

    def conninfo(self):
        '''Shorter version for matchmaker.'''
        return matchmaker.ConnInfo(stats=self.rating, deadline=self.deadline)


class ServerStatistics():
    '''Gather statistics about connection, disconnection and game finding timespans'''

    def __init__(self):
        self.STATRANGE = 100   # collect data only about that much last events.
        self.last_connection = time.time()

        self.n_connected = 0
        self.connected_at = [None] * self.STATRANGE
        self.c_ind = 0

        self.n_disconnected = 0
        self.disconnected_at = [None] * self.STATRANGE
        self.d_ind = 0

        self.started_at = [None] * self.STATRANGE
        self.s_ind = 0

        self.games_running = 0

    def log(self):
        logger.info(f'Server statistics:\n'
            f'total connections:                        {self.n_connected}\n'
            f'average connection rate:                  {self.avg_connected():.3f}\n'
            f'average disconnection time while waiting: {self.avg_disconnected():.3f}\n'
            f'disconnections while waiting:             '
            f'{(self.n_disconnected/self.n_connected*100):.1f}%\n'
            f'averate waiting time before game:         {self.avg_started():.3f}\n'
            f'games currently running:                  {self.games_running}')

    def reg_connect(self):
        self.n_connected += 1
        self.connected_at[self.c_ind] = time.time() - self.last_connection
        self.last_connection = time.time()
        self.c_ind = (self.c_ind + 1) % self.STATRANGE
        if self.n_connected % 10 == 0: self.log()    # Temp! make 100

    def reg_disconnect(self, deadline):
        self.n_disconnected += 1
        self.disconnected_at[self.d_ind] = time.time() - deadline + 60
        self.d_ind = (self.d_ind + 1) % self.STATRANGE

    def reg_start(self, deadline):
        self.started_at[self.s_ind] = time.time() - deadline + 60
        self.s_ind = (self.s_ind + 1) % self.STATRANGE

    def connection_rate(self):
        N = self.STATRANGE if self.connected_at[-1] is not None else self.c_ind
        if N == 0: return 1e-6
        return N / sum(self.connected_at[:N])

    def avg_connected(self):
        N = self.STATRANGE if self.connected_at[-1] is not None else self.c_ind
        if N == 0: return 0.0
        return sum(self.connected_at[:N]) / N

    def avg_disconnected(self):
        N = self.STATRANGE if self.disconnected_at[-1] is not None else self.d_ind
        if N == 0: return 0.0
        return sum(self.disconnected_at[:N]) / N

    def avg_started(self):
        N = self.STATRANGE if self.started_at[-1] is not None else self.s_ind
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
        self.replay, self.scores = gameloop(self.m, self.settings, connections, names)
        logger.info(f'Game {self.ID} finished.')
        self.running = False


#--------------------------- SERVER LOOP -------------------------------#

def connectserver(aux: bool):
    '''Initial server connection. Return server socket.'''
    address = '127.0.0.1', (45454 if aux else 42424)
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind(address)
    server.settimeout(.5 if aux else 5)
    server.listen(10)
    return server


def _get_command(server):
    '''Send technical commands to operate server.'''
    try:
        conn, addr = server.accept()
    except socket.timeout:
        return
    conn = NetworkConnection(conn)
    r = conn.receive(time.time() + 0.5)
    # !!! Add password
    if isinstance(r, dict) and 'command' in r:
        conn.kick('accepted')
        return r['command']


def handshake(conn: socket.socket, conns_by_token, accept) -> Optional[WaitingPlayer]:
    '''Handshake phase. Exchange messages and get player stats from DB.

    Return waiting player if protocol was satisfied, None otherwise.'''
    conn = NetworkConnection(conn)
    r = conn.receive(time.time() + HANDSHAKE_TIMELIMIT)
    if isinstance(r, Timeout) or isinstance(r, Dead):
        conn.close()
        return None
    try:
        token = parse_handshake_response(r)
    except InvalidResponseError as e:
        conn.kick(e.msg)
        return None
    if not accept:
        conn.kick('Server is about to reboot. Try again later.')
        return None
    if conns_by_token[token] >= CONNECTIONS_PER_TOKEN:
        conn.kick('connections limit exceeded')
        return None

    conns_by_token[token] += 1
    player = load_player(token)     # temp! get from db_connections
    conn.send({'you': player.name})
    return WaitingPlayer(token, player.name, load_player(player.name), conn, time.time() + 60)


def _accept_new_connection(server, waiting, conns_by_token, accept, db, stats):
    try:
        conn, addr = server.accept()
    except socket.timeout:
        logger.debug('timed out')
        return
    player = handshake(conn, conns_by_token, accept)
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
    waiting[:] = list(compress(waiting, (p.conn.alive for p in waiting)))


def _remove_from_conns(conns_by_token, token):
    # Player disconnected; if no connections are open, delete from dictionary
    conns_by_token[token] -= 1
    if conns_by_token[token] <= 0:
        del conns_by_token[token]


def _call_match_maker(waiting, games, stats):
    # return revised waiting list
    match = matchmaker.match([p.conninfo() for p in waiting], stats.connection_rate())
    if match is None: 
        return waiting
    players = list(compress(waiting, match.participants))
    game = GameThread(randrange(100), players, match.map, match.settings)
    for p in players:
        stats.reg_start(p.deadline)
    game.running = True
    game.start()
    games.append(game)
    waiting[:] = list(compress(waiting, (not x for x in match.participants)))


def _resolve_ended_games(games, conns_by_token, db, stats):
    # return revised game list
    games_running = []
    for g in games:
        if g.running:
            games_running.append(g)
            continue
        logger.debug(f'Resolving game {g.ID}')
        g.join()
        for p in g.players:
            _remove_from_conns(conns_by_token, p.token)
        # replay and rating to DB
    games[:] = games_running


def serverloop():
    '''Main loop. Accept connections, gather players for match and start games.'''
    server = connectserver(aux=False)
    aux_server = connectserver(aux=True)
    logger.info('Server started successfully')
    waiting = []
    conns_by_token = defaultdict(int)
    games = []
    stats = ServerStatistics()
    accept_players = True
    db = None

    while accept_players or games:
        # get technical commands
        command = _get_command(aux_server)
        if command == SC_STOP:
            accept_players = False

        # if there is new connection request, accept it
        _accept_new_connection(server, waiting, conns_by_token, accept_players, db, stats)

        # if smb's already disconnected, remove them
        _check_disconnected(waiting, conns_by_token, stats)

        # make match and start a new game
        _call_match_maker(waiting, games, stats)

        # if any game ended, clean up
        _resolve_ended_games(games, conns_by_token, db, stats)

        stats.games_running = len(games)
    

# TEMP!
def load_player(token: str):
    # get id from database
    return matchmaker.PlayerStats(name=token,
                  games=randrange(1000), 
                  mu=random() * 100,
                  sigma=random() * 50 / 3,
                  large=random(),
                  medium=random(),
                  small=random(),
                  futures=random(),
                  options=random(),
                  splurges=random(),
                  opponents={})


if __name__ == '__main__':
    logging.basicConfig(
            level=logging.DEBUG,
            stream=sys.stdout,
            format='%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')
    #logging.getLogger('production.server.connection').setLevel(logging.INFO)
    serverloop()
