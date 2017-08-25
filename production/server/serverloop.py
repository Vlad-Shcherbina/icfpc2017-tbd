from typing import NamedTuple, List, Optional
from itertools import compress
from collections import defaultdict
from datetime import datetime
import socket
import time
import threading
import sys
import json
from random import randrange, random

from production.server.connection import NetworkConnection, Timeout, Dead
from production.server.gameloop import gameloop
from production.bot_interface import Settings, Map
from production.server.server_interface import *
from production.json_format import parse_handshake_response, InvalidResponseError
from production.server.matchmaker import makematch, revise_players
from production.server.db_connection import connect_to_db
from production.server import config

import logging
logger = logging.getLogger(__name__)


'''
Local usage:
serverloop()
or from cmd:
serverloop.py

from cmd:
production\botscript.py zzz_julie random -c
production\botscript.py zzz_yahoo dumb -c
production\botscript.py zzz_nevermore cpp -c

'''


HANDSHAKE_TIMELIMIT = 1
CONNECTIONS_PER_TOKEN = 10

SC_STOP = 'stop server'
SERVER_COMMANDS = (SC_STOP, )


#------------------------ AUXILIARY CLASSES ----------------------------#

class ConnectedPlayer(NamedTuple):
    token: str
    stats: PlayerStats
    conn: NetworkConnection
    deadline: int

    def conninfo(self):
        '''Shorter version for matchmaker.'''
        return WaitingPlayer(stats=self.stats, deadline=self.deadline)


class ServerStatistics():
    '''Gather statistics about connection, disconnection and game finding timespans.

    Not thread-safe and should be called from main thread.'''
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

        self.ongoing = 0

    def log(self):
        logger.info(f'Server statistics:\n'
            f'total connections:                        {self.n_connected}\n'
            f'average time between connections:         {self.avg_connected():.3f}\n'
            f'average disconnection time while waiting: {self.avg_disconnected():.3f}\n'
            f'disconnections while waiting:             '
            f'{(self.n_disconnected/self.n_connected*100):.1f}%\n'
            f'averate waiting time before game:         {self.avg_started():.3f}\n'
            f'games currently running:                  {self.ongoing}')
        
    def reg_connect(self, timestamp):
        self.n_connected += 1
        self.connected_at[self.c_ind] = timestamp - self.last_connection
        self.last_connection = timestamp
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



class HandshakeThread(threading.Thread):
    '''Accept and check handshake'''
    def __init__(self, conn: socket.socket, accept):
        threading.Thread.__init__(self)
        self.conn = conn
        self.accept = accept
        self.player = None
        self.timestamp = None
        self.running = True

    def run(self):
        self.player = self.handshake()
        self.timestamp = time.time()
        self.running = False

    def handshake(self) -> Optional[ConnectedPlayer]:
        '''Handshake phase. Exchange messages and get player stats from DB.

        Return _waiting player if protocol was satisfied, None otherwise.'''
        conn = NetworkConnection(self.conn)
        r = conn.receive(time.time() + HANDSHAKE_TIMELIMIT)
        if isinstance(r, Timeout) or isinstance(r, Dead):
            conn.close()
            return None
        try:
            token = parse_handshake_response(r)
        except InvalidResponseError as e:
            conn.kick(e.msg)
            return None
        if not self.accept:
            conn.kick('Server is about to reboot. Try again later.')
            return None

        _conncount_lock.acquire()
        if _conns_by_token[token] >= CONNECTIONS_PER_TOKEN:
            _conncount_lock.release()
            conn.kick('connections limit exceeded')
            return None

        _conns_by_token[token] += 1
        _conncount_lock.release()
        player = self.db_load_player_by_token(token)
        if player is None:
            conn.kick('unknown token')
            return None
        conn.send({'you': player.name})
        return ConnectedPlayer(token=token, 
                               stats=player, 
                               conn=conn, 
                               deadline=time.time() + 55)


    def db_load_player_by_token(self, token) -> Optional[PlayerStats]:
        dbconn = connect_to_db()
        with dbconn.cursor() as cursor:
            cursor.execute('''SELECT id, name, rating_mu, rating_sigma FROM 
                              icfpc2017_players WHERE token=%s;''', (token,))
            if cursor.rowcount == 0:
                return None
            playerID, name, mu, sigma = cursor.fetchone()
            cursor.execute('''SELECT * FROM icfpc2017_participation 
                              WHERE player_id=%s''', (playerID, ))
            games = cursor.rowcount
            return PlayerStats(ID=playerID, name=name, games=games, mu=mu, sigma=sigma)



class GameThread(threading.Thread):
    '''Start gameloop and return result.'''
    def __init__(self, ID: int, 
                       players: List[ConnectedPlayer], 
                       m: Map, 
                       mapname: str,
                       settings: Settings):
        threading.Thread.__init__(self)
        self.ID = ID
        self.players = players
        self.m = m
        self.mapname = mapname
        self.settings = settings

        self.replay = None
        self.scores = None
        self.running = False
        self.timefinish = None

    def run(self):
        connections = [p.conn for p in self.players]
        names = [p.stats.name for p in self.players]
        logger.info(f'Game {self.ID} started with ' + str(names))
        self.replay, self.scores = gameloop(self.m, self.mapname, self.settings, connections, names)
        logger.info(f'Game {self.ID} finished.')
        self.timefinish = datetime.fromtimestamp(time.time())
        self.running = False


#--------------------------- SERVER LOOP -------------------------------#

_waiting = []
_handshaking = []
_ongoing = []
_conns_by_token = defaultdict(int)
_stats = ServerStatistics()

_conncount_lock = threading.Lock()


def connectserver(port, timeout):
    '''Initial server connection. Return server socket.'''
    address = '', port
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind(address)
    server.settimeout(timeout)
    server.listen(10)
    return server


def _close_pending_games():
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''UPDATE icfpc2017_games SET status='aborted'
                          WHERE status='ongoing';''')
    dbconn.commit()
    dbconn.close()


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


def _accept_new_connection(server, accept):
    try:
        conn, addr = server.accept()
    except socket.timeout:
        logger.debug('timed out')
        return
    h = HandshakeThread(conn, accept)
    h.start()
    _handshaking.append(h)


def _add_new_players():
    h_running = []
    times = []
    for h in _handshaking:
        if h.running:
            h_running.append(h)
            continue

        h.join()
        if h.player is None:
            continue
        _waiting.append(h.player)
        times.append(h.timestamp)

    for t in sorted(times):
        _stats.reg_connect(t)
    _handshaking[:] = h_running


def _check_disconnected():
    for p in _waiting:
        p.conn.recheck()
        if not p.conn.alive: 
            _stats.reg_disconnect(p.deadline)
            _remove_from_conns(p.token)
    _waiting[:] = list(compress(_waiting, (p.conn.alive for p in _waiting)))


def _remove_from_conns(token):
    # Player disconnected; if no connections are open, delete from dictionary
    _conncount_lock.acquire()
    _conns_by_token[token] -= 1
    if _conns_by_token[token] <= 0:
        del _conns_by_token[token]
    _conncount_lock.release()


def _call_match_maker():
    match = makematch([p.conninfo() for p in _waiting], _stats.connection_rate())
    if match is None: 
        return
    players = list(compress(_waiting, match.participants))

    dbconn = connect_to_db()
    gameID = db_submit_game_started(dbconn, match.mapname, match.settings)
    dbconn.commit()
    dbconn.close()

    game = GameThread(gameID, players, match.map, match.mapname, match.settings)
    for p in players:
        _stats.reg_start(p.deadline)
    game.running = True
    game.start()
    _ongoing.append(game)
    _waiting[:] = list(compress(_waiting, (not x for x in match.participants)))


def db_submit_game_started(conn, mapname: str, s: Settings):
    with conn.cursor() as cursor:
        cursor.execute('''INSERT INTO icfpc2017_games(mapname, futures, 
                          options, splurges, timestart, status) 
                          VALUES (%s, %s, %s, %s, %s, %s) RETURNING id;''',
                          (mapname, s.futures, s.options, s.splurges, 
                          datetime.fromtimestamp(time.time()), 'ongoing'))
        gameID = cursor.fetchone()[0]
    return gameID


def _resolve_finished_games():
    games_running = []
    dbconn = None
    for g in _ongoing:
        if g.running:
            games_running.append(g)
            continue
        if dbconn is None:
            dbconn = connect_to_db()
        logger.debug(f'Resolving game {g.ID}')
        g.join()
        db_sumbit_game_finished(dbconn, g.ID, g.timefinish, g.replay)
        db_submit_players_scores(dbconn, g.ID, g.players, g.scores)

        for p in g.players:
            _remove_from_conns(p.token)
        revised = revise_players([p.stats for p in g.players], g.scores)
        db_submit_players_rating(dbconn, revised)

    if dbconn is not None:
        dbconn.commit()
        dbconn.close()
    _ongoing[:] = games_running


def db_sumbit_game_finished(conn, gameID, timefinish, replay):
    with conn.cursor() as cursor:
        cursor.execute('UPDATE icfpc2017_games SET status=%s, timefinish=%s WHERE id=%s;', 
                                ('finished', timefinish, gameID))
        cursor.execute('INSERT INTO icfpc2017_replays(id, replay) VALUES(%s, %s);', 
                                (gameID, json.dumps(replay)))


def db_submit_players_scores(conn, gameID, players, scores):
    assert len(players) == len(scores)
    with conn.cursor() as cursor:
        for i in range(len(players)):
            cursor.execute('''INSERT INTO icfpc2017_participation(game_id, 
                              player_id, player_order, score) 
                              VALUES (%s, %s, %s, %s);''',
                              (gameID, players[i].stats.ID, i, scores[i]))


def db_submit_players_rating(conn, players: List[PlayerStats]):
    with conn.cursor() as cursor:
        for p in players:
            cursor.execute('''UPDATE icfpc2017_players 
                              SET rating_mu=%s, rating_sigma=%s 
                              WHERE name=%s;''',
                              (p.mu, p.sigma, p.name))


def serverloop():
    '''Main loop. Accept connections, gather players for match and start games.'''
    server = connectserver(config.GAME_SERVER_PORT, timeout=5)
    aux_server = connectserver(config.GAME_SERVER_COMMANDS_PORT, timeout=.5)
    logger.info('Server started successfully')
    _close_pending_games()
    accept_players = True

    while accept_players or _ongoing:
        # get technical commands
        command = _get_command(aux_server)
        if command == SC_STOP:
            accept_players = False

        # if there is new connection request, accept it
        _accept_new_connection(server, accept_players)

        # add players that finished handshake
        _add_new_players()

        # if smb's already disconnected, remove them
        _check_disconnected()

        # make match and start a new game
        _call_match_maker()

        # if any game ended, clean up
        _resolve_finished_games()

        _stats.ongoing = len(_ongoing)


#--------------------------- DUAL LOGGING ------------------------------#

def setup_dual_logging():
    logging.getLogger().setLevel(logging.DEBUG)
    formatter = logging.Formatter('%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')

    handler = logging.FileHandler('server_debug.log')  # filepath for logging!
    handler.setLevel(logging.DEBUG)
    handler.setFormatter(formatter)
    logging.getLogger().addHandler(handler)

#    handler = logging.FileHandler('server_info.log')   # filepath for logging!
    handler = logging.StreamHandler(sys.stdout)
    handler.setLevel(logging.INFO)
    handler.setFormatter(formatter)
    logging.getLogger().addHandler(handler)

    # handler = logging.SMPTHandler(???)
    # handler.setLevel(logging.INFO)
    # handler.setFormatter(formatter)
    # logging.getLogger().addHandler(handler)


if __name__ == '__main__':
    setup_dual_logging()
    try:
        serverloop()
    except Error as e:
        logger.exception(e)
        raise e
