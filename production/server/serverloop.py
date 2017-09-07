from production.server import config
if config.USE_HINTCHECK and __name__ == '__main__':
    import hintcheck
    hintcheck.monkey_patch_named_tuple_constructors()

from typing import NamedTuple, List, Optional
from itertools import compress, chain
from collections import defaultdict
from datetime import datetime
from time import time, sleep
import socket
import threading
import sys
import json
import zlib
import signal
from random import randrange, random

from production.server.connection import NetworkConnection, Timeout, Dead
from production.server.gameloop import gameloop
from production.bot_interface import Settings, Map
from production.server.server_interface import *
from production.json_format import parse_handshake_response, InvalidResponseError
from production.server.matchmaker import makematch, revise_players
from production.server.db_connection import connect_to_db

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
PLAYER_WAITING_TIME = 60
SERVER_TICK_TIME = 5

#------------------------ AUXILIARY CLASSES ----------------------------#

class ServerStatistics():
    '''Gather statistics about connection, disconnection and game finding timespans.

    Not thread-safe and should be called from main thread.'''
    def __init__(self):
        self.STATRANGE = 100   # collect data only about that much last events.
        self.last_connection = time()

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
        self.disconnected_at[self.d_ind] = time() - deadline + 60
        self.d_ind = (self.d_ind + 1) % self.STATRANGE

    def reg_start(self, deadline):
        self.started_at[self.s_ind] = time() - deadline + 60
        self.s_ind = (self.s_ind + 1) % self.STATRANGE

    def connection_rate(self):
        N = self.STATRANGE if self.connected_at[-1] is not None else self.c_ind
        return (N + 1) / (sum(self.connected_at[:N]) + (time() - self.last_connection))

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
    def __init__(self, conn: socket.socket):
        threading.Thread.__init__(self)
        self.conn = conn
        self.playerstats = None
        self.timestamp = None

    def run(self):
        self.newconn = self.handshake()
        self.timestamp = time()

    def handshake(self):
        '''Handshake phase. Exchange messages and get player stats from DB.'''
        self.conn = NetworkConnection(self.conn)
        r = self.conn.receive(time() + HANDSHAKE_TIMELIMIT)
        if isinstance(r, Timeout) or isinstance(r, Dead):
            self.conn.close()
            return
        try:
            token = parse_handshake_response(r)
        except InvalidResponseError as e:
            self.conn.kick(e.msg)
            return

        if _shuttingdown:
            self.conn.kick('Server is about to reboot. Try again later.')
            return

        with _players_lock:
            if token in _players and _players[token].count() >= CONNECTIONS_PER_TOKEN:
                self.conn.kick('connections limit exceeded')
                return
            player = _players.get(token, None)

        if player is not None:
            stats = player.stats
        else:
            stats = self.db_load_player_by_token(token)
            if stats is None:
                self.conn.kick('unknown token')
                return

        self.playerstats = stats
        self.conn.send({'you': stats.name})

    def db_load_player_by_token(self, token) -> Optional[PlayerStats]:
        dbconn = connect_to_db()
        with dbconn.cursor() as cursor:
            cursor.execute('''SELECT id, name, rating_mu, rating_sigma FROM 
                              players WHERE token=%s;''', (token,))
            if cursor.rowcount == 0:
                return None
            playerID, name, mu, sigma = cursor.fetchone()
            cursor.execute('''SELECT * FROM participation
                              WHERE player_id=%s''', (playerID, ))
            games = cursor.rowcount
            return PlayerStats(ID=playerID, name=name, token=token, games=games, mu=mu, sigma=sigma)


class GameThread(threading.Thread):
    '''Start gameloop and return result.'''
    def __init__(self, ID: int,
                       conns: List[NetworkConnection],
                       names: List[str],
                       tokens: List[str],
                       m: Map,
                       mapname: str,
                       settings: Settings,
                       time_estimation: Estimation):
        threading.Thread.__init__(self)
        self.ID = ID
        self.conns = conns
        self.names = names
        self.tokens = tokens
        self.m = m
        self.mapname = mapname
        self.settings = settings
        self.estimation = time_estimation

        self.replay = None
        self.scores = None
        self.forcedmoves = None
        self.timefinish = None

    def run(self):
        logger.info(f'Game {self.ID} started with ' + str(self.names))
        try:
            result = gameloop(self.m,
                              self.mapname,
                              self.settings,
                              self.conns,
                              self.names,
                              self.estimation,
                              _shutdownthreads)
        except ServerInterrupt:
            logger.warning(f'Game {self.ID} stopped preliminary.')
            return
        self.replay, self.scores, self.forcedmoves = result
        logger.info(f'Game {self.ID} finished.')
        self.timefinish = datetime.fromtimestamp(time())


#--------------------------- SERVER LOOP -------------------------------#

_players = {}       # token : Player
_waiting_conns_by_token = defaultdict(list)
_handshakers = []
_ongoing = []
_statistics = ServerStatistics()

_players_lock = threading.Lock()

_shuttingdown = False
_shutdownthreads = threading.Event()


def sigint_handler(signum, frame):
    '''Catch Ctrl+C signal.

    Shut down gracefully after first pressing: stop accepting players, wait for
    ongoing games to finish;
    force threads to break mainloops after second;
    exit without further waiting (with threads stil running) after third.
    '''
    global _shuttingdown
    if not _shuttingdown:
        logger.warning('Server shuts down gracefully.')
        _shuttingdown = True
        with _players_lock:
            for c in chain(_waiting_conns_by_token.values()):
                c[0].kick('Server is about to reboot. Try again later.')
                c[0].close()
        for h in _handshakers:
            h.join()
    elif not _shutdownthreads.is_set():
        logger.warning('Server shuts down forcefully.')
        _shutdownthreads.set()
        # join game- and handshake threads
    else:
        logger.warning('Server shuts down without stopping threads.')
        sys.exit(0)

signal.signal(signal.SIGINT, sigint_handler)


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
        cursor.execute('''UPDATE games SET status='aborted'
                          WHERE status='ongoing';''')
    dbconn.commit()
    dbconn.close()


def _accept_new_connections(server):
    while True:
        try:
            conn, addr = server.accept()
        except socket.timeout:
            return
        h = HandshakeThread(conn)
        h.start()
        _handshakers.append(h)


def _add_new_players():
    h_running = []
    times = []
    for h in _handshakers:
        #if h.running:
        if h.is_alive():
            h_running.append(h)
            continue

        h.join()
        if h.playerstats is None:
            continue
        token = h.playerstats.token
        with _players_lock:
            if not token in _players:
                _players[h.playerstats.token] = Player(h.playerstats)
            deadline = h.timestamp + PLAYER_WAITING_TIME
            _players[h.playerstats.token].waiting.append(deadline)
            _waiting_conns_by_token[token].append((h.conn, deadline))
        times.append(h.timestamp)

    for t in sorted(times):
        _statistics.reg_connect(t)
    _handshakers[:] = h_running


def _trim_dictionaries():
    trims = [token for token in _players.keys() if _players[token].count() == 0]
    for t in trims:
        assert not _waiting_conns_by_token[t]
        del _players[t]
        del _waiting_conns_by_token[t]


def _check_disconnected():
    with _players_lock:
        for token in list(_waiting_conns_by_token.keys()):
            for pair in _waiting_conns_by_token[token]:
                conn, deadline = pair
                conn.recheck()
                if not conn.alive:
                    _statistics.reg_disconnect(deadline)
                    _waiting_conns_by_token[token].remove(pair)
                    _players[token].waiting.remove(deadline)
        _trim_dictionaries()


def _call_match_maker():
    with _players_lock:
        playerslist = [p.copy() for p in _players.values()]
    m = makematch(playerslist)
    if m is None:
        return

    dbconn = connect_to_db()
    gameID = db_submit_game_started(dbconn, m.mapname, m.settings)
    dbconn.commit()
    dbconn.close()

    time_estimation = Estimation()
    conns = []
    names = []
    with _players_lock:
        for token in m.participants:
            conn, deadline = _waiting_conns_by_token[token].pop(0)
            _players[token].waiting.remove(deadline)
            _players[token].ongoing.append(time_estimation)
            names.append(_players[token].stats.name)
            conns.append(conn)
            _statistics.reg_start(deadline)
    _trim_dictionaries()
    game = GameThread(gameID, conns, names, m.participants, m.map, m.mapname, m.settings, time_estimation)
    game.start()
    _ongoing.append(game)


def db_submit_game_started(conn, mapname: str, s: Settings):
    with conn.cursor() as cursor:
        cursor.execute('''INSERT INTO games(mapname, futures,
                          options, splurges, timestart, status) 
                          VALUES (%s, %s, %s, %s, %s, %s) RETURNING id;''',
                          (mapname, s.futures, s.options, s.splurges, 
                          datetime.fromtimestamp(time()), 'ongoing'))
        gameID = cursor.fetchone()[0]
    return gameID


def _resolve_finished_games():
    games_running = []
    dbconn = None
    for g in _ongoing:
        if g.is_alive():
            games_running.append(g)
            continue
        elif g.replay is None:
            # game is aborted due to server stop. Abandon for now.
            continue
        if dbconn is None:
            dbconn = connect_to_db()
        logger.debug(f'Resolving game {g.ID}')
        g.join()

        with _players_lock:
            for token in g.tokens:
                _players[token].ongoing.remove(g.estimation)
            revised = revise_players([_players[token].stats for token in g.tokens], g.scores)
            additional = [{ 'mu_before' : _players[token].stats.mu,
                            'sigma_before' : _players[token].stats.sigma,
                            'mu_after' : r.mu,
                            'sigma_after' : r.sigma }
                          for token, r in zip(g.tokens, revised)]

        db_sumbit_game_finished(dbconn, g.ID, g.timefinish, g.replay)
        db_submit_players_scores(dbconn, g.ID, g.tokens, g.scores, g.forcedmoves, additional)
        db_submit_players_rating(dbconn, revised)

    if dbconn is not None:
        dbconn.commit()
        dbconn.close()
    _ongoing[:] = games_running


def db_sumbit_game_finished(conn, gameID, timefinish, replay):
    with conn.cursor() as cursor:
        cursor.execute('UPDATE games SET status=%s, timefinish=%s WHERE id=%s;',
                                ('finished', timefinish, gameID))
        cursor.execute('INSERT INTO replays(id, replay) VALUES(%s, %s);',
                                (gameID, zlib.compress(bytes(json.dumps(replay), 'utf-8'))))


def db_submit_players_scores(conn, gameID, tokens, scores, forcedmoves, additional):
    assert len(tokens) == len(scores)
    with conn.cursor() as cursor:
        for i in range(len(tokens)):
            cursor.execute('''INSERT INTO participation(game_id,
                              player_id, player_order, score, forcedmoves, additional) 
                              VALUES (%s, %s, %s, %s, %s, %s);''',
                              (gameID, _players[tokens[i]].stats.ID, i, scores[i],
                               forcedmoves[i], json.dumps(additional[i])))


def db_submit_players_rating(conn, players: List[PlayerStats]):
    with conn.cursor() as cursor:
        for p in players:
            cursor.execute('''UPDATE players
                              SET rating_mu=%s, rating_sigma=%s 
                              WHERE name=%s;''',
                              (p.mu, p.sigma, p.name))


def serverloop():
    '''Main loop. Accept connections, gather players for match and start games.'''
    server = connectserver(config.GAME_SERVER_PORT, timeout=0.1)
    logger.info('Server started successfully')
    _close_pending_games()

    while not _shuttingdown or _ongoing:
        # if there is new connection request, accept it
        _accept_new_connections(server)
        sleep(SERVER_TICK_TIME)

        # add players that finished handshake
        _add_new_players()

        # if smb's already disconnected, remove them
        _check_disconnected()

        # make match and start a new game
        _call_match_maker()

        # if any game ended, clean up
        _resolve_finished_games()

        _statistics.ongoing = len(_ongoing)
    logger.warning('Server stopped work.')


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
    handler.setLevel(logging.DEBUG)      #INFO)
    handler.setFormatter(formatter)
    logging.getLogger().addHandler(handler)

    # handler = logging.SMPTHandler(???)
    # handler.setLevel(logging.INFO)
    # handler.setFormatter(formatter)
    # logging.getLogger().addHandler(handler)


if __name__ == '__main__':
    setup_dual_logging()
    if config.USE_HINTCHECK:
        hintcheck.hintcheck_all_functions()
    try:
        serverloop()
    except Exception as e:
        logger.exception(e)
        raise e
