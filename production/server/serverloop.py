from typing import NamedTuple, List, Optional
import socket
import time
import threading
import sys
from random import randrange

from production.server.connection import NetworkConnection, Timeout, Dead
from production.server.matchmaker import make_match, PlayerRating
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


HANDSHAKE_LIMIT = 0.5


class WaitingPlayer(NamedTuple):
    token: str
    name: str
    rating: str             # TEMP!!!
    conn: NetworkConnection
    deadline: int

    def __lt__(self, other): return self.deadline < other.deadline
    def __le__(self, other): return self.deadline <= other.deadline
    def __gt__(self, other): return self.deadline > other.deadline
    def __ge__(self, other): return self.deadline >= other.deadline

    def short(self):
        '''Shorter version for matchmaker.'''
        return PlayerRating(token=self.token, 
                            rating=self.rating, 
                            urgent=(self.deadline < time.time()))


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
        logger.info(f'Game {self.ID} started. with ' + str(names))
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


def handshake(conn: socket.socket) -> Optional[WaitingPlayer]:
    '''Handshake phase. Exchange messages and get player stats from DB.

    Return waiting player if protocol was satisfied, None otherwise.'''
    conn = NetworkConnection(conn)
    r = conn.receive(time.time() + HANDSHAKE_LIMIT)
    if isinstance(r, Timeout) or isinstance(r, Dead):
        conn.close()
        return None
    if 'me' not in r:
        conn.zombify('bad handshake request')
        conn.close()
        return None

    token = str(r['me'])
    name = token                # temp!
    # get name and rating from DB
    conn.send({'you': name})
    return WaitingPlayer(token, name, '', conn, time.time() + 60)


def serverloop():
    '''Main loop. Accept connections, gather players for match and start games.'''
    server = connectserver()
    logger.info('Server started successfully')
    waiting_list = []
    games = []
    while True:
        try:
            conn, addr = server.accept()
        except socket.timeout:
            logger.debug('timed out')
        else:
            player = handshake(conn)
            if player: 
                waiting_list.append(player)

        match = make_match([p.short() for p in waiting_list])
        if match is None: 
            continue
        players = [p for p in waiting_list if p.token in match.tokens]
        waiting_list = [p for p in waiting_list if not p.token in match.tokens]
        game = GameThread(randrange(100), players, match.map, match.settings)
        game.start()
        games.append(game)


if __name__ == '__main__':
    logging.basicConfig(
            level=logging.DEBUG,
            stream=sys.stdout,
            format='%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')
    logging.getLogger('production.server.connection').setLevel(logging.INFO)
    serverloop()
