from typing import NamedTuple, List
from datetime import datetime
import psycopg2
import json

import logging;
logger = logging.getLogger(__name__)

class _ReplayProxy:
    '''Lazily initialized replay fetching, using explicit get() method to make the magic conspicuous'''
    def __init__(self, id):
        self.id = id
        self._replay = None

    def get(self):
        if self._replay is None:
            self._replay = _fetch_replay(self.id)
        return self._replay


class GameRecord(NamedTuple):
    id              : int
    time            : datetime
    mapname         : str
    extensions      : List[str]
    player_names    : List[str]
    num_players     : int
    scores          : List[int]
    timeouts        : int
    replay          : _ReplayProxy


class PlayerRecord(NamedTuple):
    id              : str
    botname         : str
    rating          : float


def get_game_by_id(gameID: int):
    conn = _connect()
    with conn.cursor() as cursor:
        cursor.execute('select replay from icfpc_games where id=%s' % gameID)
        return json.loads(cursor.fetchone(), encoding='utf-8')
    conn.close()


def get_games_by_range(last=100, page=0):
    conn = _connect()

    conn.close()    
    pass


def get_games_by_player(playerID: str, last=100, page=0):
    conn = _connect()

    conn.close()    
    pass


def get_player_rating(playerID: str):
    conn = _connect()

    conn.close()    
    pass


def submit_game():
    conn = _connect()

    conn.close()    
    pass


def submit_player_rating():
    conn = _connect()

    conn.close()    
    pass


def _connect():
    log.info('Connecting to replay database')
    conn = psycopg2.connect(
            dbname='practice',
            host='127.0.0.1',
            port='5432',
            user='postgres', password='sql9813')
    return conn


