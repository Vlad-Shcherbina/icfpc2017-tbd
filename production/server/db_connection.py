from typing import NamedTuple, List, Optional
from datetime import datetime
import psycopg2
import json

from production.bot_interface import Settings
from production.server.server_interface import PlayerStats

import logging;
logger = logging.getLogger(__name__)


def connect_to_db():
    logger.debug('Connecting to database')
    conn = psycopg2.connect(
            dbname='practice',
            host='127.0.0.1',
            port='5432',
            user='postgres', password='sql9813')
    return conn


#----------------------- CREATE DEV DATABASE ---------------------------#

def local_create_tables(conn):
    with conn.cursor() as cursor:
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS icfpc2017_games(
                id              serial primary key,
                mapname         text not null,
                futures         boolean,
                options         boolean,
                splurges        boolean,
                status          text not null,
                timestart       timestamp not null default CURRENT_TIMESTAMP,
                timefinish      timestamp
                );

            CREATE TABLE IF NOT EXISTS icfpc2017_players(
                id              serial primary key,
                token           varchar(32) not null unique,
                name            text not null unique,
                rating_mu       double precision not null,
                rating_sigma    double precision not null,
                contact         text not null
                );

            CREATE TABLE IF NOT EXISTS icfpc2017_participation(
                id              serial primary key,
                game_id         integer REFERENCES icfpc2017_games (id),
                player_id       integer  REFERENCES icfpc2017_players (id),
                player_order    smallint,
                score           integer
                );

            CREATE TABLE IF NOT EXISTS icfpc2017_replays(
                id              integer REFERENCES icfpc2017_games (id),
                replay          bytea
                );''')


def local_add_players(conn):
    with conn.cursor() as cursor:
        cursor.execute('''
            INSERT INTO icfpc2017_players(token, name, rating_mu, rating_sigma, contact)
            VALUES 
            ('5ccdcda942c53fa5edff6b9a49eff231', 
                'zzz_julie', 50, 16.6666666666667, 'j@mail.com'),
            ('299c78a33d868f859e0536493219556a', 
                'zzz_meee', 43, 4.1, 'me@hehe.he'),
            ('36f94b07397d25040528808da4fcb4db', 
                'zzz_yahoo', 21, 3.33, 'ya@hoo.oo'),
            ('d4d15906a8ad1f9b16ab727b38b3f39f', 
                'zzz_smb', 65, 1.99, 'smb@smwhr.hz'),
            ('e8b2cb9ac3844a4eb85bfca5d729c37e', 
                'zzz_nevermore', 81, 1.15, 'a@b.c')
            ''')


if __name__ == '__main__':
    conn = connect_to_db()
    local_create_tables(conn)
    local_add_players(conn)
    conn.commit()
    conn.close()