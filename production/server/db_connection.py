from typing import NamedTuple, List, Optional
from datetime import datetime
import psycopg2
import json

from production.bot_interface import Settings
from production.server.server_interface import PlayerStats
from production.server import config

import logging;
logger = logging.getLogger(__name__)


def connect_to_db():
    logger.debug('Connecting to database')

    conn = psycopg2.connect(
        dbname=config.DB_NAME,
        host=config.DB_ADDRESS,
        port=config.DB_PORT,
        user=config.DB_USER,
        password=config.DB_PASSWORD)

    return conn


#----------------------- CREATE DEV DATABASE ---------------------------#

def local_create_tables(conn):
    with conn.cursor() as cursor:
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS maps(
                mapname         text primary key,
                maptext         bytea,
                max_players     smallint
                );

            CREATE TABLE IF NOT EXISTS games(
                id              serial primary key,
                mapname         text REFERENCES maps not null,
                futures         boolean,
                options         boolean,
                splurges        boolean,
                status          text not null,
                timestart       timestamp not null default CURRENT_TIMESTAMP,
                timefinish      timestamp
                );

            CREATE TABLE IF NOT EXISTS players(
                id              serial primary key,
                token           varchar(32) not null unique,
                name            text not null unique,
                rating_mu       double precision not null,
                rating_sigma    double precision not null,
                contact         text not null
                );

            CREATE TABLE IF NOT EXISTS participation(
                id              serial primary key,
                game_id         integer REFERENCES games,
                player_id       integer  REFERENCES players,
                player_order    smallint,
                score           integer,
                forcedmoves     boolean,
                additional      json
                );

            CREATE TABLE IF NOT EXISTS replays(
                id              integer REFERENCES games unique,
                replay          bytea
                );
        ''')


def upload_maps_from_folder(conn):
    from production.utils import project_root
    import os
    mapdir = project_root() / 'maps' / 'official_map_samples'
    with conn.cursor() as cursor:
        for mapname in os.listdir(mapdir):
            with open(mapdir / mapname, 'r') as mapfile:
                with open(mapdir / mapname, 'r') as mapfile:
                    m = json.load(mapfile)
                for site in m['sites']:
                    site['x'] = round(site['x'], 1)
                    site['y'] = round(site['y'], 1)
                max_players = max(2, min(16, len(m['mines'])))
                cursor.execute('''INSERT INTO maps(mapname, maptext, max_players)
                                  VALUES (%s, %s, %s)''', (mapname, json.dumps(m), max_players))
        

def local_add_players(conn):
    with conn.cursor() as cursor:
        cursor.execute('''
            INSERT INTO players(token, name, rating_mu, rating_sigma, contact)
            VALUES 
            ('5ccdcda942c53fa5edff6b9a49eff231', 
                'zzz_julie', 50, 16.6666666666667, 'j@mail.com'),
            ('299c78a33d868f859e0536493219556a', 
                'zzz_meee', 50, 16.6666666666667, 'me@hehe.he'),
            ('36f94b07397d25040528808da4fcb4db', 
                'zzz_yahoo', 50, 16.6666666666667, 'ya@hoo.oo'),
            ('d4d15906a8ad1f9b16ab727b38b3f39f', 
                'zzz_smb', 50, 16.6666666666667, 'smb@smwhr.hz'),
            ('e8b2cb9ac3844a4eb85bfca5d729c37e', 
                'zzz_nevermore', 50, 16.6666666666667, 'a@b.c')
            ''')


if __name__ == '__main__':
    conn = connect_to_db()
    local_create_tables(conn)
    local_add_players(conn)
    upload_maps_from_folder(conn)
    conn.commit()
    conn.close()
