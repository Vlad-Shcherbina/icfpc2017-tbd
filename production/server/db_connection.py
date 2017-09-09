from typing import NamedTuple, List, Optional
from datetime import datetime
import psycopg2
import json
import secrets
import zlib

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
                diff_x = max(s['x'] for s in m['sites']) - min(s['x'] for s in m['sites'])
                diff_y = max(s['y'] for s in m['sites']) - min(s['y'] for s in m['sites'])
                if diff_x < 100 or diff_y < 100:
                    coeff = 100 / min(diff_x, diff_y)
                else:
                    coeff = 1

                for site in m['sites']:
                    site['x'] = round(site['x'] * coeff, 1)
                    site['y'] = round(site['y'] * coeff, 1)
                max_players = max(2, min(16, len(m['mines'])))
                m = zlib.compress(bytes(json.dumps(m), 'utf-8'))
                cursor.execute('''INSERT INTO maps(mapname, maptext, max_players)
                                  VALUES (%s, %s, %s);''', (mapname, m, max_players))


def add_player(name, contact):
    token = secrets.token_urlsafe(10)
    with conn.cursor() as cursor:
        cursor.execute('''
            INSERT INTO players(token, name, rating_mu, rating_sigma, contact)
            VALUES (%s, %s, %s, %s, %s)''',
            (token, name, 50, 50/3, contact))


def local_add_players(conn):
    add_player('zzz_julie', 'j@mail.com')
    add_player('zzz_meee', 'me@hehe.he')
    add_player('zzz_yahoo', 'ya@hoo.oo')
    add_player('zzz_smb', 'smb@smwhr.hz')
    add_player('zzz_nevermore', 'a@b.c')
    add_player('zzz_wannacry', 'mali@cio.us')


if __name__ == '__main__':
    conn = connect_to_db()
    local_create_tables(conn)
    local_add_players(conn)
    upload_maps_from_folder(conn)
    conn.commit()
    conn.close()
