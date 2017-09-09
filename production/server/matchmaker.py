from random import random, randrange, choice
from typing import NamedTuple, List, Optional, Dict, Tuple
from math import exp, factorial, sqrt, erf
from collections import defaultdict
from datetime import datetime, timedelta
import trueskill
import json
import os
import time
import zlib

from production.utils import project_root
from production.bot_interface import Map, Settings
from production.server.server_interface import *
from production.json_format import parse_map
from production.server.db_connection import connect_to_db

import logging
logger = logging.getLogger(__name__)


MAX_PLAYERS = 3 #16
WAITING_THRESHOLD = 0.95
LAST_GAMES_SPAN = 60 * 60 * 2
TRY_TO_MAKE_NEW_SET = 10
LARGE = 1000
SMALL = 100

MIN_SIGMA_ADD = 0.1
MAX_SIGMA_ADD = 1
MIN_ABSENCE_TIME = 60 * 60 * 5
MAX_ABSENCE_TIME = 60 * 60 * 24 * 2


#--------------------------- MAKE MATCH --------------------------------#

def random_map() -> Tuple[Map, str, int, Settings]:
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT mapname FROM maps;''')
        mapnames = [m[0] for m in cursor.fetchall()]
        mapname = choice(mapnames)
        cursor.execute('''SELECT maptext, max_players FROM maps WHERE mapname=%s;''', 
                       (mapname,))
        text, playernum = cursor.fetchone()
        m = parse_map(json.loads(zlib.decompress(text)))
    dbconn.close()

    setts = Settings(options = True if random() > 0.5 else False,
                    splurges = True if random() > 0.5 else False,
                    futures = True if random() > 0.5 else False,
                    raw_settings = {})

    return (m, mapname, playernum, setts)


def win_probability(mu1, sigma1, mu2, sigma2):
    # never used
    mu = m2 - mu1
    s = sigma1 ** 2 + sigma2 ** 2
    return 0.5 * (1 + erf(mu / (2 * s)))


def postpone_match_making(players: List[Player]) -> bool:
    # any reason to wait longer?
    waiting_players = sum(1 for p in players if p.waiting)
    if waiting_players < 2:
        logger.debug('Not enought players')
        return True

    first_deadline = min(p.first_deadline() for p in players)
    first_finish = min(p.first_finish() for p in players)
    if (waiting_players < MAX_PLAYERS * 2
                and first_deadline > time()
                and first_finish < first_deadline):
        logger.debug('Waiting for new players')
        return True

    return False


def players_set(players: List[Player], n, lastgamesets) -> List[str]:
    available = list(p.stats.token for p in players if p.waiting)
    n = min(n, len(available))
    for _ in range(TRY_TO_MAKE_NEW_SET):
        assigned = []
        for i in range(n):
            j = randrange(i, n)
            assigned.append(available[j])
            available[i], available[j] = available[j], available[i]
        if set(assigned) not in lastgamesets:
            break
    return assigned


def makematch(players: List[Player]) -> Optional[MatchInfo]:
    if postpone_match_making(players):
        return None

    logger.debug('Making new match')
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT game_id, token FROM participation
                          INNER JOIN games ON participation.game_id = games.id
                          INNER JOIN players ON participation.player_id = players.id
                          WHERE timefinish > %s''',
                          (datetime.utcnow() - timedelta(LAST_GAMES_SPAN),))
        gamesets = defaultdict(set)
        for gameID, token in cursor.fetchall():
            gamesets[gameID].add(token)
    dbconn.close()
    m, mapname, playernum, settings = random_map()

    playernum = min(MAX_PLAYERS, playernum)    # TEMP!!!
    participants = players_set(players, playernum, list(gamesets.values()))

    return MatchInfo(participants=participants,
                    map=m,
                    mapname=mapname,
                    settings=settings)


#------------------------- GET NEW RATINGS -----------------------------#

def sigma_time_correction(sigma, lastgame):
    delta = time() - lastgame.timestamp()
    if delta < MIN_ABSENCE_TIME:
        return MIN_SIGMA_ADD
    if delta > MAX_ABSENCE_TIME:
        return MAX_SIGMA_ADD
    return (delta / (MAX_ABSENCE_TIME - MIN_ABSENCE_TIME)
            * (MAX_SIGMA_ADD - MIN_SIGMA_ADD) + MIN_SIGMA_ADD)


def revise_ratings(players: List[PlayerStats], scores):
    # returns list of pairs (mu, sigma) in the corresponding order.
    env = trueskill.TrueSkill(mu    = 50.0,
                              sigma = 50.0/3,
                              beta  = 50.0/6,
                              tau   = 50.0/3/100,
                              draw_probability = 0.05)    # <-- or what?
    rates = []
    dbconn = connect_to_db()
    for p in players:
        with dbconn.cursor() as cursor:
            mu, sigma = p.mu, p.sigma
            cursor.execute('''SELECT timefinish FROM games INNER JOIN participation
                              ON games.id = participation.game_id
                              WHERE player_id=%s
                              ORDER BY timefinish DESC;''', (p.ID, ))
            if cursor.rowcount > 0:
                lastgame = cursor.fetchone()[0]
                sigma += sigma_time_correction(p.sigma, lastgame)
            rates.append([trueskill.Rating(mu, sigma)])
    dbconn.close()

    d = { s : i for i, s in enumerate(sorted(scores, reverse=True)) }
    # Luckily this means [10, 10, 0] <-- first two players share 2nd place.
    places = [d[s] for s in scores]

    new_rates = env.rate(rates, places)
    return [(r[0].mu, r[0].sigma) for r in new_rates]

    #leaderboard = sorted(ratings, key=env.expose, reverse=True)


def revise_players(players: List[PlayerStats], scores) -> List[PlayerStats]:
    names = set(p.name for p in players)
    revised = []
    new_ratings = revise_ratings(players, scores)
    for i, p in enumerate(players):
        revised.append(PlayerStats(ID=p.ID,
                                   name = p.name,
                                   token = p.token,
                                   games = p.games + 1,
                                   mu = new_ratings[i][0],
                                   sigma = new_ratings[i][1],
                                   ))
    return revised

#-----------------------------------------------------------------------#

if __name__ == '__main__':
    pass