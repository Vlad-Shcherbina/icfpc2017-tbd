from random import random, randrange, choice
from typing import NamedTuple, List, Optional, Dict, Tuple
from math import exp, factorial, sqrt, erf
from collections import defaultdict
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
LARGE = 1000
SMALL = 100


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
    if sum(1 for p in players if p.waiting) < MAX_PLAYERS:
        logger.debug('Not enought players')
        return True

    first_deadline = min(p.first_deadline() for p in players)
    first_finish = min(p.first_finish() for p in players)
    if (len(players) < MAX_PLAYERS * 2
                and first_deadline > time()
                and first_finish < first_deadline):
        logger.debug('Waiting for a better player')
        return True

    return False


def playersSet(players: List[Player], n) -> List[str]:
    subset = list(range(len(players)))
    assigned = []
    for i in range(n):
        j = randrange(i, n)
        assigned.append(players[subset[j]].stats.token)
        subset[i], subset[j] = subset[j], subset[i]
    return assigned


def makematch(players: List[Player]) -> Optional[MatchInfo]:
    if postpone_match_making(players):
        return None

    m, mapname, playernum, settings = random_map()
    playernum = min(MAX_PLAYERS, playernum)    # TEMP!!!
    participants = playersSet(players, playernum)

    logger.debug('Making new match')
    return MatchInfo(participants=participants,
                    map=m,
                    mapname=mapname,
                    settings=settings)


#------------------------- GET NEW RATINGS -----------------------------#

def revise_ratings(players: List[PlayerStats], scores):
    # returns list of pairs (mu, sigma) in the corresponding order.
    env = trueskill.TrueSkill(mu    = 50.0,
                              sigma = 50.0/3,
                              beta  = 50.0/6,
                              tau   = 50.0/3/100,
                              draw_probability = 0.05)    # <-- or what?
    rates = [[trueskill.Rating(p.mu, p.sigma)] for p in players]

    d = { s : i for i, s in enumerate(sorted(scores, reverse=True)) }
    # Luckily this means [10, 10, 0] <-- first two players share 2nd place.
    places = [d[s] for s in scores]

    new_rates = env.rate(rates, places)
    return [(r[0].mu, r[0].sigma) for r in new_rates]

    #leaderboard = sorted(ratings, key=env.expose, reverse=True)


def revise_players(players: List[PlayerStats], scores):
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