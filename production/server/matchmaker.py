from random import random, randrange
from typing import NamedTuple, List, Optional, Dict
from math import exp, factorial
from collections import defaultdict
import trueskill
import json
import os
import time

from production.utils import project_root
from production.bot_interface import Map, Settings
from production.json_format import parse_map

import logging
logger = logging.getLogger(__name__)


MAX_PLAYERS = 3 #16
WAITING_THRESHOLD = 0.95


class PlayerStats(NamedTuple):
    name: str
    games: int
    mu: float
    sigma: float

    large: float
    medium: float
    small: float
    futures: float
    options: float
    splurges: float
    opponents: Dict[str, float]


class ConnInfo(NamedTuple):
    stats: PlayerStats
    deadline: float


class MatchInfo(NamedTuple):
    participants: List[bool]
    map: Map
    settings: Settings


#--------------------------- MAKE MATCH --------------------------------#

def random_map():
    '''Return random loaded map and random settings.'''
    mapdir = project_root() / 'maps' / 'official_map_samples'
    maps = os.listdir(mapdir)
    with open(mapdir / maps[randrange(len(maps))], 'r') as mapfile:
        m = parse_map(json.load(mapfile))

    setts = Settings(options = True if random() > 0.5 else False,
                     splurges = True if random() > 0.5 else False,
                     futures = True if random() > 0.5 else False,
                     raw_settings = '')

    return (m, setts)


def poissonflow(rate, t, N):
    '''Given rate (number of connections per second), return probability that
    at least N connections will be made in next t seconds.
    '''
    L = rate * t
    S = P = 1
    for i in range(1, N + 1):
        P *= (L / i)
        S += P
    return S * exp(-rate * t)


def _player_priority(player: ConnInfo, conncount: int) -> float:
    return max(0, player.deadline - time.time()) * player.stats.games * conncount

def players_priority(players: List[ConnInfo]):
    '''Return list of pairs (priority, index) sorted by priority.

    Priority is an arbitrary number, less is better, 0 if deadline has passed.
    Index is player's index number in the initial list.
    '''
    conncounts = defaultdict(list)
    for i, p_info in enumerate(players):
        conncounts[p_info.stats.name].append(i)
    
    priorities = []
    for name in conncounts:
        assert conncounts[name], name    # no empty lists!
        r = _player_priority(players[conncounts[name][0]], len(conncounts[name]))
        for i in conncounts[name]:
            priorities.append((r, i))
    priorities.sort(key=lambda x: x[0])
    return priorities


def match(players: List[ConnInfo], conn_rate) -> Optional[MatchInfo]:
    if len(players) < MAX_PLAYERS:
        logger.debug('Not enought players')
        return None

    # any reason to wait longer?
    if (len(players) < MAX_PLAYERS * 2
        and players[0].deadline > time.time()
        and poissonflow(conn_rate, 
                        (players[0].deadline - time.time()),
                        (MAX_PLAYERS * 2 - len(players))
                        ) > WAITING_THRESHOLD):
        logger.debug('Waiting for a better player')
        return None

    m, settings = random_map()
    priorities = players_priority(players)
    participants = [False] * len(players)
    N = min(len(m.mines), MAX_PLAYERS)
    for r, i in priorities[:N]:
        participants[i] = True

    logger.debug('Making new match')
    return MatchInfo(participants=participants, map=m, settings=settings)


#------------------------- GET NEW RATINGS -----------------------------#

# Temp! will receive and return data in another format 
# (not binded to trueskill module)
def rate(players, scores):
    env = trueskill.TrueSkill(mu    = 50.0, 
                              sigma = 50.0/3, 
                              beta  = 50.0/6, 
                              tau   = 50.0/3/100, 
                              draw_probability = 0.05)
    rates = [[trueskill.Rating(p.stats.mu, p.stats.sigma)] for p in players]

    d = { s : i for i, s in enumerate(sorted(scores, reverse=True)) }
    places = [d[s] for s in scores]

    new_rates = env.rate(rates, places)
    return [r[0] for r in new_rates]
    #leaderboard = sorted(ratings, key=env.expose, reverse=True)


#-----------------------------------------------------------------------#

if __name__ == '__main__':
    def rating_by_player(token: str):
        return PlayerStats(name=token,
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

    p1 = ConnInfo(rating_by_player('julie'), 1)
    p2 = ConnInfo(rating_by_player('me'), 2)
    print(p1.stats.name, p1.stats.mu, p1.stats.sigma)
    print(p2.stats.name, p2.stats.mu, p2.stats.sigma)
    print(rate([p1, p2], [50, -10]))
