from random import random, randrange
from typing import NamedTuple, List, Optional, Dict
from math import exp, factorial
import json
import os
import time

from production.utils import project_root
from production.bot_interface import Map, Settings
from production.json_format import parse_map

import logging
logger = logging.getLogger(__name__)


MAX_PLAYERS = 3 #16
P_THRESHOLD = 0.5


class Rating(NamedTuple):
    games: int
    rating: float
    large: float
    medium: float
    small: float
    futures: float
    options: float
    splurges: float
    opponents: Dict[str, float]


class PlayerRating(NamedTuple):
    name: str
    rating: Rating
    deadline: float


class Match(NamedTuple):
    participants: List[bool]
    map: Map
    settings: Settings


def probability(rate, t, N):
    '''Given rate (number of connections per second), return probability that
    at least N connections will be made in next t seconds.
    '''
    return sum(((rate*t) ** i) * exp(-rate*t) / factorial(i) for i in range(N))


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


def one_player_priority(player: PlayerRating, conncount: int) -> float:
    return max(0, deadline - time.time()) * player.games * conncount

def players_priority(players: List[PlayerRating]):
    conncounts = {}
    for i, p in enumerate(player):
        if p not in conncounts:
            conncounts[p.name] = []
        conncounts[p.name].append(i)
    priorities = []
    for name in conncounts:
        r = one_player_priority(players[conncounts[name][0]], len(conncounts[name]))
        for i in conncounts[name]:
            priorities.append((r, i))
    priorities.sort(key=lambda x: x[0])
    return priorities


def make_match(players: List[PlayerRating], conn_rate) -> Optional[Match]:
    m, settings = random_map()
    if len(players) < min(len(m.mines), MAX_PLAYERS):
        return None

    if (len(players) < len(m.mines) + MAX_PLAYERS 
        and players[0].deadline > time.time()
        and probability(conn_rate, 
                        players[0].deadline - time.time(),
                        len(m.mines) + MAX_PLAYERS - len(players)) > P_THRESHOLD):
        pass



    return Match(participants = [True] * len(m.mines) + [False] * (len(players) - len(m.mines)),
                 map = m,
                 settings = settings)


    if len(players) < 2:
#        logger.debug('no match found')

        return None
    if random() < 0.6:
        logger.debug('no match found')
        return None
    with open(project_root() / 'maps' / 'official_map_samples' / 'sample.json', 'r') as f:
        m = parse_map(json.load(f))
#    logger.debug('match found')
    return Match(participants=[True] * 2 + [False] * (len(players) - 2), 
                 map=m, 
                 settings=Settings(futures=True, options=True, splurges=True, raw_settings=''))


if __name__ == '__main__':
    random_map()
