from random import random
from typing import NamedTuple, List, Optional
import json

from production.utils import project_root
from production.bot_interface import Map, Settings
from production.json_format import parse_map

import logging
logger = logging.getLogger(__name__)

class PlayerRating(NamedTuple):
    token: str
    rating: str         # TEMP!
    urgent: bool


class Match(NamedTuple):
    tokens: List[str]
    map: Map
    settings: Settings


def make_match(players: List[PlayerRating]) -> Optional[Match]:
#    logger.debug('making match...')

    if len(players) < 2:
#        logger.debug('no match found')
        return None
    if random() < 0.6:
        logger.debug('no match found')
        return None
    with open(project_root() / 'maps' / 'official_map_samples' / 'sample.json', 'r') as f:
        m = parse_map(json.load(f))
#    logger.debug('match found')
    return Match(tokens=[p.token for p in players[:2]], 
                 map=m, 
                 settings=Settings(futures=True, options=True, splurges=True, raw_settings=''))