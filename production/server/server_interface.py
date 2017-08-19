from typing import NamedTuple, List
from datetime import datetime

from production.bot_interface import Map, Settings



class PlayerStats(NamedTuple):
    name: str
    games: int
    mu: float
    sigma: float

    # large: int
    # medium: int
    # small: int
    # futures: int
    # options: int
    # splurges: int
    # opponents: Dict[str, int]


class GameStats(NamedTuple):
    mapname: str
    futures: bool
    options: bool
    splurges: bool
    participants: List[str]
    scores: List[int]
    timestamp: int  # TODO: datetime?


class ConnInfo(NamedTuple):
    stats: PlayerStats
    deadline: float


class MatchInfo(NamedTuple):
    participants: List[bool]
    map: Map
    settings: Settings
