from typing import NamedTuple, List
from datetime import datetime

from production.bot_interface import Map, Settings


class PlayerStats(NamedTuple):
    ID: int
    name: str
    games: int
    mu: float
    sigma: float


class GameStats(NamedTuple):
    ID: int
    mapname: str
    futures: bool
    options: bool
    splurges: bool
    participants: List[str]
    scores: List[int]
    timestamp: int  # TODO: datetime?


class WaitingPlayer(NamedTuple):
    stats: PlayerStats
    deadline: float


class MatchInfo(NamedTuple):
    participants: List[bool]
    map: Map
    mapname: str
    settings: Settings
