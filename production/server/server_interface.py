from typing import NamedTuple, List, Dict
from datetime import datetime
from time import time

from production.bot_interface import Map, Settings
INFINITY = 3600

class PlayerStats(NamedTuple):
    ID: int
    name: str
    token: str
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


class MatchInfo(NamedTuple):
    participants: List[str]
    map: Map
    mapname: str
    settings: Settings


class ServerInterrupt(KeyboardInterrupt):
    pass


class Estimation:
    def __init__(self):
        self.estimation = time() + INFINITY
        pass

    def start(self, N, turns):
        self.timestart = time()
        self.turns = turns
        self.estimation = time() + 10 * N + turns

    def set(self, i):
        if i == 0:
            return
        self.estimation = (time() - self.timestart) / i * self.turns + self.timestart

    def get(self):
        return self.estimation


class Player:
    def __init__(self, stats: PlayerStats):
        self.stats = stats
#        self.lastgame = lastgame
        self.waiting = []
        self.ongoing = []

    def new_conn(self, deadline):
        self.waiting.append(deadline)

    def dead_conn(self, deadline):
        self.waiting.remove(deadline)

    def in_game(self, deadline, time_estimation):
        self.waiting.remove(deadline)
        self.ongoing.append(time_estimation)

    def end_game(self, time_estimation):
        while time_estimation in self.ongoing:
            self.ongoing.remove(time_estimation)

    def count(self):
        return len(self.waiting) + len(self.ongoing)

    def first_deadline(self):
        return min(self.waiting) if self.waiting else time() + INFINITY

    def first_finish(self):
        return min(x.get() for x in self.ongoing) if self.ongoing else time() + INFINITY

    def copy(self):
        newplayer = Player(self.stats)
        newplayer.waiting = self.waiting[:]
        newplayer.ongoing = self.ongoing[:]
        return newplayer