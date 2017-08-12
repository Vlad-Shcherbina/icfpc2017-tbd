# !python3
# source:
# https://github.com/Vlad-Shcherbina/icfpc2017-tbd/blob/master/production/bot_interface.py

from abc import ABCMeta, abstractmethod
from typing import Any, NamedTuple, Callable, Set, Dict, Tuple, Union, List, Optional

import logging;
logger = logging.getLogger(__name__)


Graph = Dict[int, Set[int]]
# GvR form:
#   iter(G) lists all vertices
#   iter(G[v]) lists the neighbors of v
#   w in G[v] tests adjacency


class Settings(NamedTuple):
    futures: bool
    splurges: bool
    options: bool
    raw_settings: dict


class REQ_ERROR(NamedTuple):
    valid = ''
    zombie = 'zombie'
    disconnected = 'disconnected'
    timeout = 'timeout'
    parsejson = 'json reading error'
    parsemove = 'move reading error'
    illegal = 'illegal move'


def pair(x, y):
    return (min(x, y), max(x, y))


#----------------------------- BOARD -----------------------------------#

class Board:
    def __init__(self, adj: Graph, mines: Set):
        self.adj = adj
        self.mines = mines
        self.claimed_by = {}
        self.optioned_by = {}
        self.futures_by_player = {}

        self.N = 0 
        self.passes = None
        self.settings = None


    def adjacent(self, u: int, v: int) -> bool:
        return u in self.adj and v in self.adj[u]


    def set_futures(self, punter: int, source: int, target: int):
        #if not self.settings.futures:   # checked in gameholder
        #    return
        if not source in self.mines: 
            return
        if not punter in self.futures_by_player:
            self.futures_by_player[punter] = []
        self.futures_by_player[punter].append((source, target))


    def verify_claim(self, punter: int, source: int, target: int) -> bool:
        if not self.adjacent(source, target):
            return False
        return pair(source, target) not in self.claimed_by


    def verify_option(self, punter: int, source: int, target: int) -> bool:
        if not self.settings.options: 
            return False
        if not self.adjacent(source, target):
            return False
        river = pair(source, target)
        return (river in self.claimed_by 
                and not self.claimed_by[river] == punter
                and not river in self.optioned_by)


    def verify_splurge(self, punter: int, route: List[int]) -> bool:
        if not self.settings.splurges: 
            return False
        # no duplicate rivers!
        return all((self.verify_claim(punter, s, t) 
                    or self.verify_option(punter, s, t))
                    for s, t in zip(route, route[1:]))


    def claim(self, punter: int, source: int, target: int):
        self.claimed_by[pair(source, target)] = punter
        self.passes[punter] = 0


    def option(self, punter: int, source: int, target: int):
        self.optioned_by[pair(source, target)] = punter
        self.passes[punter] = 0


    def splurge(self, punter: int, route: List[int]):
        for source, target in zip(route, route[1:]):
            river = pair(source, target)
            if river in self.claimed_by:
                self.optioned_by[river] = punter
            else:
                self.claimed_by[river] = punter
        self.passes[punter] = 0


    def passmove(self, punter: int):
        self.passes[punter] += 1


#-------------------------- MOVE TYPES ---------------------------------#

class PassMove(NamedTuple):
    punter: int
    msg: str = ''
    timespan: float = 0.0

    @staticmethod
    def key(): return 'pass'

    def verify(self, board: Board) -> bool:
        return True

    def play(self, board: Board):
        board.passmove(self.punter)


class ClaimMove(NamedTuple):
    punter: int
    source: int
    target: int
    timespan: float = 0.0

    @staticmethod
    def key(): return 'claim'

    def verify(self, board: Board) -> bool:
        return board.verify_claim(self.punter, self.source, self.target)

    def play(self, board: Board):
        board.claim(self.punter, self.source, self.target)


class OptionMove(NamedTuple):
    punter: int
    source: int
    target: int
    timespan: float = 0.0

    @staticmethod
    def key(): return 'option'

    def verify(self, board: Board) -> bool:
        return board.verify_option(self.punter, self.source, self.target)

    def play(self, board: Board):
        board.option(self.punter, self.source, self.target)


class SplurgeMove(NamedTuple):
    punter: int
    route: List[int]
    timespan: float = 0.0

    @staticmethod
    def key(): return 'splurge'

    def verify(self, board: Board) -> bool:
        return board.verify_splurge(self.punter, self.route)

    def play(self, board: Board):
        board.splurge(self.punter, self.route)


Move = Union[ClaimMove, PassMove, SplurgeMove, OptionMove]


#------------------------ REQUEST AND RESPONSE -------------------------#

# class SetupResponse(NamedTuple):
#     punter: int  # my punter ID
#     punters: int
#     map: Board
#     settings: Settings

# class SetupRequest(NamedTuple):
#     ready: int  # my punter ID
#     state: str #GameState
#     futures: Dict[int, int]  # mine -> target


# class GameplayResponce(NamedTuple):
#     moves: List[Move]
#     raw_moves: list  # moves in their format
#     state: str #GameState

# class GameplayRequest(NamedTuple):
#     move: Move
#     state: str #GameState


# class ScoreResponse(NamedTuple):
#     moves: List[Move]
#     score_by_punter: Dict[int, int]
#     state: str #GameState


# class Bot(metaclass=ABCMeta):
#     """Pretty much reflects the offline protocol.

#     Naturally, should not contain any game state, only bot tuning parameters.
#     """
#     @abstractmethod
#     def setup(self, request: SetupRequest) -> SetupResponse:
#         raise NotImplementedError()

#     @abstractmethod
#     def gameplay(self, request: GameplayRequest) -> GameplayResponse:
#         raise NotImplementedError()
