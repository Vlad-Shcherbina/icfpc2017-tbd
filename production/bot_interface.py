from abc import ABCMeta, abstractmethod
from typing import Any, NamedTuple, Callable, Set, Dict, Tuple, Union, List, Optional


Graph = Dict[int, Set[int]]
# GvR form:
#   iter(G) lists all vertices
#   iter(G[v]) lists the neighbors of v
#   w in G[v] tests adjacency


class Map(NamedTuple):
    g: Graph
    mines: Set[int]

    raw_map: dict  # map in their format

    # Dunno if we can rely on these, but it's nice for visualization.
    site_coords: Dict[int, Tuple[float, float]]


GameState = Any
# Better call it 'bot state', but they chose this name.
# teuwer | @mangbo: GameState
#        | should be a valid JSON
#        | value.
# @rudih | vuvko: the game state
#        | is your data and is not
#        | changed


class Settings(NamedTuple):
    futures: bool
    splurges: bool
    options: bool
    raw_settings: dict


#-------------------------- MOVE TYPES ---------------------------------#

class PassMove(NamedTuple):
    punter: int

    @staticmethod
    def key(): return 'pass'

    def verify(self, board) -> bool:
        return True

    def play(self, board):
        board.passmove(self.punter)


class ClaimMove(NamedTuple):
    punter: int
    source: int
    target: int

    @staticmethod
    def key(): return 'claim'

    def verify(self, board) -> bool:
        return board.verify_claim(self.punter, self.source, self.target)

    def play(self, board):
        board.claim(self.punter, self.source, self.target)


class OptionMove(NamedTuple):
    punter: int
    source: int
    target: int

    @staticmethod
    def key(): return 'option'

    def verify(self, board) -> bool:
        return board.verify_option(self.punter, self.source, self.target)

    def play(self, board):
        board.option(self.punter, self.source, self.target)


class SplurgeMove(NamedTuple):
    punter: int
    route: List[int]

    @staticmethod
    def key(): return 'splurge'

    def verify(self, board) -> bool:
        return board.verify_splurge(self.punter, self.route)

    def play(self, board):
        board.splurge(self.punter, self.route)


Move = Union[ClaimMove, PassMove, SplurgeMove, OptionMove]


#------------------------ REQUEST AND RESPONSE -------------------------#

class SetupRequest(NamedTuple):
    punter: int  # my punter ID
    punters: int
    map: Map
    settings: Settings

class SetupResponse(NamedTuple):
    ready: int  # my punter ID
    state: GameState
    futures: Dict[int, int]  # mine -> target


class GameplayRequest(NamedTuple):
    moves: List[Move]
    raw_moves: list  # moves in their format
    state: GameState

class GameplayResponse(NamedTuple):
    move: Move
    state: GameState


class ScoreRequest(NamedTuple):
    state: GameState
    moves: List[Move]
    score_by_punter: Dict[int, int]



#------------------------- OTHER GAME OBJECTS --------------------------#

class Bot(metaclass=ABCMeta):
    """Pretty much reflects the offline protocol.

    Naturally, should not contain any game state, only bot tuning parameters.
    """
    @abstractmethod
    def setup(self, request: SetupRequest) -> SetupResponse:
        raise NotImplementedError()

    @abstractmethod
    def gameplay(self, request: GameplayRequest) -> GameplayResponse:
        raise NotImplementedError()


class Story(NamedTuple):
    """Everything that's needed to reconstruct the state of the game."""
    punters: int
    my_id: int
    settings: Settings
    map: Map
    my_futures: Dict[int, int]
    moves: List[Move]               # no SplurgeMoves: unpacked before adding
    score: Optional[Dict[int, int]] = None  # score as reported by the server

    def remaining_options(self) -> int:
        r = 0
        if self.settings.options:
            r = len(self.map.mines)
        for move in self.moves:
            if isinstance(move, OptionMove) and move.punter == self.my_id:
                r -= 1
        assert r >= 0, r
        return r
