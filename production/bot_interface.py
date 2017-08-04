from abc import ABCMeta, abstractmethod
from typing import Any, NamedTuple, Callable, Set, Dict, Tuple


Graph = Dict[int, Set[int]]
# GvR form:
#   iter(G) lists all vertices
#   iter(G[v]) lists the neighbors of v
#   w in G[v] tests adjacency


class Map(NamedTuple):
    g: Graph
    mines: Set[int]

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


class SetupRequest(NamedTuple):
    punter: str  # my name
    punters: int
    map: Map

class SetupResponse(NamedTuple):
    ready: str  # my name
    state: GameState


class GameplayRequest(NamedTuple):
    moves: Any  # TODO
    state: GameState

class GameplayResponse(NamedTuple):
    # TODO: don't undestand what they wrote about the disjoint union
    move: Any  # TODO
    state: GameState


class ScoreRequest(NamedTuple):
    state: GameState
    moves: Any  # TODO
    scores: Any  # TODO


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

    @abstractmethod
    def score(self, request: ScoreRequest) -> None:
        raise NotImplementedError()
