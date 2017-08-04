import json
from abc import ABCMeta, abstractmethod
from typing import Any, NamedTuple, Callable


Map = Any  # TODO


GameState = Any
# Better call it 'bot state', but they chose this name.
# teuwer | @mangbo: GameState
#        | should be a valid JSON
#        | value.
# @rudih | vuvko: the game state
#        | is your data and is not
#        | changed


class SetupRequest(NamedTuple):
    punter: str
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
