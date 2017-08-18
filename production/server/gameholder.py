from typing import NamedTuple, List, Dict, Union, Set
import json

import logging;
logger = logging.getLogger(__name__)

from production.cpp.stuff import Board as Scoreboard
from production.bot_interface import *
from production.json_format import format_move, format_settings


#--------------------------- GAMEBOARD ---------------------------------#

def pair(x, y):
    return (min(x, y), max(x, y))


class Gameboard:
    '''Python version of c++ board.

    Checks if move is legal and carries it out. Holds the current state 
    of the game. Does not count score.
    '''
    def __init__(self, adj: Graph, mines: Set[int], N: int, settings: Settings):
        self.adj = adj
        self.mines = mines
        self.claimed_by = {}
        self.optioned_by = {}
        self.futures_by_player = {}

        self.N = N
        self.passes = [0] * N
        self.settings = settings
        self.remaining_options = [len(mines)] * self.N


    def adjacent(self, u: int, v: int) -> bool:
        return u in self.adj and v in self.adj[u]


    def set_futures(self, punter: int, source: int, target: int):
        # futures setting checked in gameholder.
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
        if (not self.settings.options 
                or self.remaining_options[punter] == 0
                or not self.adjacent(source, target)):
            return False
        river = pair(source, target)
        return (river in self.claimed_by 
                and not self.claimed_by[river] == punter
                and not river in self.optioned_by)


    def verify_splurge(self, punter: int, route: List[int]) -> bool:
        if not self.settings.splurges: 
            return False
        rivers = list(pair(x, y) for x, y in zip(route, route[1:]))
        if len(set(rivers)) < len(rivers): return False   # duplicating rivers
        options_needed = 0
        for river in rivers:
            if not self.adjacent(*river):
                return False
            if river in self.claimed_by:
                if not self.verify_option(punter, *river):
                    return False
                options_needed += 1
        return options_needed <= self.remaining_options[punter]


    def claim(self, punter: int, source: int, target: int):
        self.claimed_by[pair(source, target)] = punter
        self.passes[punter] = 0


    def option(self, punter: int, source: int, target: int):
        self.optioned_by[pair(source, target)] = punter
        self.passes[punter] = 0
        self.remaining_options[punter] -= 1


    def splurge(self, punter: int, route: List[int]):
        for source, target in zip(route, route[1:]):
            river = pair(source, target)
            if river in self.claimed_by:
                self.optioned_by[river] = punter
                self.remaining_options[punter] -= 1
            else:
                self.claimed_by[river] = punter
        assert self.remaining_options[punter] >= 0
        self.passes[punter] = 0


    def passmove(self, punter: int):
        self.passes[punter] += 1


#--------------------------- SCOREBOARD --------------------------------#

def construct_scoreboard(board: Gameboard) -> Scoreboard:
    '''Converts gameboard to scoreboard (c++ board).

    One-way conversion (no unpacking) to be used solely for score 
    calculations. 
    '''
    pack = {}
    index = 0
    for s in board.adj:
        pack[s] = index
        index += 1

    adj = [[] for _ in board.adj]
    for u, ws in board.adj.items():
        for w in ws:
            adj[pack[u]].append(pack[w])

    mines = [pack[m] for m in board.mines]

    scoreboard = Scoreboard(adj, mines)

    for player, futures_set in board.futures_by_player.items():
        scoreboard.set_futures(
                player, {pack[k]: pack[v] for k, v in futures_set})

    claimed_rivers = []
    for river, punter in board.claimed_by.items():
        claimed_rivers.append((punter, pack[river[0]], pack[river[1]]))
    scoreboard.set_claims(claimed_rivers)

    optioned_rivers = []
    for river, punter in board.optioned_by.items():
        optioned_rivers.append((punter, pack[river[0]], pack[river[1]]))
    scoreboard.set_options(optioned_rivers)

    return scoreboard


#--------------------------- GAME HOLDER -------------------------------#

class RevisedMove(NamedTuple):
    move: Move
    error: Optional[str]


class GameHolder:
    '''Update game state, verify whether move is legal, calculate score.

    self.process_xxx()
        gets pre-parsed Responces, guaranteed to be valid.

    self.get_xxx()
        returns formatted jsons
    '''
    def __init__(self, board: Gameboard):
        self.N = board.N
        self.board = board
        self.lastmove = [format_move(PassMove(punter=i)) for i in range(self.N)]
        self.totals = None


    def get_setup_request(self, ID: int, rawmap: dict) -> dict:
        return { 'punter' : ID,
                 'punters' : self.N,
                 'map' : rawmap,
                 'settings' : format_settings(self.board.settings)}


    def process_futures(self, response: SetupResponse):
        if not self.board.settings.futures: return
        for s, t in response.futures.items():
            self.board.set_futures(response.ready, s, t)


    def get_gameplay_request(self):
        return { 'move' : { 'moves' : self.lastmove[:] } }


    def process_move(self, move: Move) -> Optional[str]:
        error = None
        if move.verify(self.board):
            move.play(self.board)
        else:
            move = PassMove(punter=move.punter)
            error = 'illegal move'

        self.lastmove[move.punter] = format_move(move)
        return error


    def get_score_request(self):
        if self.totals is None: self.score()
        scores = [dict(punter=i, score=sum(self.totals[i])) for i in range(self.N)]
        return dict(stop = dict(moves=self.lastmove[:], scores=scores))


    def score(self):
        '''Returns summary score for each player.

        Also calculates totals -- list of lists for each player.
        First number is base score, others are futures.
        '''
        scoreboard = construct_scoreboard(self.board)
        self.totals = scoreboard.totals(self.N)
        return [sum(s) for s in self.totals]


if __name__ == '__main__':
    pass
