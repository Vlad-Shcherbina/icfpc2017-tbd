from typing import NamedTuple, List, Dict, Union
import json

import logging;
logger = logging.getLogger(__name__)

from production.cpp.stuff import Board as Scoreboard
from production.bot_interface import *
from production.json_format import *


def pair(x, y):
    return (min(x, y), max(x, y))


#--------------------------- GAMEBOARD ---------------------------------#


class Gameboard:
    '''Python version of c++ board.

    Checks if move is legal and carries it out. Holds the current state 
    of the game. Does not count score.
    '''
    def __init__(self, adj: Graph, mines: Set, N: int, settings: Settings):
        self.adj = adj
        self.mines = mines
        self.claimed_by = {}
        self.optioned_by = {}
        self.futures_by_player = {}

        self.N = N
        self.passes = [0] * N
        self.settings = settings


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
    error: str


class GameHolder:
    def __init__(self, board: Gameboard):
        self.N = board.N
        self.board = board
        self.lastmove = [format_move(PassMove(punter=i)) for i in range(board.N)]


    def setup_response(self, ID: int, raw_map: dict) -> dict:
        return dict(punter=ID, punters=self.N, map=raw_map)


    def process_setup(self, punter: int, request: dict):
        '''Set futures, if there are any.'''
        if not self.board.settings.futures: return
        for f in request.get('futures', []):
            self.board.set_futures(punter, source=f['source'], target=f['target'])


    def process_request(self, punter: int, request: dict) -> str:
        '''Take move request in json and play it.

        Return false if unable to parse a valid (perhaps illegal) move.
        '''
        revised = self._request_to_move(punter, request)
        revised = self._make_move(revised)
        self.lastmove[punter] = format_move(revised.move)
        return revised


    def _request_to_move(self, punter: int, request: dict) -> RevisedMove:
        '''Parse move in json to Move. 

        If move is badly formatted, return Pass with error, bot goes to zombie.
        '''
        try:
            move = parse_move(request)
            if move.punter != punter:
                log.info(f'Punter {punter} declared wrong number {move.punter}')
                move.punter = punter
            return RevisedMove(move=move, error=None)

        except (AssertionError, KeyError) as e:
            return RevisedMove(move=PassMove(punter=punter), 
                               error='not a valid move')


    def _make_move(self, revised: RevisedMove) -> RevisedMove:
        '''Check if move is legal and play it.

        If move is illegal, return Pass with error, bot keeps running.
        '''
        if revised.move.key() == 'pass': return revised
        if revised.move.verify(self.board):
            revised.move.play(self.board)
            return revised
        else:
            return RevisedMove(move = PassMove(punter=revised.move.punter), 
                               error='illegal move')


    def get_response(self):
        '''Get json to pass to the next player.'''
        return dict(move = dict(moves=self.lastmove[:]))


    def score(self):
        '''Score is list of lists for each player.

        First number is base score, others are futures.
        '''
        scoreboard = construct_scoreboard(self.board)
        totals = scoreboard.totals(self.N)
        return totals


if __name__ == '__main__':
    pass
