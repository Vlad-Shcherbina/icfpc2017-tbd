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

class GameHolder:
    def __init__(self, mapfile: str, settings: Settings, N: int):
        self.N = N

        with open(mapfile) as f:
            self.board = parse_board(json.load(f))
        self.board.N = N
        self.board.passes = [0] * N
        self.board.settings = settings

        self.replay = []
        self.lastmove = [format_move(PassMove(punter=i)) for i in range(N)]


    def process_setup(self, punter: int, request):
        '''Set futures, if there are any.'''
        if not self.board.settings.futures: return
        for f in request.get('futures', []):
            self.board.set_futures(punter, source=f['source'], target=f['target'])


    def process_request(self, punter: int, request) -> bool:
        '''Take move request in json and play it.

        Return false if unable to parse a valid (perhaps illegal) move.
        '''
        move = self._request_to_move(punter, request)
        move = self._make_move(move)
        self.lastmove[punter] = format_move(move, False)
        self.replay.append(format_move(move, True))
        return not move.key == 'pass' or not move.error == 'move parsing error'


    def _request_to_move(self, punter: int, request):
        '''Parse move in json to Move. 

        If move is badly formatted, return Pass with error, bot goes to zombie.
        '''
        try:
            move = parse_move(request)
            return move
        except (AssertionError, KeyError) as e:
            return PassMove(punter=punter, error='move parsing error')


    def _make_move(self, move: Move) -> Move:
        '''Check if move is legal and play it.

        If move is illegal, return Pass with error, bot keeps running.
        '''
        if move.key() == 'pass': return move
        if move.verify(self.board):
            move.play(self.board)
        else:
            move = PassMove(punter=move.punter, error='illegal move')
        return move


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
