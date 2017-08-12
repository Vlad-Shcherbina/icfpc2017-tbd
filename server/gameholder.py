from typing import NamedTuple, List, Dict, Union
import json

import logging;
logger = logging.getLogger(__name__)

from stuff import Scoreboard
from server_interface import *
from json_format import *


class GameHolder:
    def __init__(self, mapfile: str, settings: Settings, N: int):
        self.N = N

        with open(mapfile) as f:
            self.board = parse_map(json.load(f))
        self.board.N = N
        self.board.passes = [0] * N
        self.board.settings = settings

        self.replay = []
        self.lastmove = [format_move(PassMove(punter=i), False) for i in range(N)]


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
        return not move.key == 'pass' or not move.msg == REQ_ERROR.parsemove


    def _request_to_move(self, punter: int, request):
        '''Parse move in json to Move. 

        If move is badly formatted, return Pass with error, bot goes to zombie.
        '''
        try:
            move = parse_move(request)
            return move
        except (AssertionError, KeyError) as e:
            return PassMove(punter=punter, msg=REQ_ERROR.parsemove)


    def _make_move(self, move: Move) -> Move:
        '''Check if move is legal and play it.

        If move is illegal, return Pass with error, bot works.
        '''
        if move.key() == 'pass': return move
        if move.verify(self.board):
            move.play(self.board)
        else:
            move = PassMove(punter=move.punter, msg=REQ_ERROR.illegal)
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


# def state_from_setup_req(req, futures):
#     return dict(
#             punters=req.punters,
#             my_id=req.punter,
#             settings=req.settings.raw_settings,
#             my_futures=[[k, v] for k, v in futures.items()],
#             map=req.map.raw_map,
#             all_past_moves=[])


def construct_scoreboard(board: Board) -> Scoreboard:
    # Pack site IDs to contiguous range.
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


if __name__ == '__main__':
    pass
