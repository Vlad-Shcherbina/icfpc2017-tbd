import copy
import logging

from production.bot_interface import *
from production.json_format import parse_map, parse_move
from production.cpp import stuff as cpp


def reconstruct_board(map: Map, moves: List[Move]):
    # Pack site IDs to contiguous range.
    pack = {}
    unpack = []
    for s in map.g:
        pack[s] = len(unpack)
        unpack.append(s)

    adj = [[] for _ in map.g]
    for u, ws in map.g.items():
        for w in ws:
            adj[pack[u]].append(pack[w])

    mines = [pack[m] for m in map.mines]

    board = cpp.Board(adj, mines)

    for move in moves:
        if isinstance(move, ClaimMove):
            board.claim_river(
                move.punter, pack[move.source], pack[move.target])

    return board


class CppBot(Bot):
    """
    GameState format:
    {'punters': 142,
     'my_id': 42,
     'settings': <in their format>
     'map': <in their format>
     'all_past_moves': <list of moves in their format>}
    """

    def setup(self, req: SetupRequest) -> SetupResponse:
        state = dict(
            punters=req.punters,
            my_id=req.punter,
            settings=req.settings.raw_settings,
            map=req.map.raw_map,
            all_past_moves=[])
        # TODO: futures
        return SetupResponse(ready=req.punter, state=state, futures={})

    def gameplay(self, req: GameplayRequest) -> GameplayResponse:
        map = parse_map(req.state['map'])

        new_state = copy.deepcopy(req.state)
        new_state['all_past_moves'] += req.raw_moves

        moves = [parse_move(m) for m in new_state['all_past_moves']]

        rivers = set((u, w) for u, ws in map.g.items() for w in ws)
        for move in moves:
            if isinstance(move, ClaimMove):
                rivers.remove((move.source, move.target))
                rivers.remove((move.target, move.source))

        board = reconstruct_board(map, moves)
        predicted_score = {}
        for punter in range(req.state['punters']):
            predicted_score[punter] = board.base_score(punter)
        logging.info(f'predicted score: {predicted_score}')

        if rivers:
            source, target = min(rivers)
            move = ClaimMove(
                punter=req.state['my_id'],
                source=source, target=target)
        else:
            move = PassMove(punter=req.state['my_id'])


        return GameplayResponse(move=move, state=new_state)
