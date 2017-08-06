import copy
import random
import logging
from typing import Dict

from production.bot_interface import *
from production.json_format import parse_map, parse_move
from production.cpp import stuff as cpp


def reconstruct_board(
        map: Map, moves: List[Move], my_id: int, my_futures: Dict[int, int]):
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

    board.set_futures(my_id, {pack[k]: pack[v] for k, v in my_futures.items()})

    for move in moves:
        if isinstance(move, ClaimMove):
            board.claim_river(
                move.punter, pack[move.source], pack[move.target])

    return pack, unpack, board


class CppBot(Bot):
    """
    GameState format:
    {'punters': 142,
     'my_id': 42,
     'settings': <in their format>
     'my_futures': [[1, 2], [10, 20], ...]
     'map': <in their format>
     'all_past_moves': <list of moves in their format>}
    """

    def setup(self, req: SetupRequest) -> SetupResponse:
        futures = {}
        if req.settings.futures:
            not_mines = list(set(req.map.g) - set(req.map.mines))
            if not_mines:
                for mine in req.map.mines:
                    futures[mine] = random.choice(not_mines)

        state = dict(
            punters=req.punters,
            my_id=req.punter,
            settings=req.settings.raw_settings,
            my_futures=[[k, v] for k, v in futures.items()],
            map=req.map.raw_map,
            all_past_moves=[])

        return SetupResponse(ready=req.punter, state=state, futures=futures)

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

        pack, unpack, board = reconstruct_board(
            map, moves, req.state['my_id'], dict(req.state['my_futures']))
        predicted_score = {}
        for punter in range(req.state['punters']):
            predicted_score[punter] = board.base_score(punter)
        logging.info(f'predicted score: {predicted_score}')

        cut_prob_grad = {}
        for mine in map.mines:
            cut_prob = 1.0 - 1.0 / req.state['punters']
            rp = cpp.ReachProb(board, req.state['my_id'], pack[mine], cut_prob)
            for k, v in rp.get_cut_prob_grad().items():
                cut_prob_grad.setdefault(k, 0.0)
                cut_prob_grad[k] += v
        #logging.info(f'*********** {cut_prob_grad}')

        if cut_prob_grad:
            source, target = min(cut_prob_grad, key=cut_prob_grad.get)
            #logging.info(f'*** {source}, {target}')
            source = unpack[source]
            target = unpack[target]
            move = ClaimMove(
                punter=req.state['my_id'],
                source=source, target=target)
        else:
            move = PassMove(punter=req.state['my_id'])


        return GameplayResponse(move=move, state=new_state)
