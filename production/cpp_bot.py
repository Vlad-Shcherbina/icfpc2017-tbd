import copy
import random
import logging
from typing import Dict

from production.bot_interface import *
from production.json_format import parse_map, parse_move
from production.cpp import stuff as cpp
from production.cpp import glue


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

        story = Story(
            punters=new_state['punters'],
            my_id=new_state['my_id'],
            map=map,
            my_futures=dict(new_state['my_futures']),
            moves=moves)

        rivers = set((u, w) for u, ws in map.g.items() for w in ws)
        for move in moves:
            if isinstance(move, ClaimMove):
                rivers.remove((move.source, move.target))
                rivers.remove((move.target, move.source))

        board = glue.reconstruct_board(story)
        pack = board.pack
        unpack = board.unpack

        predicted_score = {}
        for punter in range(req.state['punters']):
            predicted_score[punter] = board.base_score(punter)
        logging.info(f'my id: {story.my_id}, predicted score: {predicted_score}')

        pi = glue.compute_prob_info(story, board, story.my_id)
        #logging.info(f'*********** {cut_prob_grad}')

        if pi.cut_prob_grad:
            source, target = min(pi.cut_prob_grad, key=pi.cut_prob_grad.get)
            #logging.info(f'*** {source}, {target}')
            move = ClaimMove(
                punter=req.state['my_id'],
                source=source, target=target)
        else:
            move = PassMove(punter=req.state['my_id'])


        return GameplayResponse(move=move, state=new_state)
