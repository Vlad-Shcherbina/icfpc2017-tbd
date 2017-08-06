import copy
import random
import logging
from typing import Dict
from math import log

from production.bot_interface import *
from production.json_format import parse_map, parse_move
from production.cpp import stuff as cpp
from production.cpp import glue
from production import visualization


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


def render_prob_field(story: Story, size=600):
    vis = visualization.Visualization(size, size)
    vis.draw_story(story)
    base_im = vis.get_image()

    vis = visualization.Visualization(size // 2, size // 2)
    vis.draw_background()
    vis.adjust_to_map(story.map)

    board = glue.reconstruct_board(story)

    pi = glue.compute_prob_info(story, board, story.my_id)

    a = min(0, min(pi.cut_prob_grad.values()))
    b = max(0, max(pi.cut_prob_grad.values()))

    vis.draw_text((15, 15), f'white: {-a}')
    vis.draw_text((15, 30), f'black: {-b}')

    for u, vs in story.map.g.items():
        for v in vs:
            if (u, v) not in pi.cut_prob_grad:
                continue
            t = (pi.cut_prob_grad[u, v] - a) / (b - a + 1e-6)
            c = int(255 * (1 - t))
            vis.draw_edge(
            story.map.site_coords[u],
            story.map.site_coords[v],
            color=(c, c, c), width=3)


    grad_im = vis.get_image()

    vis = visualization.Visualization(size // 2, size // 2)
    vis.draw_background()
    vis.draw_map(story.map)
    for site in story.map.g:
        p = max(reach_prob[site] for reach_prob in pi.reach_prob.values())
        vis.draw_point(
            story.map.site_coords[site],
            color=prob_palette(p), size=6)

    for i, p in enumerate([1, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001]):
        vis.draw_text((5, 15 * i + 5), f'p={p}', color=prob_palette(p))
    prob_im = vis.get_image()

    if vis.width < 2 * vis.height:
        return visualization.hstack(
            base_im, visualization.vstack(prob_im, grad_im))
    else:
        return visualization.vstack(
            base_im, visualization.hstack(prob_im, grad_im))


def prob_palette(p):
    assert 0 <= p <= 1

    def hz(a, b, x):
        return (log(x) - log(a)) / (log(b) - log(a))

    if p > 0.1:
        t = hz(0.1, 1.0, p)
        return (255, 255, int(255 * t))
    if p > 0.01:
        t = hz(0.01, 0.1, p)
        return (255, int(255 * t), 0)
    if p > 0.001:
        t = hz(0.001, 0.01, p)
        return (int(255 * t), 0, 0)
    return (0, 0, 0)
