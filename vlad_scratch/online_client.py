try:
    import hintcheck
    hintcheck.monkey_patch_named_tuple_constructors()
except ImportError:
    pass

import logging; logger = logging.getLogger(__name__)

import json
import time
import socket
import collections
from typing import Union
from math import log

from production import dumb_bots
from production import cpp_bot
from production.cpp import stuff as cpp
from production import json_format
from production.bot_interface import *
from production import scraper
from production import visualization
from production import utils
from production import comms


def render(msg: Union[GameplayRequest, ScoreRequest], size=600):
    vis = visualization.Visualization(size, size)
    vis.draw_background()
    map_ = json_format.parse_map(msg.state['map'])
    vis.draw_map(map_)

    legend = [f'[{i}]' for i in range(msg.state['punters'])]

    if isinstance(msg, ScoreRequest):
        for k, v in msg.score_by_punter.items():
            legend[k] += f' score={v}'

    moves = list(map(json_format.parse_move, msg.state['all_past_moves']))
    moves += msg.moves

    pass_cnt = collections.Counter()
    for i, move in enumerate(moves):
        # Don't count pass moves in the first turn
        # (they send placeholder pass moves for each player).
        if i < msg.state['punters']:
            continue
        # Also, weirdly, they don't send the very last moves
        # in the score request, and replace them with passes instead.
        if isinstance(msg, ScoreRequest) and i >= len(moves) - msg.state['punters']:
            continue
        if isinstance(move, PassMove):
            pass_cnt[move.punter] += 1
    for i in range(len(legend)):
        if pass_cnt[i]:
            legend[i] += f' passed {pass_cnt[i]} times'

    legend[msg.state['my_id']] += ' (me)'
    vis.draw_legend(legend)

    for move in moves:
        me=(move.punter==msg.state['my_id'])
        vis.draw_move(move, map_, me=me)

    pack, unpack, board = cpp_bot.reconstruct_board(
        map_, moves, msg.state['my_id'], dict(msg.state['my_futures']))

    for source, target in msg.state['my_futures']:
        color = (255, 0, 0)
        if pack[target] in board.reachable_by_claimed(msg.state['my_id'], pack[source]):
            color = (0, 255, 0)

        vis.draw_edge(
            map_.site_coords[source],
            map_.site_coords[target],
            color=color, width=1)
        vis.draw_point(map_.site_coords[target], color=None, outline=color, size=10)

    base_im = vis.get_image()

    vis = visualization.Visualization(size // 2, size // 2)
    vis.draw_background()
    vis.adjust_to_map(map_)

    cut_prob_grad = {}
    reach_probs = []
    for mine in map_.mines:
        cut_prob = 1.0 - 1.0 / msg.state['punters']
        rp = cpp.ReachProb(board, msg.state['my_id'], pack[mine], cut_prob)
        reach_probs.append(rp.reach_prob)
        for k, v in rp.get_cut_prob_grad().items():
            cut_prob_grad.setdefault(k, 0.0)
            cut_prob_grad[k] += v

    a = min(0, min(cut_prob_grad.values()))
    b = max(0, max(cut_prob_grad.values()))

    vis.draw_text((15, 15), f'white: {-a}')
    vis.draw_text((15, 30), f'black: {-b}')

    for u, vs in map_.g.items():
        for v in vs:
            uv = pack[u], pack[v]
            if uv not in cut_prob_grad:
                continue
            t = (cut_prob_grad[uv] - a) / (b - a + 1e-6)
            c = int(255 * (1 - t))
            vis.draw_edge(
            map_.site_coords[u],
            map_.site_coords[v],
            color=(c, c, c), width=3)


    grad_im = vis.get_image()

    vis = visualization.Visualization(size // 2, size // 2)
    vis.draw_background()
    vis.draw_map(map_)
    for site in map_.g:
        p = max(reach_prob[pack[site]] for reach_prob in reach_probs)
        vis.draw_point(
            map_.site_coords[site],
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


def main():
    try:
        import hintcheck
        hintcheck.hintcheck_all_functions()
    except ImportError:
        pass

    logging.basicConfig(
        level=logging.INFO,
        format='%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')

    game = scraper.wait_for_game(
        predicate=scraper.only_easy_eagers_p,
        #predicate=lambda g: not scraper.only_eagers_p(g) and g.map_name != 'sample.json',
        extensions={'futures'})
    bot = cpp_bot.CppBot()

    turn_number = 0

    def cb(msg):
        nonlocal turn_number

        if isinstance(msg, SetupResponse):
            logger.info(f'my futures: {msg.futures}')
        if isinstance(msg, GameplayResponse):
            logger.info(f'my move: {msg.move}')

        if isinstance(msg, (GameplayRequest, ScoreRequest)):
            logger.info(f'server reports moves: {msg.moves}')

            im = render(msg)
            im.save(utils.project_root() / 'outputs' / f'{turn_number:04d}.png')

            turn_number += 1

        return msg

    scores = comms.online_mainloop(
        'punter.inf.ed.ac.uk', game.port, 'tbdbd', bot, on_comms_cb=cb, game=game)

    logger.info(f'my id: {scores.state["my_id"]}')
    logger.info(f'scores: {scores.score_by_punter}')


if __name__ == '__main__':
    main()
