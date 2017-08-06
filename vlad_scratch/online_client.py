try:
    import hintcheck
    hintcheck.monkey_patch_named_tuple_constructors()
except ImportError:
    pass

import logging; logger = logging.getLogger(__name__)

import json
import time
import socket
from typing import Union
from math import log

from production import dumb_bots
from production import cpp_bot
from production.cpp import stuff as cpp
from production.cpp import glue
from production import json_format
from production.bot_interface import *
from production import scraper
from production import visualization
from production import utils
from production import comms


def render(msg: Union[GameplayRequest, ScoreRequest], size=600):
    map_ = json_format.parse_map(msg.state['map'])
    moves = list(map(json_format.parse_move, msg.state['all_past_moves']))
    moves += msg.moves

    story = Story(
        punters=msg.state['punters'],
        my_id=msg.state['my_id'],
        map=map_,
        my_futures=dict(msg.state['my_futures']),
        moves = moves)
    if isinstance(msg, ScoreRequest):
        story = story._replace(score=msg.score_by_punter)

    vis = visualization.Visualization(size, size)
    vis.draw_story(story)
    base_im = vis.get_image()

    vis = visualization.Visualization(size // 2, size // 2)
    vis.draw_background()
    vis.adjust_to_map(map_)

    board = glue.reconstruct_board(story)

    pi = glue.compute_prob_info(story, board, story.my_id)

    a = min(0, min(pi.cut_prob_grad.values()))
    b = max(0, max(pi.cut_prob_grad.values()))

    vis.draw_text((15, 15), f'white: {-a}')
    vis.draw_text((15, 30), f'black: {-b}')

    for u, vs in map_.g.items():
        for v in vs:
            if (u, v) not in pi.cut_prob_grad:
                continue
            t = (pi.cut_prob_grad[u, v] - a) / (b - a + 1e-6)
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
        p = max(reach_prob[site] for reach_prob in pi.reach_prob.values())
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
