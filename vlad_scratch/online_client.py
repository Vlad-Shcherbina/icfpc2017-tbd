try:
    import hintcheck
    hintcheck.monkey_patch_named_tuple_constructors()
except ImportError:
    pass

import logging; log = logging.getLogger(__name__)

import json
import time
import socket
import collections
from typing import Union

from production import dumb_bots
from production import cpp_bot
from production import json_format
from production.bot_interface import *
from production import scraper
from production import visualization
from production import utils
from production import comms


def render(msg: Union[GameplayRequest, ScoreRequest]):
    vis = visualization.Visualization(600, 600)
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


    pack, unpack, board = cpp_bot.reconstruct_board(map_, moves)

    for source, target in msg.state['my_futures']:
        color = (255, 0, 0)
        if pack[target] in board.reachable_by_claimed(msg.state['my_id'], pack[source]):
            color = (0, 255, 0)

        vis.draw_edge(
            map_.site_coords[source],
            map_.site_coords[target],
            color=color, width=1)
        vis.draw_point(map_.site_coords[target], color=None, outline=color, size=10)

    return vis.get_image()


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
            log.info(f'my futures: {msg.futures}')
        if isinstance(msg, GameplayResponse):
            log.info(f'my move: {msg.move}')

        if isinstance(msg, (GameplayRequest, ScoreRequest)):
            log.info(f'server reports moves: {msg.moves}')

            im = render(msg)
            im.save(utils.project_root() / 'outputs' / f'{turn_number:04d}.png')

            turn_number += 1

        return msg

    scores = comms.online_mainloop(
        'punter.inf.ed.ac.uk', game.port, 'tbdbd', bot, on_comms_cb=cb)

    log.info(f'my id: {scores.state["my_id"]}')
    log.info(f'scores: {scores.score_by_punter}')


if __name__ == '__main__':
    main()
