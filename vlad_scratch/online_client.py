try:
    import hintcheck
    hintcheck.monkey_patch_named_tuple_constructors()
except ImportError:
    pass

import logging; log = logging.getLogger(__name__)

import json
import time
import socket

from production import dumb_bots
from production import json_format
from production.bot_interface import *
from production import scraper
from production import visualization
from production import utils
from production import comms


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
        predicate=scraper.only_easy_eagers_p, extensions={'futures'})
    bot = dumb_bots.FirstMoveBot()

    turn_number = 0

    def cb(msg):
        nonlocal turn_number

        if isinstance(msg, GameplayResponse):
            log.info(f'my move: {msg.move}')

        if isinstance(msg, (GameplayRequest, ScoreRequest)):
            log.info(f'state.all_past_moves: {msg.state["all_past_moves"]}')
            vis = visualization.Visualization(600, 600)
            vis.draw_background()
            map_ = json_format.parse_map(msg.state['map'])
            vis.draw_map(map_)
            vis.draw_legend(msg.state['punters'])

            for move in msg.state['all_past_moves']:
                move = json_format.parse_move(move)
                vis.draw_move(move, map_)

            im = vis.get_image()
            im.save(utils.project_root() / 'outputs' / f'{turn_number:04d}.png')

            turn_number += 1

        return msg

    comms.online_mainloop(
        'punter.inf.ed.ac.uk', game.port, 'tbdbd', bot, on_comms_cb=cb)


if __name__ == '__main__':
    main()
