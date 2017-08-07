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

from production import dumb_bots
from production import cpp_bot
from production.cpp import stuff as cpp
from production.cpp import glue
from production import json_format
from production.bot_interface import *
from production import scraper
from production import utils
from production import comms


def story_from_msg(msg: Union[GameplayRequest, ScoreRequest]) -> Story:
    map_ = json_format.parse_map(msg.state['map'])
    moves = list(map(json_format.parse_move, msg.state['all_past_moves']))
    moves += msg.moves

    story = Story(
        punters=msg.state['punters'],
        my_id=msg.state['my_id'],
        settings=json_format.parse_settings(msg.state['settings']),
        map=map_,
        my_futures=dict(msg.state['my_futures']),
        moves = moves)
    if isinstance(msg, ScoreRequest):
        story = story._replace(score=msg.score_by_punter)

    return story


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

            im = cpp_bot.render_prob_field(story_from_msg(msg))
            im.save(utils.project_root() / 'outputs' / f'{turn_number:04d}.png')

            turn_number += 1

        return msg

    scores = comms.online_mainloop(
        'punter.inf.ed.ac.uk', game.port, 'tbdbd', bot, on_comms_cb=cb, game=game)

    logger.info(f'my id: {scores.state["my_id"]}')
    logger.info(f'scores: {scores.score_by_punter}')


if __name__ == '__main__':
    main()
