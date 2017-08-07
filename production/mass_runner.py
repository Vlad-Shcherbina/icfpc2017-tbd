import logging; logger = logging.getLogger(__name__)

import time
import random
import urllib.error

from production import cpp_bot
from production import scraper
from production import comms
from production.bot_interface import *


class Abandon(Exception):
    pass


def cb(msg):
    """Abandon the game if each opponent passed 3 moves."""
    if not isinstance(msg, GameplayRequest):
        return msg
    pass_cnt = [0] * msg.state['punters']
    my_id = msg.state['my_id']
    for move in msg.state['all_past_moves'] + msg.raw_moves:
        if 'pass' in move:
            pass_cnt[move['pass']['punter']] += 1
    if all(c >= 3 for i, c in enumerate(pass_cnt) if i != my_id):
        logger.warning(f'ABANDON!! my id: {my_id}, pass cnt: {pass_cnt}')
        raise Abandon()
    return msg


def main(_):
    logging.basicConfig(
            level=logging.INFO,
            format='%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')

    while True:
        time.sleep(random.random() * 10)
        game = scraper.wait_for_game(
            patience=100,
            predicate=lambda g:
                g.map_name not in ('sample.json', 'circle.json', 'Sierpinski-triangle.json') and
                (not g.punters or
                 not any(p == 'eager punter'
                        for p in g.punters)),
            extensions={'futures', 'splurges', 'options'})
        logger.warning(f'found game {game}')
        bot = cpp_bot.CppBot()

        name = random.choice(['-skulk-', 'gorge', 'Fade', 'ONOS', 'lerk'])
        try:
            scores = comms.online_mainloop(
                'punter.inf.ed.ac.uk', game.port, name, bot, game=game)
        except Abandon:
            continue
        except (ConnectionRefusedError, TimeoutError, ConnectionResetError, urllib.error.URLError):
            logger.exception('aaa')
            continue

        logger.warn(f'my id: {scores.state["my_id"]} scores: {scores.score_by_punter}')


if __name__ == '__main__':
    import multiprocessing
    N = 30
    list(multiprocessing.Pool(N).map(main, range(N)))
