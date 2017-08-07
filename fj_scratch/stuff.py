import logging; log = logging.getLogger(__name__)
import production
from production.comms import online_mainloop
from production import scraper, json_format
json_format.REPORT_UNKNOWN_FIELDS = True

import time
import distutils.core
import distutils.util

# I don't like waiting 5-10 seconds for this shit
import pybind11
pybind11.get_include = lambda *args, **kwargs: 'C:/Users/Fj/Miniconda3/Include'

def main():
    '''Please don't change predicates or bots, this is a smoke test'''
    from production.utils import config_logging
    config_logging()
    log.setLevel(logging.DEBUG)

    log.debug('hello')
    name='stuff'
    sources=['stuff.cpp']
    headers=['debug.h', 'pretty_printing.h']

    from production.dumb_bots import FirstMoveBot
    from production.cpp_bot import CppBot
    bot = FirstMoveBot()
    # bot = CppBot()

    game = scraper.wait_for_game(predicate=scraper.only_easy_eagers_p, extensions={'futures', 'splurges', 'options'})
    log.info(f'Joining {game}')
    scores = online_mainloop('punter.inf.ed.ac.uk', game.port, 'tbd testbot', bot, game=game)
    log.info(f'Scores: id={scores.state.get("my_id")} {scores.score_by_punter}')

if __name__ == '__main__':
    main()
