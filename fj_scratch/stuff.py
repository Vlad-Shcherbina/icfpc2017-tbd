import logging; log = logging.getLogger(__name__)
import production
from production.comms import online_mainloop, online_mainloop_pseudoasync
from production import scraper, json_format
json_format.REPORT_UNKNOWN_FIELDS = True

# I don't like waiting 5-10 seconds for this shit
import pybind11
pybind11.get_include = lambda *args, **kwargs: 'C:/Users/Fj/Miniconda3/Include'


def check_FirstMoveBot():
    from production.dumb_bots import FirstMoveBot
    bot = FirstMoveBot()

    game = scraper.wait_for_game(predicate=scraper.only_easy_eagers_p, extensions={'futures', 'splurges', 'options'})

    log.info(f'Joining {game}')
    scores = online_mainloop('punter.inf.ed.ac.uk', game.port, 'tbd testbot', bot, game=game)
    log.info(f'Scores: id={scores.state.get("my_id")} {scores.score_by_punter}')


def check_FirstMove_and_CppBot():
    from production.dumb_bots import FirstMoveBot
    from production.cpp_bot import CppBot
    bots = [FirstMoveBot(), CppBot()]

    def find_empty_room_for_two(g: scraper.Game):
        return (scraper.only_easy_maps(g) and
                g.punters_max == 2 and
                g.punters_num == 0)

    game = scraper.wait_for_game(predicate=find_empty_room_for_two, patience=1000, extensions={'futures', 'splurges', 'options'})
    log.info(f'Joining {game}')

    loops = [online_mainloop_pseudoasync('punter.inf.ed.ac.uk', game.port, 'tbd testbot', bot, game=game) for bot in bots]
    while not all(loop is None for loop in loops):
        for i, loop in enumerate(loops):
            scores = next(loop)
            if scores is not None:
                log.info(f'Scores: id={scores.state.get("my_id")} {scores.score_by_punter}')
                loops[i] = None


def main():
    '''Please don't change predicates or bots, this is a smoke test'''
    from production.utils import config_logging
    config_logging()
    log.setLevel(logging.DEBUG)

    # check_FirstMoveBot()
    check_FirstMove_and_CppBot()


if __name__ == '__main__':
    main()
