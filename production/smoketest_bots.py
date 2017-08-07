import logging; log = logging.getLogger(__name__)
import production
from production.comms import online_mainloop, online_mainloop_pseudoasync
from production import scraper, json_format
from production.dumb_bots import FirstMoveBot
from production.cpp_bot import CppBot

json_format.REPORT_UNKNOWN_FIELDS = True

def check_FirstMoveBot():
    bot = FirstMoveBot()

    def predicate(g: scraper.Game):
        return scraper.only_eagers_p(g) and g.map_name == 'randomMedium.json'

    game = scraper.wait_for_game(predicate=predicate, extensions={'futures', 'splurges', 'options'})

    log.info(f'Joining {game}')
    scores = online_mainloop('punter.inf.ed.ac.uk', game.port, 'tbd testbot', bot, game=game)
    log.info(f'Scores: id={scores.state.get("my_id")} {scores.score_by_punter}')


def check_CppBot():
    bot = CppBot()

    def predicate(g: scraper.Game):
        return scraper.only_eagers_p(g) and g.map_name == 'randomMedium.json'

    game = scraper.wait_for_game(predicate=predicate, extensions={'futures', 'splurges', 'options'})

    log.info(f'Joining {game}')
    scores = online_mainloop('punter.inf.ed.ac.uk', game.port, 'tbd testbot', bot, game=game)
    log.info(f'Scores: id={scores.state.get("my_id")} {scores.score_by_punter}')


def check_FirstMove_and_CppBot(orgy=False):
    bots = [FirstMoveBot(), CppBot()]
    if orgy:
        bots += [FirstMoveBot(), CppBot()]

    def predicate(g: scraper.Game):
        return (g.map_name == ('randomMedium.json' if orgy else 'sample.json') and
                g.punters_num == 0)

    game = scraper.wait_for_game(predicate=predicate, patience=1000, extensions={'futures', 'splurges', 'options'})
    log.info(f'Joining {game}')

    loops = [online_mainloop_pseudoasync('punter.inf.ed.ac.uk', game.port, 'tbd testbot', bot, game=game) for bot in bots]
    while not all(loop is None for loop in loops):
        for i, loop in enumerate(loops):
            if loop is None:
                continue
            scores = next(loop)
            if scores is not None:
                log.info(f'Scores: id={scores.state.get("my_id")} {scores.score_by_punter}')
                loops[i] = None


def check_FirstMove_and_CppBot_orgy():
    return check_FirstMove_and_CppBot(True)


def main():
    from production.utils import config_logging
    config_logging()
    log.setLevel(logging.DEBUG)

    for check in (check_FirstMove_and_CppBot_orgy,): # check_FirstMoveBot, check_CppBot, check_FirstMove_and_CppBot,
        check()
        log.warn('')
        log.warn('-' * 50)
        log.warn('')


if __name__ == '__main__':
    main()