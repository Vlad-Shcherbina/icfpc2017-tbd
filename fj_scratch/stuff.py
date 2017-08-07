import logging; log = logging.getLogger(__name__)
import json
import production
from production.comms import online_mainloop, online_mainloop_pseudoasync
from production import scraper, json_format, visualization, utils
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

def main():
    from production.utils import config_logging
    config_logging()
    log.setLevel(logging.DEBUG)

    v = visualization.Visualization(800, 800)
    d = utils.project_root() / 'maps' / 'official_map_samples' / 'lambda.json'
    m = json_format.parse_map(json.loads(d.read_text()))
    v.draw_map(m)
    v.get_image().save('zzz.png')



if __name__ == '__main__':
    main()
