from bs4 import BeautifulSoup
import re
import sys
import typing
import urllib.request
import random

import logging; log = logging.getLogger(__name__)


class Game(typing.NamedTuple):
    punters_num: int
    punters_max: int
    port: int
    map_name: str
    punters: typing.List[str]
    extensions: typing.List[str]


def __parse_status(status):
    if 'Game in progress' in status or 'Offline' in status:
        return None
    elif 'Waiting for punters' in status:
        l = re.split("[\(/\)]", status)
        return (int(l[1]), int(l[2]))
    else:
        sys.error('Unknown status: ' + status)


def games():
    with urllib.request.urlopen('http://punter.inf.ed.ac.uk/status.html') as response:
        html = response.read()
        soup = BeautifulSoup(html, 'html.parser')
        trs = soup.body.table.find_all('tr')
        for tr in trs[1:]:
            tds = tr.contents
            status = __parse_status(tds[0].string)
            if status:
                punters = tds[1].string
                if punters:
                    punters = [p.strip() for p in punters.split(',')]
                else:
                    punters = []

                extensions = tds[2].string
                if extensions:
                    extensions = [p.strip() for p in extensions.split(',')]
                else:
                    extensions = []

                yield Game(
                    punters_num=status[0],
                    punters_max=status[1],
                    port=int(tds[3].string),
                    map_name=tds[4].string,
                    punters=punters,
                    extensions=extensions)


def wait_for_game(*, predicate=lambda g: True, shuffle=True):
    while True:
        gs = list(games())
        if shuffle:
            random.shuffle(gs)
        for g in gs:
            if not predicate(g):
                continue
            log.info(g)
            if g.punters_max - g.punters_num != 1:
                continue
            return g
        log.info('no imminent games, waiting...')
        time.sleep(3)


def main():
    for game in games():
        print(game)

if __name__ == "__main__":
    main()
