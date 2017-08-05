from bs4 import BeautifulSoup
import re
import sys
import time
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


def wait_for_game(*, predicate=lambda g: True, shuffle=True, extensions={}):
    while True:
        gs = list(games())
        if shuffle:
            random.shuffle(gs)
        for g in gs:
            if not predicate(g):
                continue
            if any(extension not in extensions for extension in g.extensions):
                continue
            log.info(g)
            if g.punters_max - g.punters_num != 1:
                continue
            return g
        log.info('no imminent games, waiting...')
        time.sleep(3)


def wait_for_game1(*, punters=1, predicate=lambda g: True, shuffle=True, extensions={}):
    while True:
        gs = list(games())
        if shuffle:
            random.shuffle(gs)
        for g in gs:
            if not predicate(g):
                continue
            if any(extension not in extensions for extension in g.extensions):
                continue
            log.info(g)
            if g.punters_max - g.punters_num != punters:
                continue
            return g
        log.info('no imminent games, waiting...')
        time.sleep(3)

def only_given_port(x):
    def ogp(g: Game):
        return g.port == x
    return ogp

def only_not_blacklisted(xs):
    def onb(g: Game):
        return all(p not in xs for p in g.punters) and (len(g.punters) > 0)
    return onb

# Use this to be nice to others
def only_eagers_p(g: Game):
    return all(p == 'eager punter' for p in g.punters)


# Use this to be nice to others and your dumb bot
def only_easy_eagers_p(g: Game):
    return only_eagers_p(g) and g.map_name in ('sample.json', 'lambda.json', 'Sierpinski-triangle.json')


def main():
    import getopt
    def usage():
        print("Prints port number for a game")
        print("Options:")
        print("\t-s, --slots\t number of free slots in a game")
        print("\t-e, --easy\t 'true' or 'false' for easy game (default) ")

    try:
        opts, _ = getopt.getopt(sys.argv[1:], 's:e', ['slots=', 'easy='])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    if not opts:
        usage()
        sys.exit(1)
    easy: bool = True
    slots: int = 0

    for opt, arg in opts:
        if opt in ('-s', '--slots'):
            slots = int(arg)
        elif opt in ('-e', '--easy'):
            easy = bool(arg == 'true')
        else:
            usage()
            sys.exit(2)
    pred = only_easy_eagers_p if easy else (lambda x: not only_easy_eagers_p(x))
    gs: list[Game] = [g for g in list(games()) if pred(g) and g.punters_max - g.punters_num == slots]
    print(gs[0].port if gs else "None")

if __name__ == "__main__":
    main()
