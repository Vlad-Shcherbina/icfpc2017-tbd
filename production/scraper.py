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
    if 'Game in progress' in status or 'Offline' in status or 'Game finished' in status:
        return None
    elif 'Waiting for punters' in status:
        l = re.split("[\(/\)]", status)
        return (int(l[1]), int(l[2]))
    else:
        assert False, status


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


def wait_for_game(*, patience=1, predicate=lambda g: True, shuffle=True, extensions={}):
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
            if g.punters_max - g.punters_num > patience:
                continue
            return g
        log.warning('no imminent games, waiting...')
        time.sleep(random.random() * 6)


def wait_for_game1(*, punters=1, patience=0, predicate=lambda g: True, shuffle=True, extensions={}):
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
            if (g.punters_max - g.punters_num > patience 
                             or g.punters_max - g.punters_num < punters):
                continue
            return g
        log.warning('no imminent games, waiting...')
        time.sleep(3)


def only_given_port(x):
    def ogp(g: Game):
        return g.port == x
    return ogp

def only_not_blacklisted(xs):
    def onb(g: Game):
        # blackmail 'someone_LDK' along with 'someone'
        return all(name not in p for name in xs for p in g.punters) and (len(g.punters) > 0)
    return onb

def soft_blacklisted(xs, N=None, allowed_blacklisted=1):
    '''Return true if there is no more than allowed number for each name in
    blacklist and no less than N other not 'eager_punter' players.

    With allowed_blacklisted = 0 can serve as blacklist + diverse players.
    '''
    def sb(g: Game):
        for name in xs:
            if sum(1 for p in g.punters if name in p) > allowed_blacklisted: 
                return False
        count = 0
        ext_xs = xs + ['eager punter']
        for p in g.punters:
            if not any((name in p) for name in ext_xs): count += 1
        return count >= N if N is not None else count >= len(g.punters) / 2
    return sb

# Use this to be nice to others
def only_eagers_p(g: Game):
    return all(p == 'eager punter' for p in g.punters)

# Use this to be nice to others and your dumb bot
def only_easy_eagers_p(g: Game):
    return only_eagers_p(g) and only_easy_maps(g)

def only_hard_eagers_p(g: Game):
    return only_eagers_p(g) and only_hard_maps(g)

def only_easy_maps(g: Game):
    return g.map_name in ('sample.json', 'lambda.json', 'Sierpinski-triangle.json')

def only_hard_maps(g: Game):
    return g.map_name not in ('sample.json', 'lambda.json', 'Sierpinski-triangle.json')    

def diverse_players(g:Game):
    return sum(p == 'eager punter' for p in g.punters) < len(g.punters) / 2


def main():
    import getopt
    import random
    def usage():
        print("Prints port number for a game")
        print("Options:")
        print("\t-s, --slots\t number of free slots in a game")
        print("\t-r, --random\t pick random port, not the first one")

    try:
        opts, _ = getopt.getopt(sys.argv[1:], 's:r', ['slots=', "random"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    if not opts:
        usage()
        sys.exit(1)
    slots: int = 0
    is_random: bool = False

    for opt, arg in opts:
        if opt in ('-s', '--slots'):
            slots = int(arg)
        elif opt in ('-r', "--random"):
            is_random = True
        else:
            usage()
            sys.exit(2)
    gs: list[Game] = [g for g in list(games()) if only_easy_eagers_p(g) and g.punters_max - g.punters_num == slots]
    if gs:
        g = random.choice(gs) if is_random else gs[0]
        print(g.port)
    else:
        print("None")
        sys.exit(4)

if __name__ == "__main__":
    main()
