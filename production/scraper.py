from bs4 import BeautifulSoup
import re
import sys
import typing
import urllib.request


class Game(typing.NamedTuple):
    punters_num: int
    punters_max: int
    port: int
    map_name: str
    punters: typing.List[str]


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
            print(tds)
            if status:
                yield Game(status[0], status[1], int(tds[3].string), tds[4].string, (tds[1].string or '').split(','))


def wait_for_game(predicate=lambda g: True):
    names = [name] if isinstance(name, str) else name # None or list
    while True:
        for g in games():
            if not predicate(g):
                continue
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
