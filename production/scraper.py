from bs4 import BeautifulSoup
import re
import sys
import urllib.request


class Game(object):
    def __init__(self, punters_num, punters_max, port, map_name):
        self.punters_num = punters_num
        self.punters_max = punters_max
        self.port = port
        self.map_name = map_name

    def __repr__(self):
        return f"Port={self.port}, punters_max={self.punters_max}, punters_num={self.punters_num}, map_name={self.map_name}."


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
                yield Game(status[0], status[1], int(tds[2].string), tds[3].string)


def main():
    for game in games():
        print(game)

if __name__ == "__main__":
    main()
