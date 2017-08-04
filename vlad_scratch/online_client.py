try:
    import hintcheck
    hintcheck.monkey_patch_named_tuple_constructors()
except ImportError:
    pass

import logging; log = logging.getLogger(__name__)

import json
import time
import socket
import random

from production import dumb_bots
from production import json_format
from production.bot_interface import *
from production import scraper
from production import visualization
from production import utils


class CommsException(Exception):
    pass

class Connection:
    def __init__(self, server, port, name):
        self.name = name
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((server, port))
        self.buf = bytearray()


    def send(self, req):
        s = json.dumps(req).encode()
        s = b'%d:%b' % (len(s), s)
        log.debug(f'{self.name} sending {s}')
        self.socket.sendall(s)


    def recv(self):
        buf = self.buf
        def recvsome():
            data = self.socket.recv(4096)
            if not data:
                raise CommsException('Server unexpectedly closed connection')
            buf.extend(data)

        while True:
            colon_pos = buf.find(b':')
            if colon_pos >= 0:
                break
            recvsome()

        msg_len = int(buf[:colon_pos].decode())
        # basic sanity check, 10Mb should be enough for everyone
        assert 0 <= msg_len <= 10_000_000

        msg_end = colon_pos + 1 + msg_len
        while len(buf) < msg_end:
            recvsome()

        res = json.loads(buf[colon_pos + 1 : msg_end])
        del buf[:msg_end]
        log.debug(f'{self.name} got {res}')
        return res


    def perform_handshake(self):
        self.send({'me': self.name})
        res = self.recv()
        assert res['you'] == self.name


    def get_request(self):
        return self.recv()


    def send_move(self, move):
        self.send(move)


def main():
    try:
        import hintcheck
        hintcheck.hintcheck_all_functions()
    except ImportError:
        pass

    logging.basicConfig(
        level=logging.INFO,
        format='%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')
    log.setLevel(logging.DEBUG)

    while True:
        games = list(scraper.games())
        random.shuffle(games)
        for game in games:
            log.info(game)
            if game.punters_num + 1 == game.punters_max:
                log.info('this game is about to begin!')
                break
        else:  # no break
            log.info('no imminent games, waiting...')
            time.sleep(5)
            continue
        break

    bot = dumb_bots.FirstMoveBot()

    con = Connection('punter.inf.ed.ac.uk', game.port, 'tbd tbd')
    print('conn')
    con.perform_handshake()

    turn_number = 0

    old_state = None
    while True:
        req = con.get_request()
        if old_state is not None:
            assert 'state' not in req
            req['state'] = old_state

        log.info(req)
        req = json_format.parse_any_request(req)

        if isinstance(req, SetupRequest):
            resp = bot.setup(req)
            resp = json_format.format_setup_response(resp)
        elif isinstance(req, GameplayRequest):
            resp = bot.gameplay(req)
            resp = json_format.format_gameplay_response(resp)
        elif isinstance(req, ScoreRequest):
            bot.score(req)
            log.info(f'scores = {req.score_by_punter}')
            break
        else:
            assert False

        old_state = resp.pop('state')
        log.info(f'state = {old_state}')

        con.send_move(resp)

        vis = visualization.Visualization(300, 300)
        vis.draw_background()
        map_ = json_format.parse_map(old_state['map'])
        vis.draw_map(map_)
        for move in old_state['all_past_moves']:
            move = json_format.parse_move(move)
            if isinstance(move, ClaimMove):
                color = (
                    move.punter % 2 * 255,
                    move.punter // 2 % 2 * 255,
                    move.punter // 4 % 2 * 255)
                vis.draw_edge(
                    map_.site_coords[move.source],
                    map_.site_coords[move.target],
                    color=color)

        im = vis.get_image()
        im.save(utils.project_root() / 'outputs' / f'{turn_number:04d}.png')

        turn_number += 1


if __name__ == '__main__':
    main()
