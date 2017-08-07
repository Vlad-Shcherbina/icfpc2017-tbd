import logging; log = logging.getLogger(__name__)
import os

import sys, json, socket, typing, time, functools
from copy import deepcopy

from production import scraper
from production import bot_interface as bi
from production import json_format as jf
from production import match_history


class CommsException(Exception):
    pass


class OnlineConnection:
    def __init__(self, host, port):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((host, port))


    def read(self):
        data = self.socket.recv(4096)
        if not data:
            raise CommsException('Server unexpectedly closed connection')
        return data


    def write(self, data):
        self.socket.sendall(data)


class RedirectToStderr:
    def __init__(self, stderr):
        self.stderr = stderr

    def write(self, data):
        self.stderr.write(data.decode())

    def flush(self):
        self.stderr.flush()


class OfflineConnection:
    def __init__(self):
        self.stdout = sys.stdout
        sys.stdout = RedirectToStderr(sys.stderr)
        self.stdin = os.fdopen(sys.stdin.fileno(), 'rb', buffering=0)
        # really paranoid people would also dup2 stdout elsewhere and replace 1st descriptor

    def read(self):
        data = self.stdin.read(4096)
        if not data:
            raise CommsException('Server unexpectedly closed connection')
        return data

    def write(self, data):
        self.stdout.buffer.write(data)
        self.stdout.buffer.flush()


class ColonCodec:
    'Handles n:json packet parsing and composing'

    def __init__(self, connection):
        self.connection = connection
        self.buf = bytearray()
        self.capturelog = []


    def send(self, req):
        self.capturelog.append(req)
        s = json.dumps(req).encode()
        s = b'%d:%b' % (len(s), s)
        log.debug(f'send({s})')
        self.connection.write(s)


    def recv(self):
        buf = self.buf

        while True:
            colon_pos = buf.find(b':')
            if colon_pos >= 0:
                break
            if len(buf) > 8:
                raise CommsException('Buffer length part too long')
            buf.extend(self.connection.read())

        msg_len = int(buf[:colon_pos].decode())
        assert 0 <= msg_len < 1_000_000_000

        msg_end = colon_pos + 1 + msg_len
        while len(buf) < msg_end:
            buf.extend(self.connection.read())

        res = json.loads(buf[colon_pos + 1 : msg_end])
        del buf[:msg_end]
        log.debug(f'recv({res})')
        self.capturelog.append(res)
        return res


class Capturer:
    'captures messages before passing them to codec'
    def __init__(self, codec):
        self.codec = codec
        self.capturelog = []
        self.request_timer = None


    def send(self, req):
        if self.request_timer is not None:
            req['debug_request_timer'] = time.perf_counter() - self.request_timer
            self.request_timer = None
        self.capturelog.append(deepcopy(req))
        self.codec.send(req)


    def recv(self):
        res = self.codec.recv()
        self.capturelog.append(deepcopy(res))
        self.request_timer = time.perf_counter()
        return res


def handshake(conn, name):
    conn.send({'me': name})
    res = conn.recv()
    assert res['you'] == name


def offline_mainloop(name: str, bot: bi.Bot):
    'Do not touch'
    conn = ColonCodec(OfflineConnection())
    handshake(conn, name)

    while True:
        req = jf.parse_any_request(conn.recv())

        if isinstance(req, bi.SetupRequest):
            conn.send(jf.format_setup_response(bot.setup(req)))
        elif isinstance(req, bi.GameplayRequest):
            conn.send(jf.format_gameplay_response(bot.gameplay(req)))
        elif isinstance(req, bi.ScoreRequest):
            break
        else:
            assert False, f'wtf is this {req}'


class OnlineTransport:
    '''Flat procedural interface suitable for use with offline bots.

    There's no OfflineTransport btw'''


    def __init__(self, host, port, name, on_comms_cb=lambda msg: msg):
        self.name = name
        self.conn = Capturer(ColonCodec(OnlineConnection(host, port)))
        self.on_comms_cb = on_comms_cb
        handshake(self.conn, name)


    def get_setup(self) -> bi.SetupRequest:
        req = self.conn.recv()
        return self.on_comms_cb(jf.parse_setup_request(req))


    def send_setup_response(self, response: bi.SetupResponse):
        response = self.on_comms_cb(response)
        self.state = deepcopy(response.state)
        response = response._replace(state=None)
        self.conn.send(jf.format_setup_response(response))


    def get_gameplay(self) -> typing.Union[bi.GameplayRequest, bi.ScoreRequest]:
        while True:
            req = self.conn.recv()
            timeout = req.get('timeout')
            if timeout is not None:
                log.warning('OMG timeout:', timeout)
            else:
                break

        req['state'] = self.state
        if 'stop' in req:
            log.warning('game ended')
            return self.on_comms_cb(jf.parse_score_request(req))
        else:
            return self.on_comms_cb(jf.parse_gameplay_request(req))


    def send_gameplay_response(self, response: bi.GameplayResponse):
        response = self.on_comms_cb(response)
        self.state = deepcopy(response.state)
        response = response._replace(state=None)
        self.conn.send(jf.format_gameplay_response(response))


def online_mainloop_pseudoasync(host, port, name: str, bot: bi.Bot, on_comms_cb=lambda msg: msg, game=None):
    'Copypaste and augment as you wish'
    tr = OnlineTransport(host, port, name, on_comms_cb)
    yield # after handshake

    req = tr.get_setup()
    log.warning('game started')
    res = bot.setup(req)
    tr.send_setup_response(res)

    while True:
        yield # after setup and every move
        req = tr.get_gameplay()
        if isinstance(req, bi.ScoreRequest):
            break
        res = bot.gameplay(req)
        tr.send_gameplay_response(res)

    match_history.submit_replay(name, game, tr.conn.capturelog)
    yield req


@functools.wraps(online_mainloop_pseudoasync)
def online_mainloop(*args, **kwargs):
    for req in online_mainloop_pseudoasync(*args, **kwargs):
        pass
    return req


def main():
    '''Please don't change predicates or bots, this is a smoke test'''
    from production.utils import config_logging
    config_logging()

    log.setLevel(logging.DEBUG)
    from production.dumb_bots import FirstMoveBot
    bot = FirstMoveBot()

#    game = scraper.wait_for_game(predicate=scraper.only_easy_eagers_p,
#                                 extensions={'futures', 'options'},
#                                 patience=300)
    game = scraper.wait_for_game(extensions={'futures', 'splurges', 'options'}, patience=1)
    log.info(f'Joining {game}')
    scores = online_mainloop('punter.inf.ed.ac.uk', game.port, 'cbd', bot, game=game)
    log.info(f'Scores: id={scores.state.get("my_id")} {scores.score_by_punter}')


if __name__ == '__main__':
    main()
