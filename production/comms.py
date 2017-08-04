import logging; log = logging.getLogger(__name__)

import json
import socket
from copy import deepcopy

from production.scraper import wait_for_game
from production.bot_interface import Bot
from production import json_format as jf


class CommsException(Exception):
    pass


class OnlineConnection:
    def __init__(self, server, port):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((server, port))


    def read(self):
        data = self.socket.recv(4096)
        if not data:
            raise CommsException('Server unexpectedly closed connection')
        return data


    def write(self, data):
        self.socket.sendall(data)
    

class ColonCodec:
    'Handles n:json packet parsing and composing'

    def __init__(self, connection):
        self.connection = connection
        self.buf = bytearray()
    

    def send(self, req):
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
            buf.extend(self.connection.read())

        msg_len = int(buf[:colon_pos].decode())
        assert 0 <= msg_len < 1_000_000_000

        msg_end = colon_pos + 1 + msg_len
        while len(buf) < msg_end:
            buf.extend(self.connection.read())
        
        res = json.loads(buf[colon_pos + 1 : msg_end])
        del buf[:msg_end]
        log.debug(f'recv({res})')
        return res


def online_mainloop(conn, name: str, bot: Bot):
    conn = ColonCodec(conn)
    # handshake
    conn.send({'me': name})
    res = conn.recv()
    assert res['you'] == name

    # setup
    req = conn.recv()
    res = bot.setup(jf.parse_setup_request(req))
    state = deepcopy(res.state)
    conn.send({'ready': res.ready})

    while True:
        req = conn.recv()
        timeout = req.get('timeout')
        if timeout is not None:
            log.warning('OMG timeout:', timeout)
            continue

        req['state'] = state
        if 'stop' in req:
            log.warning('game ended')
            return jf.parse_score_request(req)

        res = bot.gameplay(jf.parse_gameplay_request(req))
        state = res.state
        conn.send(jf.format_gameplay_response(res))


def main():
    logging.basicConfig(level=logging.INFO)
    log.setLevel(logging.DEBUG)
    from production.dumb_bots import FirstMoveBot
    bot = FirstMoveBot()

    game = wait_for_game(lambda g: 'abject failure' not in g.punters)
    log.info(f'Joining {game}')
    conn = OnlineConnection('punter.inf.ed.ac.uk', game.port)
    log.info('Scores:', online_mainloop(conn, 'tbd tbd', bot))


if __name__ == '__main__':
    main()