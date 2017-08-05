import logging; log = logging.getLogger(__name__)

import json, socket, typing
from copy import deepcopy

import production.scraper as scraper
from production import bot_interface as bi
from production import json_format as jf


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
        return res


def handshake(conn, name):
    conn.send({'me': name})
    res = conn.recv()
    assert res['you'] == name


class OnlineTransport:
    '''Flat procedural interface suitable for use with offline bots.abs
    
    There's no OfflineTransport btw'''

    
    def __init__(self, host, port, name):
        self.name = name
        self.conn = ColonCodec(OnlineConnection(host, port))
        handshake(self.conn, name)
    
    
    def get_setup(self) -> bi.SetupRequest:
        req = self.conn.recv()
        return jf.parse_setup_request(req)

    
    def send_setup_response(self, response: bi.SetupResponse):
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
            return jf.parse_score_request(req)
        else:
            return jf.parse_gameplay_request(req)

    def send_gameplay_response(self, response: bi.GameplayResponse):
        self.state = deepcopy(response.state)
        response = response._replace(state=None)
        self.conn.send(jf.format_gameplay_response(response))


def online_mainloop(host, port, name: str, bot: bi.Bot):
    'Copypaste and augment as you wish'
    tr = OnlineTransport(host, port, name)

    req = tr.get_setup()
    tr.send_setup_response(bot.setup(req))
    
    while True:
        req = tr.get_gameplay()
        if isinstance(req, bi.ScoreRequest):
            return req
        tr.send_gameplay_response(bot.gameplay(req))


def main():
    logging.basicConfig(level=logging.INFO)
    log.setLevel(logging.DEBUG)
    from production.dumb_bots import FirstMoveBot
    bot = FirstMoveBot()

    # be nice to others
    def only_eagers(g: scraper.Game):
        return all(p == 'eager punter' for p in g.punters)

    game = scraper.wait_for_game(predicate=only_eagers) 
    log.info(f'Joining {game}')
    scores = online_mainloop('punter.inf.ed.ac.uk', game.port, 'tbd tbd', bot)
    log.info(f'Scores: {scores}')


if __name__ == '__main__':
    main()