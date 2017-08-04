import logging; log = logging.getLogger(__name__)

import json
import socket


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

    
    def get_setup(self):
        res = self.recv()
        self.id = res['punter']
        self.send({'ready': self.id})

    
    def get_moves(self):
        return self.recv()


    def send_move(self, move):
        self.send(move)


def main():
    logging.basicConfig(level=logging.INFO)
    log.setLevel(logging.DEBUG)

    con = Connection('punter.inf.ed.ac.uk', 9008, 'tbd tbd')
    con.perform_handshake()
    setup = con.get_setup()
    while True:
        moves = con.get_moves()
        if 'stop' in moves:
            break
        con.send_move({'pass' : {'punter': con.id}})


if __name__ == '__main__':
    main()