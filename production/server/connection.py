import socket
import time
import json
import re
from typing import NamedTuple, Union
import logging
logger = logging.getLogger(__name__)


class Timeout(NamedTuple):
    pass


class Dead(NamedTuple):
    reason: str


class NetworkConnection:
    """
    Usage example:

    conn, addr = server_socket.accept()
    conn = NetworkConnection(conn)

    conn.send(handshake_message)
    msg = conn.receive(time.time() + 1)

    # Don't want to discover that the peer disconnected when the game
    # has started and we are sending setup messages already.
    conn.recheck()
    assert conn.alive

    conn.send(...)
    conn.receive(...)
    ...
    conn.kick('10 timeouts in a')
    ...
    conn.send(...)
    conn.receive(...)
    ...
    conn.close()
    """

    def __init__(self, socket):
        self.socket = socket
        self.reason_dead = None
        self.buf = bytearray()

    @property
    def alive(self):
        return not self.reason_dead

    def send(self, msg: dict):
        """Never fails."""

        if not self.alive:
            return

        s = json.dumps(msg).encode()
        s = b'%d:%b' % (len(s), s)
        logger.debug(f'send({s})')

        self.socket.settimeout(0.5)
        try:
            self.socket.sendall(s)
        except socket.timeout:
            self.reason_dead = 'send timed out'
        except OSError:
            logger.exception('send failed')
            self.reason_dead = 'send failed'

    def _receive(self, deadline) -> Union[dict, Dead]:
        buf = self.buf

        while True:
            colon_pos = buf.find(b':')
            if colon_pos >= 0:
                break
            if len(buf) > 9:
                self.reason_dead = 'message length is too long'
                return Dead(reason=self.reason_dead)
            self.socket.settimeout(max(1e-3, deadline - time.time()))
            buf.extend(self.socket.recv(4096))

        if colon_pos > 9:
            self.reason_dead = 'message length is too long'
            return Dead(reason=self.reason_dead)

        if not re.match(b'\d+$', buf[:colon_pos]):
            self.reason_dead = 'message length is not a number'
            return Dead(reason=self.reason_dead)

        msg_len = int(buf[:colon_pos].decode())
        assert 0 <= msg_len < 1_000_000_000

        msg_end = colon_pos + 1 + msg_len
        while len(buf) < msg_end:
            self.socket.settimeout(max(1e-3, deadline - time.time()))
            buf.extend(self.socket.recv(4096))

        try:
            res = json.loads(buf[colon_pos + 1 : msg_end])
        except json.JSONDecodeError:
            self.reason_dead = 'not a valid JSON'
            return Dead(reason=self.reason_dead)

        if not isinstance(res, dict):
            self.reason_dead = 'not a valid move'  # not a dict
            return Dead(reason=self.reason_dead)

        del buf[:msg_end]
        logger.debug(f'recv({res})')

        return res

    def receive(self, deadline) -> Union[dict, Timeout, Dead]:
        if not self.alive:
            return Dead(reason=self.reason_dead)

        try:
            return self._receive(deadline)
        except socket.timeout:
            return Timeout()
        except OSError:
            logger.exception('recv failed')
            self.reason_dead = 'recv failed'
            return Dead(reason=self.reason_dead)

    def skip_received(self):
        if not self.alive:
            return
        del self.buf[:]
        self.socket.settimeout(1e-3)
        try:
            while True:
                self.socket.recv(4096)
        except socket.timeout:
            pass
        except OSError:
            logger.exception('recv failed')
            self.reason_dead = 'recv failed'

    def recheck(self):
        if not self.alive:
            return
        self.socket.settimeout(1e-3)
        try:
            data = self.socket.recv(1)
        except socket.timeout:
            return
        except OSError:
            logger.exception('recheck failed')
            self.reason_dead = 'recheck failed'
        if not data:
            self.reason_dead = 'peer disconnected'
        self.buf.extend(data)

    def kick(self, reason):
        if not self.alive:
            return
        self.reason_dead = f'kicked ({reason})'
        # try:
        #     self.socket.sendall(f'kicked ({reason})'.encode())
        # except socket.timeout:
        #     pass
        # except OSError:
        #     logger.exception('send failed')
        self.socket.close()
        self.socket = None

    def close(self):
        if self.socket is not None:
            self.socket.close()
