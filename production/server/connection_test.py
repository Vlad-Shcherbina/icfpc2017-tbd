import socket
import time
import threading

import pytest

from connection import NetworkConnection, Timeout, Dead


@pytest.fixture
def connect():
    address = '127.0.0.1', 42424
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind(address)
    server.listen(1)

    client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client.connect(address)

    conn, addr = server.accept()
    conn = NetworkConnection(conn)

    yield conn, client

    conn.close()
    server.close()


def test_simple(connect):
    conn, client = connect

    client.sendall(b'8:{"hi":1}')
    msg = conn.receive(deadline=time.time() + 1)
    assert msg == dict(hi=1)

    conn.send(dict(q=2))
    data = client.recv(4096)
    assert data == b'8:{"q": 2}'

    assert conn.alive


def test_timeout(connect):
    conn, client = connect

    msg = conn.receive(deadline=time.time() + 0.5)
    assert msg == Timeout()


def test_skip_receive(connect):
    conn, client = connect

    msg = conn.receive(deadline=time.time() + 0.5)
    assert msg == Timeout()
    client.send(b'stuff')

    conn.skip_received()

    conn.send({})
    data = client.recv(4096)
    assert data == b'2:{}'
    client.send(b'2:{}')

    msg = conn.receive(deadline=time.time() + 1)
    assert msg == {}


def test_recheck(connect):
    conn, client = connect

    conn.recheck()
    assert conn.alive

    client.send(b'2:{}')
    conn.recheck()
    assert conn.alive

    msg = conn.receive(deadline=time.time() + 1)
    assert msg == {}

    client.close()
    conn.recheck()
    assert conn.reason_dead == 'peer disconnected'

    conn.recheck()
    assert conn.reason_dead == 'peer disconnected'


def test_kick(connect):
    conn, client = connect

    client.settimeout(1e-3)
    try:
        client.recv(4096)
        assert False
    except socket.timeout:
        pass

    conn.kick('test')

    data = client.recv(4096)
    assert data == b'kicked (test)'

    conn.send({'hi': 1})
    msg = conn.receive(deadline=time.time() + 1)
    assert msg == Dead(reason='kicked (test)')
    assert not conn.alive

    conn.kick('again')

    msg = conn.receive(deadline=time.time() + 1)
    assert msg == Dead(reason='kicked (test)')
    assert not conn.alive


def test_length_no_colon(connect):
    conn, client = connect

    client.send(b'1234567890')
    msg = conn.receive(deadline=time.time() + 1)
    assert msg == Dead(reason='message length is too long')
    assert not conn.alive


def test_length_too_long(connect):
    conn, client = connect

    client.send(b'1234567890:{}')
    msg = conn.receive(deadline=time.time() + 1)
    assert msg == Dead(reason='message length is too long')
    assert not conn.alive


def test_length_not_number(connect):
    conn, client = connect

    client.send(b'zzz:{}')
    msg = conn.receive(deadline=time.time() + 1)
    assert msg == Dead(reason='message length is not a number')
    assert not conn.alive


def test_length_not_valid_json(connect):
    conn, client = connect

    client.send(b'3:zzz')
    msg = conn.receive(deadline=time.time() + 1)
    assert msg == Dead(reason='not a valid JSON')
    assert not conn.alive


def test_length_not_dict(connect):
    conn, client = connect

    client.send(b'1:1')
    msg = conn.receive(deadline=time.time() + 1)
    assert msg == Dead(reason='not a valid move')  # not a dict
    assert not conn.alive


if __name__ == '__main__':
    import sys, logging, pytest
    logging.basicConfig(level=logging.DEBUG)
    pytest.main([__file__] + sys.argv[1:])
