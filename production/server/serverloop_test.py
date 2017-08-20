import production.server.serverloop as server
from production.server.matchmaker import makematch, MAX_PLAYERS, players_priority
from production.server.server_interface import *
from production.botscript import run_bot

import time
from random import random, randrange


def random_player():
    return PlayerStats(ID=randrange(100),
                       name=f'zzz_{randrange(100)}',
                       games=randrange(1000),
                       mu=random(),
                       sigma=random() / 10,
                       )

base_player = PlayerStats(0, 'zzz_', 50, 0.8, 0.05)
players = [base_player._replace(name='zzz_julie'),
           base_player._replace(ID=1, name='zzz_meee', games=40),
           base_player._replace(ID=2, name='zzz_smb', games=100),
           base_player._replace(ID=3, name='zzz_yahoo'),
           base_player._replace(ID=4, name='zzz_nevermore', games=5)
          ]

def test_players_priorities():
    connected = [WaitingPlayer(players[0], time.time() + 30),
                 WaitingPlayer(players[1], time.time() + 10),
                 WaitingPlayer(players[0], time.time() - 10)]
    priorities = players_priority(connected)
    assert priorities[0][1] == 2
    assert priorities[1][1] == 1
    assert priorities[2][1] == 0


def test_match():
    # no players is not enough to start match
    assert makematch([], 1) is None
    
    # enough players to start match right away
    conns = [WaitingPlayer(random_player(), time.time() + randrange(60))
                        for _ in range(MAX_PLAYERS * 2)]
    assert makematch(conns, 1)

    # players passed deadlines and should get game right away.
    conns = [WaitingPlayer(random_player(), time.time() - randrange(60))
                        for _ in range(MAX_PLAYERS)]
    assert makematch(conns, 1)


if __name__ == '__main__':
    test_match()
