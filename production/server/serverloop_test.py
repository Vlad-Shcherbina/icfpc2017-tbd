import production.server.serverloop as server
from production.server.matchmaker import match, MAX_PLAYERS
from production.server.server_interface import *
from production.botscript import run_bot

import time
from random import random, randrange


def random_player():
    return PlayerStats(name=f'zzz_{randrange(100)}',
                       games=randrange(1000),
                       mu=random(),
                       sigma=random() / 10,
                       )

# base_player = matchmaker.PlayerStats('zzz_', 50, 0.8, 0.05, 10, 20, 20, 10, 10, 10, {})
base_player = PlayerStats('zzz_', 50, 0.8, 0.05)
players = [base_player._replace(name='zzz_julie'), #, opponents={'zzz_smb':5, 'zzz_meee':10}),
           base_player._replace(name='zzz_meee', games=40), # opponents={'zzz_julie':10}),
           base_player._replace(name='zzz_smb', games=100), # opponents={'zzz_julie':5, 'zzz_howie':15}),
           base_player._replace(name='zzz_yahoo'), # opponents={'zzz_smb':15, 'zzz_nevermore':4}),
           base_player._replace(name='zzz_nevermore', games=5) #, opponents={'zzz_howie':4})]
          ]

def test_players_priorities():
    connected = [ConnInfo(players[0], time.time() + 30),
                 ConnInfo(players[1], time.time() + 10),
                 ConnInfo(players[0], time.time() - 10)]
    priorities = players_priority(connected)
    assert priorities[0][1] == 2
    assert priorities[1][1] == 1
    assert priorities[2][1] == 0


def test_match():
    # no players is not enough to start match
    assert match([], 1) is None
    
    # enough players to start match right away
    conns = [ConnInfo(random_player(), time.time() + randrange(60))
                        for _ in range(MAX_PLAYERS * 2)]
    assert match(conns, 1)

    # players passed deadlines and should get game right away.
    conns = [ConnInfo(random_player(), time.time() - randrange(60))
                        for _ in range(MAX_PLAYERS)]
    assert match(conns, 1)


if __name__ == '__main__':
    test_match()
