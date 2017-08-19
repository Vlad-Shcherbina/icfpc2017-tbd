import production.server.serverloop as server
import production.server.matchmaker as matchmaker
from production.botscript import run_bot

import time
from random import random, randrange


def random_player():
    return matchmaker.PlayerStats(name=f'zzz_{randrange(100)}',
                                  games=randrange(1000),
                                  mu=random(),
                                  sigma=random() / 10,
                                  large=randrange(1000),
                                  medium=randrange(1000),
                                  small=randrange(1000),
                                  futures=randrange(1000),
                                  options=randrange(1000),
                                  splurges=randrange(1000),
                                  opponents={ 'zzz_nowhere_man' : randrange(100)})

base_player = matchmaker.PlayerStats('zzz_', 50, 0.8, 0.05, 10, 20, 20, 10, 10, 10, {})
players = [base_player._replace(name='zzz_julie', opponents={'zzz_smb':5, 'zzz_meee':10}),
           base_player._replace(name='zzz_meee', games=40, opponents={'zzz_julie':10}),
           base_player._replace(name='zzz_smb', games=100, opponents={'zzz_julie':5, 'zzz_howie':15}),
           base_player._replace(name='zzz_howie', opponents={'zzz_smb':15, 'zzz_nevermore':4}),
           base_player._replace(name='zzz_nevermore', games=5, opponents={'zzz_howie':4})]


def test_players_priorities():
    connected = [matchmaker.ConnInfo(players[0], time.time() + 30),
                 matchmaker.ConnInfo(players[1], time.time() + 10),
                 matchmaker.ConnInfo(players[0], time.time() - 10)]
    priorities = matchmaker.players_priority(connected)
    assert priorities[0][1] == 2
    assert priorities[1][1] == 1
    assert priorities[2][1] == 0


def test_match():
    # no players is not enough to start match
    assert matchmaker.match([], 1) is None
    
    # enough players to start match right away
    conns = [matchmaker.ConnInfo(random_player(), time.time() + randrange(60))
                        for _ in range(matchmaker.MAX_PLAYERS * 2)]
    assert matchmaker.match(conns, 1)

    # players passed deadlines and should get game right away.
    conns = [matchmaker.ConnInfo(random_player(), time.time() - randrange(60))
                        for _ in range(matchmaker.MAX_PLAYERS)]
    assert matchmaker.match(conns, 1)


if __name__ == '__main__':
    test_match()
