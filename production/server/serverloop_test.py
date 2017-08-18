import production.server.serverloop as server
import production.server.matchmaker as matchmaker
import time

def test_players_priorities():
    player1 = matchmaker.PlayerStats(name='julie',
                                     games=50,
                                     mu=0.8,
                                     sigma=0.05,
                                     large=10,
                                     medium=20,
                                     small=20,
                                     futures=10,
                                     options=10,
                                     splurges=10,
                                     opponents={ 'smb':5, 'meee':10 })
    player2 = player1._replace(name='meee', games=40, opponents={ 'julie':10 })
    connected = [matchmaker.ConnInfo(player1, time.time() + 30),
                 matchmaker.ConnInfo(player2, time.time() + 10),
                 matchmaker.ConnInfo(player1, time.time() - 10)]
    priorities = matchmaker.players_priority(connected)
    assert priorities[0][1] == 2
    assert priorities[1][1] == 1
    assert priorities[2][1] == 0


if __name__ == '__main__':
    test_players_priorities()
