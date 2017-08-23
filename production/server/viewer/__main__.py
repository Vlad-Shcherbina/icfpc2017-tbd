# hack to placate the restarter
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from typing import NamedTuple, List
import io
import os
import json
import functools
import webbrowser

import flask
import jinja2 as jnj

from production import json_format
from production.server.db_connection import connect_to_db
#from production.server.server_interface import *

PORT = 5017
GAMES_RANGE = 100

app = flask.Flask(
    'webviewer',
    root_path=os.path.dirname(__file__))


class PlayerBaseInfo(NamedTuple):
    ID: int
    name: str
    games: int
    rating: int


class GameBaseInfo(NamedTuple):
    ID: int
    mapname: str
    settings: str
    players: str
    time: str


class PlayerPerfomanceInfo:
    def __init__(self, playerID):
        self.ID = playerID
        self.name = ''
        self.place = -1
        self.score = 0
        self.futures = []
        self.moves = {'pass' : 0, 'claim' : 0, 'splurge' : 0, 'option' : 0 }
        self.averagetime = 0.0
        self.forcedpasses = 0
        self.illegalmoves = 0
        self.timeouts = 0
        self.zombiesince = '--'
        self.zombiereason = ''

        self.sumtime = 0.0


# -------------------------- PAGE RENDERING -----------------------------#

@app.route('/')
def index():
    return leaderboard()


@app.route('/leaderboard')
def leaderboard():
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT id, name, rating_mu, rating_sigma 
                          FROM icfpc2017_players;''')
                        # WHERE NOT name LIKE "zzz_%"
        playerlist = []
        for row in cursor.fetchall():
            playerID, name, rating_mu, rating_sigma = row
            rating = int(rating_mu - 3 * rating_sigma + 0.5)
            cursor.execute('''SELECT id FROM icfpc2017_participation 
                              WHERE player_id=%s;''', 
                              (playerID,))
            games = cursor.rowcount
            playerlist.append(PlayerBaseInfo(ID=playerID, name=name, rating=rating, games=games))
        playerlist.sort(key=lambda x: x.rating, reverse=True)
    return flask.render_template('leaderboard.html', playerlist=playerlist)
    dbconn.close()


@app.route('/lastgames/<page>')
def lastgames(page):
    page = int(page)
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT id, mapname, futures, options, splurges, timefinish
                          FROM icfpc2017_games WHERE status='finished'
                          ORDER BY timefinish DESC;''')
        if cursor.rowcount < page * GAMES_RANGE:
            page = (cursor.rowcount - 1) // GAMES_RANGE
        cursor.scroll(page * GAMES_RANGE)

        gamelist = []
        for row in cursor.fetchmany(GAMES_RANGE):
            gameID, mapname, futures, options, splurges, timefinish = row
            settings = (('futures, ' if futures else '')
                      + ('options, ' if options else '')
                      + ('splurges, ' if splurges else ''))[:-2]
            cursor.execute('''SELECT player_id FROM icfpc2017_participation
                              WHERE game_id=%s ORDER BY score DESC;''',
                              (gameID,))
            playerlist = []
            for playerID in cursor.fetchall():
                cursor.execute('SELECT name FROM icfpc2017_players WHERE id=%s', (playerID,))
                playerlist.append(cursor.fetchone()[0])
            players = ', '.join(playerlist)
            gamelist.append(GameBaseInfo(ID=gameID, 
                                         mapname=mapname, 
                                         settings=settings, 
                                         players=players,
                                         time=timefinish.replace(microsecond=0)))
    dbconn.close()
    return flask.render_template('lastgames.html', gamelist=gamelist)


@app.route('/instructions')
def instructions():
    return flask.render_template('instructions.html')


@app.route('/playerstatistics/<playerID>')
def playerstatistics(playerID):
    playerID = int(playerID)
    return flask.render_template('404.html')


@app.route('/gamestatistics/<gameID>')
def gamestatistics(gameID):
    gameID = int(gameID)
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT mapname, futures, options, splurges,
                          timestart, timefinish FROM icfpc2017_games
                          WHERE id=%s;''', (gameID, ))
        if not cursor.rowcount:
            return flask.render_template('404.html')
        mapname, futures, options, splurges, timestart, timefinish = cursor.fetchone()
        settings = (('futures, ' if futures else '')
                  + ('options, ' if options else '')
                  + ('splurges, ' if splurges else ''))[:-2]
        
        cursor.execute('''SELECT replay FROM icfpc2017_replays
                          WHERE id=%s;''', (gameID, ))
        assert cursor.rowcount == 1
        replay = json.loads(bytes(cursor.fetchone()[0]))

        playerIDs = []
        playerscores = []
        cursor.execute('''SELECT player_id, score FROM icfpc2017_participation
                          WHERE game_id=%s ORDER BY player_order;''', (gameID, ))
        for row in cursor.fetchall():
            playerIDs.append(row[0])
            playerscores.append(row[1])
        
        playerlist = _playerperfomances(replay, playerIDs)
        assert len(playerIDs) == len(playerlist)
        assert all(p.score == ps for p, ps in zip(playerlist, playerscores))
    
    dbconn.close()
    return flask.render_template('gamestatistics.html',
                                 gameID=gameID,
                                 mapname=mapname,
                                 settings=settings,
                                 movecount=len(replay['moves']),
                                 timespan=timefinish-timestart,
                                 playerperfs=playerlist,
                                 replay=replay)


@app.route('/replay<gameID>.json')
def downloadreplay(gameID):
    gameID = int(gameID)
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('SELECT replay from icfpc2017_replays WHERE id=%s;', (gameID,))
        if not cursor.rowcount:
            return flask.render_template('404.html'), 404
        replay = json.loads(bytes(cursor.fetchone()[0]).decode())
        return flask.Response(
            json.dumps(replay, indent='  '),
            mimetype='application/json')

# ----------------------------- AUXILIARY -------------------------------#

def _movekey(move: dict) -> str:
    for key in ['pass', 'claim', 'option', 'splurge']:
        if key in move: 
            return key
    assert False, move


def _playerperfomances(replay: dict, playerIDs):
    players = [PlayerPerfomanceInfo(ID) for ID in playerIDs]
    for i, player in enumerate(replay['participants']):
        assert player['punter'] == i
        name = player['name']
        players[i].name = name

    for move in replay['moves']:
        key = _movekey(move)
        punter = move[key]['punter']
        players[punter].moves[key] += 1
        if 'timespan' in move:
            players[punter].sumtime += move['timespan']

        if 'error' in move:
            assert key == 'pass'
            players[punter].forcedpasses += 1
            if 'original' in move:
                players[punter].illegalmoves += 1
            elif move['error'] == 'timeout':
                players[punter].timeouts += 1
            elif move['error'] == 'zombie':
                assert players[punter].zombiereason     # assert already zombie
            else:
                players[punter].zombiesince = move['roundno']
                players[punter].zombiereason = move['error']

    for p in players:
        movesmade = sum(p.moves.values()) - p.forcedpasses
        p.averagetime = p.sumtime / movesmade if movesmade else 0
        if not p.futures:    # TODO: wtf?
            p.futures = '--'

    scores = sorted(replay['score'], key=lambda x: x['score'], reverse=True)
    for i, result in enumerate(scores):
        punter = result['punter']
        players[punter].score = result['score']
        players[punter].futures = result['futures']
        for p in reversed(players[:punter+1]):
            if p.score == result['score']:
                p.place = i + 1
            else:
                break

    return players


if __name__ == '__main__':
    if '--debug' in sys.argv[1:]:
        app.jinja_env.auto_reload = True
        app.debug = True

    app.run(port=PORT)
