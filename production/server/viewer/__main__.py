# hack to placate the restarter
import sys, os, datetime
import zlib
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..'))

from production.server import config
if config.USE_HINTCHECK and __name__ == '__main__':
    import hintcheck
    hintcheck.monkey_patch_named_tuple_constructors()

from typing import NamedTuple, List
import time
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


app = flask.Flask(
    'webviewer',
    root_path=os.path.dirname(__file__))
app.jinja_env.undefined = jnj.StrictUndefined


# used for a list of players in leaderboard
class PlayerBaseInfo(NamedTuple):
    ID: int
    name: str
    games: int
    rating: int


# used for a list of games in lastgames
class GameBaseInfo(NamedTuple):
    ID: int
    mapname: str
    settings: str
    players: str
    time: datetime.datetime


# used for a list of games in playerrating
class GameRatingInfo(NamedTuple):
    ID: int
    time: datetime.datetime
    mu_diff: float
    sigma_diff: float


# used for a list of players in gamestatistics
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
    '''Show all players sorted by rating.'''
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT players.id, name, rating_mu, rating_sigma, 
                          COUNT(game_id) FROM players INNER JOIN participation 
                          ON players.id = participation.player_id 
                          GROUP BY players.id;''')
                        # WHERE NOT name LIKE "zzz_%"
        playerlist = []
        for row in cursor.fetchall():
            playerID, name, rating_mu, rating_sigma, games = row
            rating = int(rating_mu - 3 * rating_sigma + 0.5)
            playerlist.append(PlayerBaseInfo(ID=playerID, name=name, rating=rating, games=games))
        playerlist.sort(key=lambda x: x.rating, reverse=True)
    return flask.render_template('leaderboard.html', playerlist=playerlist)
    dbconn.close()


@app.route('/lastgames')
def lastgames():
    '''Show a page of games, filters provided as string arguments.'''
    dbconn = connect_to_db()
    gamelist = _get_games_conditioned(dbconn)
    dbconn.close()
    if len(gamelist) > config.GAMES_PER_PAGE:
        before = gamelist[config.GAMES_PER_PAGE].time
        gamelist = gamelist[:config.GAMES_PER_PAGE]
        hasnext = True
    else:
        before = time.time()
        hasnext = False
    return flask.render_template('lastgames.html', 
                                gamelist=gamelist, 
                                hasnext=hasnext, 
                                before=before)


@app.route('/instructions')
def instructions():
    '''Show page with instructions.'''
    return flask.render_template('instructions.html')


@app.route('/playerstatistics/<playerID>')
def playerstatistics(playerID):
    '''Show summary statistics for a given player.'''
    playerID = int(playerID)
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT name, rating_mu, rating_sigma 
                          FROM players WHERE id = %s''', (playerID, ))
        if cursor.rowcount == 0:
            return flask.render_template('404.html', notfound='player'), 404
        name, mu, sigma = cursor.fetchone()

        cursor.execute('''SELECT COUNT(game_id) 
                          FROM players INNER JOIN participation 
                          ON players.id = participation.player_id 
                          WHERE player_id = %s;''', (playerID, ))
        games = cursor.fetchone()[0]
    dbconn.close()

    return flask.render_template('playerstatistics.html',  
                                name=name, 
                                playerID=playerID, 
                                rating=mu-3*sigma,
                                mu=mu, 
                                sigma=sigma, 
                                games=games)


@app.route('/playerrating')
def playerrating():
    playerID = int(flask.request.args.get('playerID', 0))
    dbconn = connect_to_db()
    before = flask.request.args.get('before', datetime.datetime.utcnow())

    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT name, rating_mu, rating_sigma from players
                          WHERE id=%s''', (playerID, ))
        if cursor.rowcount == 0:
            return render_template('404.html', notfound='player'), 404

        name, mu, sigma = cursor.fetchone()
        cursor.execute('''SELECT game_id, timefinish, additional
                          FROM participation INNER JOIN games
                          ON games.id = participation.game_id
                          WHERE player_id = %s AND timefinish <= %s
                          ORDER BY timefinish DESC
                          LIMIT %s;''',
                          (playerID, before, config.GAMES_PER_PAGE + 1))
        rows = cursor.fetchall()

    dbconn.close()
    gamelist = []
    for row in rows[:config.GAMES_PER_PAGE]:
        gameID, timefinish, add = row
        #add = json.loads(add)
        mu_diff = add['mu_after']-add['mu_before']
        sigma_diff = add['sigma_after']-add['sigma_before']
        gamelist.append(GameRatingInfo(
                            ID=gameID,
                            time=timefinish,
                            mu_diff=mu_diff,
                            sigma_diff=sigma_diff))
    if len(rows) > config.GAMES_PER_PAGE:
        before = rows[config.GAMES_PER_PAGE][1]
        hasnext = True
    else:
        hasnext = False
    return flask.render_template('playerrating.html',
                                playerID=playerID,
                                name=name,
                                mu=mu,
                                sigma=sigma,
                                gamelist=gamelist,
                                hasnext=hasnext,
                                before=before)


@app.route('/gamestatistics/<gameID>')
def gamestatistics(gameID):
    '''Show summary statistics and replay for a given game.'''
    gameID = int(gameID)
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('''SELECT games.mapname, futures, options, splurges, 
                          timestart, timefinish, replay 
                          FROM games INNER JOIN replays ON games.id = replays.id 
                          WHERE games.id=%s;''', (gameID, ))
        if not cursor.rowcount:
            return flask.render_template('404.html', notfound='game'), 404
        mapname, futures, options, splurges, timestart, timefinish, replay = cursor.fetchone()
        settings = (('futures, ' if futures else '') 
                    + ('options, ' if options else '')
                    + ('splurges, ' if splurges else ''))[:-2]
        replay = json.loads(zlib.decompress(bytes(replay)))

        playerIDs = []
        playerscores = []
        cursor.execute('''SELECT player_id, score FROM participation 
                          WHERE game_id=%s ORDER BY player_order;''', 
                          (gameID, ))
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
    '''Create link for downloading replay in json format.'''
    gameID = int(gameID)
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('SELECT replay from replays WHERE id=%s;', (gameID,))
        if not cursor.rowcount:
            return flask.render_template('404.html', notfound='replay'), 404
        replay = json.loads(zlib.decompress(bytes(cursor.fetchone()[0])))
        return flask.Response(
            json.dumps(replay, indent='  '), 
            mimetype='application/json')


@app.route('/<mapname>')
def downloadmap(mapname):
    '''Create link for downloading map in json format.'''
    dbconn = connect_to_db()
    with dbconn.cursor() as cursor:
        cursor.execute('SELECT maptext from maps WHERE mapname=%s;', (mapname,))
        if not cursor.rowcount:
            return flask.render_template('404.html', notfound='map'), 404
        maptext = json.loads(zlib.decompress(bytes(cursor.fetchone()[0])))
        return flask.Response(
            json.dumps(maptext, indent='  '), 
            mimetype='application/json')


def url_for_other_page(before):
    args = flask.request.args.copy()
    args['before'] = before
    return flask.url_for(flask.request.endpoint, **args)
app.jinja_env.globals['url_for_other_page'] = url_for_other_page

@app.template_filter('datetimeformat')
def datetimeformat(value, format='%Y-%m-%d %H:%M:%S'):
    return value.strftime(format)

@app.template_filter('timedeltaformat')
def timedeltaformat(value, format='%H:%M:%S'):
    s = value.total_seconds()
    return '{:02}:{:02}:{:02}'.format(int(s // 3600), int(s % 3600 // 60), int(s % 60))

# ----------------------------- AUXILIARY -------------------------------#

def _movekey(move: dict) -> str:
    '''Determine type of move in dictionary.'''
    for key in ['pass', 'claim', 'option', 'splurge']:
        if key in move: 
            return key
    assert False, move


def _playerperfomances(replay: dict, playerIDs):
    '''Gather players' statistics for one replay: moves, time, errors, etc.'''
    players = [PlayerPerfomanceInfo(ID) for ID in playerIDs]
    for i, player in enumerate(replay['participants']):
        assert player['punter'] == i
        name = player['name']
        players[i].name = name

    for i, response in enumerate(replay['setup']['responses']):
        assert response['ready'] == i
        if 'error' in response:
            players[i].zombiesince = 'setup'
            players[i].zombiereason = response['error']

    for i, move in enumerate(replay['moves']):
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
                pass
            else:
                players[punter].zombiesince = i // len(players) + 1
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


def _get_games_conditioned(conn) -> List[GameBaseInfo]:
    '''Get a list of games with filters provided as string query_args.'''
    query, query_args = _get_query_for_game_conditions(flask.request.args)

    with conn.cursor() as cursor:
        cursor.execute(query, query_args)
        if cursor.rowcount == 0:
            return []
        rows = cursor.fetchall()
        participants = { r[0] : [] for r in rows }
        cursor.execute('''SELECT game_id, name 
                          FROM participation INNER JOIN players 
                          ON participation.player_id = players.id 
                          WHERE game_id IN %s 
                          ORDER BY score DESC;''', 
                          (tuple(participants.keys()), ))
        for gameID, name in cursor.fetchall():
            participants[gameID].append(name)

        gamelist = []
        for row in rows:
            gameID, mapname, futures, options, splurges, timefinish = row
            settings = (('futures, ' if futures else '') 
                        + ('options, ' if options else '') 
                        + ('splurges, ' if splurges else ''))[:-2]

            players = ', '.join(participants[gameID])
            gamelist.append(GameBaseInfo(ID=gameID, 
                                         mapname=mapname, 
                                         settings=settings, 
                                         players=players, 
                                         time=timefinish))
    return gamelist


def _get_query_for_game_conditions(request_args):
    query_args = tuple()
    query = '''SELECT games.id, mapname, futures, options, splurges, timefinish 
               FROM games INNER JOIN participation 
               ON games.id = participation.game_id 
               WHERE status='finished' '''

    if 'playerID' in request_args:
        query += 'AND player_id=%s '
        query_args += (request_args['playerID'],)
    if 'before' in request_args:
        query += 'AND timefinish <= %s '
        query_args += (request_args['before'],)
    if 'errors' in request_args:
        query += 'AND forcedmoves=%s '
        query_args += (request_args['errors'],)
    

    query += 'GROUP BY games.id ORDER BY timefinish DESC LIMIT %s;'
    query_args += (config.GAMES_PER_PAGE + 1,)
    return query, query_args


if __name__ == '__main__':
    if config.USE_HINTCHECK:
        hintcheck.hintcheck_all_functions()

    if config.FLASK_DEBUG:
        app.jinja_env.auto_reload = True
        app.debug = True

    app.run(port=config.WEB_SERVER_PORT)
