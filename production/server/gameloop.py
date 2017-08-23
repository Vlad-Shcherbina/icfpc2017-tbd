import logging;
logger = logging.getLogger(__name__)

from production.server.gameholder import *
from production.server.connector import *
from production.bot_interface import *
from production import json_format
from production.json_format import InvalidResponseError

GAMELIMIT = 1
SETUPLIMIT = 10

def gameloop(m: Map, 
             settings: Settings, 
             connections: List[NetworkConnection], 
             names: List[str]):
    '''Main loop for single game. Connects players and gamestate.

    Holds the game and returns replay (List[Dict]) and summary scores.
    Closes connections with all bots at exit.
    '''
    N = len(names)
    assert N == len(connections)
    replay = {'participants' : [{ 'punter' : i, 'name' : names[i]} for i in range(N)],
              'setup' : {},
              'moves' : [],
              'score' : None}

    connector, gameholder = setup_game(m, settings, connections, replay)
    turns = sum([len(a) for a in gameholder.board.adj.values()]) // 2

    # Game starts!
    ID = 0
    for moveno in range(turns):
        connector.send(ID, gameholder.get_gameplay_request())
        connresponse = connector.receive(ID, time.time() + GAMELIMIT, GAMELIMIT)
        error = connresponse.error
        try:
            move = json_format.parse_move(connresponse.message, ID)
        except InvalidResponseError as e:
            assert connresponse.error is None
            connector.zombify(ID, e.msg)
            move = PassMove(punter=ID)
            error = e.msg

        illegal_error = gameholder.process_move(move)

        original = None
        if illegal_error is not None:
            # illegal move - change move to pass and add original
            assert error is None
            error = illegal_error
            original = move
            move = PassMove(punter=ID)

        record = json_format.format_move(move, 
                                         error=error, 
                                         timespan=connresponse.timespan,
                                         original=original)
        replay['moves'].append(record)
        ID = (ID + 1) % N

    score = gameholder.score()
    totals = []
    for i in range(N):
        connector.send(i, gameholder.get_score_request())
        totals.append(dict(punter=i, 
                           name=names[i], 
                           score=score[i],
                           futures=gameholder.totals[i][1:]))
    replay['score'] = totals
    connector.close_all()
    return replay, score


def setup_game(m: Map, 
          settings: Settings, 
          connections: List[NetworkConnection], 
          replay: dict):

    '''Pre-loop setups: create gameholder and connector and send setup requests.'''
    N = len(connections)

    # add one setup request to replay, to provide map and settings
    replay['setup'].update({'map' : m.raw_map,
                            'settings' : json_format.format_settings(settings),
                            'responses' : []})

    board = Gameboard(adj=m.g, mines=m.mines, N=N, settings=settings)
    gameholder = GameHolder(board)
    connector = Connector(connections)

    deadlines = [None] * N
    for ID in range(N):
        setup_request = gameholder.get_setup_request(ID, m.raw_map)
        connector.send(ID, setup_request)
        deadlines[ID] = time.time() + SETUPLIMIT
        time.sleep(0.1)

    for ID in range(N):
        connresponse = connector.receive(ID, deadlines[ID], SETUPLIMIT)
        if 'pass' in connresponse.message:
            r = SetupResponse(ready=ID, state='', futures=[])
        else:
            try:
                r = json_format.parse_setup_response(connresponse.message, ID)
            except InvalidResponseError as e:
                assert connresponse.error is None, connresponse.error
                connector.zombify(ID, e.msg, logged=False)
                r = SetupResponse(ready=ID, state='', futures=[])
            else:
                gameholder.process_futures(response=r)
        replay['setup']['responses'].append(json_format.format_setup_response(r))
            
    return connector, gameholder
