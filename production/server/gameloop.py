import logging;
logger = logging.getLogger(__name__)

from gameholder import *
from connector import *
from production.bot_interface import *
from production import json_format
from production.json_format import InvalidResponseError

GAMELIMIT = 1
SETUPLIMIT = 10

def gameloop(rawmap, settings, connections, names):
    N = len(names)
    assert N == len(connections)
    replay = [{ 'punter' : i, 'name' : names[i]} for i in range(N)]

    connector, gameholder = setup_game(rawmap, settings, connections, replay)
    turns = sum([len(a) for a in gameholder.board.adj.values()]) // 2

    ID = 0
    for _ in range(turns):
        request = gameholder.get_gameplay_request()
        connector.send(ID, request)
        connresponse = connector.receive(ID, time.time() + GAMELIMIT)
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
        replay.append(record)
        ID = (ID + 1) % N

    score = gameholder.score()
    totals = []
    for i in range(N):
        connector.send(i, gameholder.get_score_request())
        totals.append(dict(punter=i, 
                           name=names[i], 
                           score=score[i],
                           futures=gameholder.totals[i][1:]))
    replay.append({ 'scores' : totals })
    connector.close_all()
    return replay, score


def setup_game(rawmap: dict, 
          settings: Settings, 
          connections: List[NetworkConnection], 
          replay: List) -> int:

    N = len(connections)
    m = json_format.parse_map(rawmap)

    # add only one setup to replay, to provide map and settings
    replay.append(dict(punter=0, 
                       punters=N, 
                       map=rawmap, 
                       settings=json_format.format_settings(settings)))

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
        connresponse = connector.receive(ID, deadlines[ID])
        try:
            r = json_format.parse_setup_response(connresponse.message, ID)
        except InvalidResponseError as e:
            assert connresponse.error is None
            connector.zombify(ID, e.msg)
            r = SetupResponse(ready=ID, state='', futures=[])
        else:
            gameholder.process_futures(response=r)
        replay.append(json_format.format_setup_response(r))
            
    return connector, gameholder
