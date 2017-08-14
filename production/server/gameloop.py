import logging;
logger = logging.getLogger(__name__)

from gameholder import *
from connector import *
from production.bot_interface import *
from production import json_format

def gameloop(mapfile, settings, connections, names, turns):
    replay = []
    N = len(names)
    assert N == len(connections)

    connector, gameholder = setup_game(mapfile, settings, connections, names, replay)

    ID = 0
    for _ in range(turns * N):
        request = connector.send(ID, gameholder.get_response(), timelimit=1)
        move, error = gameholder.process_request(ID, request.message)
        if error == 'not a valid move':
            connector.zombify(ID, error)

        # there should appear only one not-None error
        assert error is None or request.error is None
        if request.error: error = request.error
        record = json_format.format_move(move, error, request.timespan)
        replay.append(record)
        ID = (ID + 1) % N

    totals = gameholder.score()

    for i in range(N):
        connector.send(i, sum(totals[i]), timelimit=0.5) # 1s
        replay.append(dict(punter=i, 
                           name=names[i], 
                           score=sum(totals[i]),
                           futures=totals[i][1:]))
    #replay to DB
    #new ratings to DB
    return totals   # <-- for debug only


def setup_game(mapfile: str, 
          settings: Settings, 
          connections: List[NetworkConnection], 
          names: List[str],
          replay: List) -> int:

    N = len(names)
    with open(mapfile) as f:
        m = json_format.parse_map(json.load(f))

    # add only one setup to replay, to provide map and settings
    replay.append(dict(punter=0, 
                       punters=N, 
                       map=m.raw_map, 
                       settings=json_format.format_settings(settings)))

    board = Gameboard(adj=m.g, mines=m.mines, N=N, settings=settings)
    gameholder = GameHolder(board)
    connector = Connector(connections)

    for ID in range(N):
        setup_response = gameholder.setup_response(ID, m.raw_map)
        request = connector.send(ID, setup_response, timelimit=10)
        r = reparse_setup_request(ID, request.message)
        if r is None:
            connector.zombify(ID, 'not a valid futures request')
            r = { 'ready': ID }
        else:
            gameholder.process_futures(ID, r)
        replay.append(r)
            
    return connector, gameholder


# move to json_format?
def reparse_setup_request(ID, request):
    '''Remove any possible additional field from request for replay.'''
    revised = { 'ready': ID }
    if not 'futures' in request:
        return revised

    revised['futures'] = []
    if not isinstance(request['futures'], list):
        return None

    for f in request['futures']:
        if (not isinstance(f, dict) or not 'source' in f or not 'target' in f):
            return None
        revised['futures'].append(dict(source=f['source'], target=f['target']))
    return revised
