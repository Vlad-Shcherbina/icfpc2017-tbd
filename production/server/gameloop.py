import logging;
logger = logging.getLogger(__name__)

from gameholder import *
from connector import *
from production.bot_interface import *
from production.json_format import parse_map

connector = None
gameholder = None
replay = []

def gameloop(mapfile, settings, connections, names):
    turns = setup_game(mapfile, settings, connections, names)

    ID = 0
    for _ in range(turns * len(connections)):
        request = connector.send(ID, gameholder.get_response(), timelimit=1)
        revised = gameholder.process_request(ID, request.message)
        error = request.error if request.error is not None else revised.error
        if revised.error:
            connector.zombify(ID, revised.error)
        # revised to replay!
        ID = (ID + 1) % len(names)

    totals = gameholder.score()
    #for i in range(len(names)):
    #    connector.send(i, totals.scorejson[i], timelimit=0.5) # 1s
    #totals to replay
    #new ratings to replay - ?


def setup_game(mapfile: str, 
          settings: Settings, 
          connections: List[NetworkConnection], 
          names: List[str]) -> int:

    global gameholder, connector
    N = len(names)
    with open(mapfile) as f:
        m = parse_map(json.load(f))

    board = Gameboard(adj=m.g, mines=m.mines, N=N, settings=settings)
    gameholder = GameHolder(board)
    connector = Connector(connections)

    #setup_replay(m, names)

    for ID in range(N):
        setup_request = gameholder.setup_response(ID, m.raw_map)
        response = connector.send(ID, setup_request, timelimit=2)  # !!! 10
        if not response.error:
            gameholder.process_setup(response.message)

    return int(sum(len(adj) for adj in m.g.values()) / 2 / N) # max_players???


def setup_replay(map: Map, settings: List, names: List):
    pass