from production.server.gameloop import *
from production.utils import project_root


def test_smth():
    #assert False, "working"
    pass


def test_gameholder_from_file():
    s = Settings(futures=True, splurges=False, options=True, raw_settings='')
    mapfile = project_root() / 'maps' / 'official_map_samples' / 'lambda.json'
    with open(mapfile) as f:
        m = parse_map(json.load(f))
    board = Gameboard(adj=m.g, mines=m.mines, N=4, settings=s)
    holder = GameHolder(board)

    datafile = project_root() / 'production' / 'server' / 'test_aux' / 'testgame1.txt'
    data = open(datafile)
    
    i = 0
    f = json.loads(data.readline())
    holder.process_setup(0, f)
    
    for line in data.readlines():
        request = json.loads(line)
        holder.process_request(i, request)
        i = (i + 1) % 4
    data.close()
    assert holder.score() == [[5, 8], [1], [0], [0]]


def test_gameloop_from_file():
    pass


if __name__ == '__main__':
    test_gameholder_from_file()