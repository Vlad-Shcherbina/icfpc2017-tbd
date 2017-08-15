import threading
import production.json_format

from production.server.gameloop import *
from production.utils import project_root


def test_tester():
    #assert False, "working"
    pass


def test_gameholder_from_file():
    s = Settings(futures=True, 
                 splurges=False, 
                 options=True, 
                 raw_settings='')
    mapfile = project_root() / 'maps' / 'official_map_samples' / 'sample.json'
    with open(mapfile) as f:
        m = json_format.parse_map(json.load(f))

    board = Gameboard(adj=m.g, mines=m.mines, N=4, settings=s)
    holder = GameHolder(board)
    data = open(project_root() / 'production' / 'server' / 'test_aux' / 'testgame1.txt')
    
    ID = 0
    f = json_format.parse_setup_response(json.loads(data.readline()), ID)
    holder.process_futures(f)
    
    for line in data.readlines():
        move = json_format.parse_move(json.loads(line))
        holder.process_move(move)
        ID = (ID + 1) % 4
    data.close()
    holder.score()
    assert holder.totals == [[10, 8], [5], [0], [0]]


class FileBot(threading.Thread):
    def __init__(self, filename, client):
        threading.Thread.__init__(self)
        self.filename = filename
        self.client = client
        self.client.settimeout(1)

    def run(self):
        text = open(self.filename, 'r')
        current_time = time.time()
        for line in text:
            line = line.strip()
            msg = (f'{len(line)}:{line}').encode('utf-8')
            try:
                self.client.recv(4096)
            except socket.timeout:
                break
            self.client.send(msg)
        text.close()


def gameloop_from_file(N, testfiles, settings, m, result=None):
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind(('127.0.0.1', 42424))
    server.listen(1)

    conns = []
    clients = [None] * N
    bots = [None] * N

    for i in range(N):
        clients[i] = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        clients[i].connect(('127.0.0.1', 42424))
        bots[i] = FileBot(testfiles[i], clients[i])
        
        conn, addr = server.accept()
        conn = NetworkConnection(conn)
        conns.append(conn)
        bots[i].start()

    _, t = gameloop(m, settings, conns, [str(i) for i in range(N)])
    for bot in bots: bot.join()
    assert result is None or t == result

    for c in conns: c.close()
    for c in clients: c.close()
    server.close()


def test_gameloop_simple():
    N = 2
    testfolder = project_root() / 'production' / 'server' / 'test_aux'
    testfiles = [testfolder / (f'testbot{i}.txt') for i in range(N)]

    with open(project_root() / 'maps' / 'official_map_samples' / 'sample.json', 'r') as f:
        m = json_format.parse_map(json.load(f))

    settings = Settings(options=True, 
                        futures=True, 
                        splurges=False, 
                        raw_settings='')
    result = [-7, 16]        # [[1, -8], [15, 1]]
    gameloop_from_file(N, testfiles, settings, m, result)


if __name__ == '__main__':
    test_gameloop_simple()
    test_gameholder_from_file()
