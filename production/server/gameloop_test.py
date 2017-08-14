import threading

from production.server.gameloop import *
from production.utils import project_root


def test_smth():
    #assert False, "working"
    pass


def test_gameholder_from_file():
    s = Settings(futures=True, 
                 splurges=False, 
                 options=True, 
                 raw_settings='')
    mapfile = project_root() / 'maps' / 'official_map_samples' / 'lambda.json'
    with open(mapfile) as f:
        m = parse_map(json.load(f))

    board = Gameboard(adj=m.g, mines=m.mines, N=4, settings=s)
    holder = GameHolder(board)

    data = open(project_root() / 'production' / 'server' / 'test_aux' / 'testgame1.txt')
    
    ID = 0
    f = json.loads(data.readline())
    holder.process_futures(0, f)
    
    for line in data.readlines():
        request = json.loads(line)
        holder.process_request(ID, request)
        ID = (ID + 1) % 4
    data.close()
    assert holder.score() == [[5, 8], [1], [0], [0]]


class FileBot(threading.Thread):
    def __init__(self, filename, client, timelimit):
        threading.Thread.__init__(self)
        self.filename = filename
        self.client = client
        self.timelimit = timelimit

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


def gameloop_from_file(N, testfiles, settings, mapfile, result=None, turns=10):
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind(('127.0.0.1', 42424))
    server.listen(1)

    conns = []
    clients = [None] * N

    for i in range(N):
        clients[i] = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        clients[i].connect(('127.0.0.1', 42424))
        bot = FileBot(testfiles[i], clients[i], timelimit=0.5)
        
        conn, addr = server.accept()
        conn = NetworkConnection(conn)
        conns.append(conn)
        bot.start()

    t = gameloop(mapfile, settings, conns, [str(i) for i in range(N)], turns)
    assert result is None or t == result

    for c in conns: c.close()
    for c in clients: c.close()
    server.close()


def test_gameloop_simple():
    N = 2
    testfolder = project_root() / 'production' / 'server' / 'test_aux'
    testfiles = [testfolder / ('testbot' + str(i) + '.txt') for i in range(N)]
    mapfile = project_root() / 'maps' / 'official_map_samples' / 'lambda.json'
    settings = Settings(options=True, 
                        futures=True, 
                        splurges=False, 
                        raw_settings='')
    result = [[5], [5, -8]]
    gameloop_from_file(N, testfiles, settings, mapfile, result, turns=15)
        

if __name__ == '__main__':
    test_gameloop_simple()