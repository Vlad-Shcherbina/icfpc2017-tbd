from gameholder import *
from production.utils import project_root

def test_request_recognise():
    
    s = Settings(futures=True, splurges=False, options=True, raw_settings='')
    mapfile = project_root() / 'maps' / 'official_map_samples' / 'lambda.json'
    holder = GameHolder(mapfile, settings=s, N=4)

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


def test_smth():
    #assert False, "working"
    pass


if __name__ == '__main__':
    test_request_recognise()