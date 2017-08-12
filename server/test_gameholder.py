from gameholder import *


def test_request_recognise():
    s = Settings(futures=True, splurges=False, options=True, raw_settings='')
    holder = GameHolder('../maps/official_map_samples/lambda.json', s, 4)
    datafile = open('test_aux/testgame1.txt')
    i = 0
    f = json.loads('{"ready": 0, "futures": [{"source": 32, "target": 8}]}')
    holder.process_setup(0, f)
    for line in datafile.readlines():
        request = json.loads(line)
        holder.process_request(i, request)
        i = (i + 1) % 4
    datafile.close()
    assert holder.score() == [[5, 8], [1], [0], [0]]


def test_smth():
    #assert False, "working"
    pass


if __name__ == '__main__':
    test_request_recognise()