import json

from production import utils
from production.bot_interface import *
from production import json_format


def test_pasing_official_map():
    d = utils.project_root() / 'maps' / 'official_map_samples'
    for p in d.glob('*.json'):
        print(p)
        m = json.loads(p.read_text())
        json_format.parse_map(m)


def test_simple_gameplay_step():
    r = {
    'move': {
        'moves': [
            {'claim': {'punter': 0, 'source': 3, 'target': 5}},
            {'pass': {'punter': 1}}]},
    'state': 'zzz'}
    print(json_format.parse_gameplay_request(r))

    r = GameplayResponse(move=PassMove(punter=42), state='zzz2')
    print(json_format.format_gameplay_response(r))


if __name__ == '__main__':
    import sys, pytest
    pytest.main([__file__] + sys.argv[1:])
