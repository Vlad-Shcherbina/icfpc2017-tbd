import json

from production import utils
from production import json_format


def test_pasing_official_map():
    d = utils.project_root() / 'maps' / 'official_map_samples'
    for p in d.glob('*.json'):
        print(p)
        m = json.loads(p.read_text())
        json_format.parse_map(m)


if __name__ == '__main__':
    import sys, pytest
    pytest.main([__file__] + sys.argv[1:])
