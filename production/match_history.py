import typing, json
from datetime import datetime
import psycopg2

from production import scraper, json_format
from production.bot_interface import *


''' db structure, for the record:

create table icfpc2017_games(
    id              serial primary key,
    time            timestamptz not null default CURRENT_TIMESTAMP,
    botname         varchar(256) not null,
    submitter       varchar(256) not null,
    mapname         varchar(256) not null,
    extensions      varchar(256) not null,
    player_names    varchar(256) not null,
    num_players     integer not null,
    rank            integer not null,
    scores          varchar(256) not null,
    timeouts        integer not null,
    replay          bytea not null);

No indices except primary key because who cares.
'''

class _ReplayProxy:
    '''Lazily initialized replay fetching, using explicit get() method to make the magic conspicuous'''
    def __init__(self, id):
        self.id = id
        self._replay = None

    def get(self):
        if self._replay is None:
            self._replay = _fetch_replay(self.id)
        return self._replay


class GameRecord(typing.NamedTuple):
    id              : int
    time            : datetime
    botname         : str
    submitter       : str
    mapname         : str
    extensions      : typing.List[str]
    player_names    : typing.List[str]
    num_players     : int
    rank            : int
    scores          : typing.List[int]
    timeouts        : int
    replay          : _ReplayProxy


# set to override, default computer name
submitter = None
conn = None
def _init_globals():
    global submitter, conn
    if submitter is None:
        import getpass
        submitter = getpass.getuser()
    if conn is None:
        conn = psycopg2.connect(
            dbname='exampledb',
            host='35.187.84.11',
            user='postgres', password='asrna843l7a8;dtasi3')


def _db_replay_to_json(replay):
    return json.loads(str(replay, encoding='utf-8'))


def _fetch_replay(id: str):
    _init_globals()
    with conn.cursor() as cur:
        cur.execute('select replay from icfpc2017_games where id=%s', (id,))
        return _db_replay_to_json(cur.fetchone()[0])
    

def get_games(last=1000, with_replays=False, my=False, where=None):
    '''
    last=n: select n last replays, None for all
    with_replays=True: prefetch actual replay values
    my=True: select only replays for match_history.owner
    where="'futures' in extensions": free-form sql filters.
    
    Latest replays are returned first'''
    _init_globals()
    with conn.cursor() as cur:
        params = []
        query = 'select ' + ', '.join(GameRecord._fields[:-1])
        query += ', replay' if with_replays else ", ''::bytea"
        query += ' from icfpc2017_games where'
        if my:
            query += ' submitter = %s and'
            params.append(submitter)
        if where:
            query += ' (' + where + ') and'
        query += ' 1=1'
        query += ' order by id desc limit %s'
        params.append(last if last is not None else 'ALL')
        cur.execute(query, params)
        res = []
        for row in cur.fetchall():
            d = dict(zip(GameRecord._fields, row))
            d['extensions'] = d['extensions'].split()
            d['player_names'] = d['player_names'].split(',')
            d['scores'] = [int(s) for s in d['scores'].split()]
            
            rp = _ReplayProxy(d['id'])
            if with_replays:
                rp._replay = _db_replay_to_json(d['replay'])
            d['replay'] = rp

            res.append(GameRecord(**d))
        return res


def submit_game(botname, mapname, extensions, player_names, num_players, rank, scores, timeouts, replay):
    '''returns id, for future reference.'''
    _init_globals()
    with conn:
        with conn.cursor() as cur:
            cur.execute('''insert into icfpc2017_games
                (botname, submitter, mapname, extensions, player_names, num_players, rank, scores, timeouts, replay) values
                (     %s,        %s,      %s,         %s,           %s,          %s,    %s,    %s,       %s,     %s)
                returning id
            ''', (botname, submitter, mapname, extensions, player_names, num_players, rank, scores, timeouts, json.dumps(replay).encode()))
            return cur.fetchone()[0]


def submit_replay(botname: str, game: scraper.Game, replay: typing.Any):
    'Analyze replay and call submit_game'

    # find setup_request
    for it in replay:
        if 'map' in it:
            break
    else:
        assert False, f'No setup request in replay: {replay}'
    
    setup_request = json_format.parse_setup_request(it)

    # Last element of a replay should be a score response
    scores = json_format.parse_score_request(replay[-1]).score_by_punter
    # put our id first
    our_score = scores.pop(setup_request.punter)
    rank = sum(our_score <= s for s in scores.values())
    scores = [our_score] + list(scores.values())
    timeouts = 0
    return submit_game(
            botname,
            game.map_name if game else '<unknown>',
            ' '.join(k for k, v in setup_request.settings.raw_settings.items() if v),
            ','.join(game.punters) if game else '', 
            setup_request.punters, 
            rank, 
            ' '.join(str(s) for s in scores), 
            timeouts, 
            replay)


def get_statistics():
    _init_globals()
    with conn.cursor() as cur:
        cur.execute('select count(*), min(time), max(time) from icfpc2017_games')
        return cur.fetchone()


def replay_length(replay: typing.List[dict]):
    num_turns = len(replay) // 2 - 1
    return num_turns


def story_from_replay(replay: typing.List[dict], turn_number) -> Story:
    """does not rely on the state"""
    assert 0 <= turn_number <= replay_length(replay)

    req = json_format.parse_setup_request(replay[2])

    assert 'ready' in replay[3]
    my_futures = {f['source']: f['target'] for f in replay[3].get('futures', ())}

    moves = []
    for i in range(turn_number + 1):
        msg = replay[4 + 2 * i]
        moves += map(
            json_format.parse_move,
            (msg.get('move') or msg.get('stop'))['moves'])

    story = Story(
        punters=req.punters,
        my_id=req.punter,
        map=req.map,
        my_futures=my_futures,
        moves=moves)

    msg = json_format.parse_any_request(replay[4 + 2 * turn_number])
    if isinstance(msg, ScoreRequest):
        story = story._replace(score=msg.score_by_punter)
    else:
        assert isinstance(msg, GameplayRequest), type(msg)

    return story


def main():
    realistic_replay = [
        {'punter': 3, 'punters': 4, 'map': {'sites': [], 'rivers': [], 'mines': []}, 'state': {}}, 
        {'state': {}}, 
        {'stop': {'moves': [], 'scores': [{'punter': 0, 'score': 29}, {'punter': 1, 'score': 31}, {'punter': 2, 'score': 25}, {'punter': 3, 'score': 1}]}, 'state': {}},
    ]

    # for _ in range(3):
    #     print(submit_game('fake bot', 'fake_map.json', 'x y', 'player 1,player 2', 8, 3, '123 124 21 223 1', 0, [{}, dict(a='b'), {}, {}]))
    # g = scraper.Game(3, 5, 9001, 'other_fake.json', ['pp x', 'pp y'], ['ext'])
    # for _ in range(3):
    #     print(submit_replay('fake bot2', g, realistic_replay))

    games = get_games(last=5, with_replays=True, my=True, where='rank > 1')
    print(games)
    for i, msg in enumerate(games[0].replay._replay):
        print()
        print(i, msg)


if __name__ == '__main__':
    main()    
                
