import subprocess
import cProfile
import time

from production import match_history
from production import json_format
from production import cpp_bot
from production import utils


def run(game):
    start = time.time()
    moves = 0

    bot = cpp_bot.CppBot()
    for msg in game.replay.get():
        if 'map' in msg:
            req = json_format.parse_setup_request(msg)
            print('setup')
            resp = bot.setup(req)
            state = resp.state
            moves += 1
        elif 'move' in msg:
            req = json_format.parse_gameplay_request(msg)
            req = req._replace(state=state)
            resp = bot.gameplay(req)
            state = req.state
            print(resp.move)
            moves += 1

    print(f'{(time.time() - start) / moves}s per move')


def main():
    game = match_history.get_game(956)  # edinburgh-sparse
    profile_file = utils.project_root() / 'outputs' / 'profile'

    cProfile.runctx('run(game)', globals(), locals(), profile_file)

    subprocess.check_call(
        f'gprof2dot -f pstats -n 1 -e 1 {profile_file} | '
        f'dot -Tpng -o {utils.project_root() / "outputs" / "profile.png"}',
        shell=True)


if __name__ == '__main__':
    main()