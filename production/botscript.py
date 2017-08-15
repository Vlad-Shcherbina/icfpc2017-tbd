import sys
import time
import argparse
from production.comms import online_mainloop

import logging
logger = logging.getLogger(__name__)



def help():
    print('Usage:\n'
        '>> botscript.py name bot loop'
        'where \n'
        'name -- string with name for bot,\n'
        'bot -- "cpp" for CppBot, "dumb" for FirstMoveBot, "random" for RandMoveBot,\n'
        'loop -- 0 or omitted for single run, cycle time in seconds for loop\n')


def run_bot(token, bot, cycled):
    if bot == 'cpp':
        from production.cpp_bot import CppBot as Bot
    elif bot == 'dumb':
        from production.dumb_bots import FirstMoveBot as Bot
    elif bot == 'random':
        from production.dumb_rand_bots import RandMoveBot as Bot
    else:
        return
    bot = Bot()

    while True:
        logger.info(f'bot {token} is connecting to the game')
        try:
            scores = online_mainloop('127.0.0.1', 42424, token, bot)
        except ConnectionRefusedError as e:
            logger.warning('Server refused connection')
        except ConnectionResetError as e:
            logger.warning('Server closed connection')
        else: 
            logger.info(f'bot {token} finished game.\n{scores}')
        if not cycled: 
            return


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('token', 
                        help='token (currently arbitrary nickname) of the bot',
                        type=str)

    parser.add_argument('bot', 
                        help='type cpp for CppBot, dumb for FirstMoveBot, random for RandMoveBot',
                        type=str)

    parser.add_argument('-c', '--cycle',
                        help='run bots in cycle, starting next one after previous terminates',
                        action='store_true')

    args = parser.parse_args()
    logging.basicConfig(
        level=logging.DEBUG,
        stream=sys.stdout,
        format='%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')
    run_bot(args.token, args.bot, args.cycle)

if __name__ == '__main__':
    main()
