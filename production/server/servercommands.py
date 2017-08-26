import socket
import sys
import time
import argparse
from production.comms import online_mainloop

import logging
logger = logging.getLogger(__name__)

# Stub - shortcuts to send server operational commands.
# There is only one command right now: stop accepting players, finish all 
# running games and stop.

SC_STOP = 'stop server'
SERVER_COMMANDS = (SC_STOP, )

def main(command):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('127.0.0.1', 42350))
    msg = '{"command": "' + command + '"}'
    s.sendall(f'{len(msg)}:{msg}'.encode())
    s.close()

if __name__ == '__main__':
    main(SC_STOP)
