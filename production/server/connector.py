from typing import NamedTuple, Optional
import time
from connection import *

import logging;
logger = logging.getLogger(__name__)


CONN_TOLERANCE = 0


class ConnMessage(NamedTuple):
    message: dict
    timespan: float
    error: Optional[str]


class Connector:
    '''Interim between gameloop and connections.

    Keeps track on all set of current connections, their timeouts
    and zombie status.'''

    def __init__(self, conns):
        self.bots = conns
        self.timeouts = [0] * len(conns)


    def send(self, ID, request):
        bot = self.bots[ID]
        if not bot.alive:
            return      
        bot.skip_received()
        bot.send(request)


    def receive(self, ID, deadline) -> ConnMessage:
        '''Return message if player connected and sent valid json.

        If connection timed out or is considered dead, return pass move and
        error message.
        '''
        bot = self.bots[ID]
        if not bot.alive:
            error = bot.reason_dead
            bot.reason_dead = 'zombie'
            return ConnMessage(message={'pass': {'punter': ID}}, 
                               error=error,
                               timespan=0)

        timestart = time.time()
        response = bot.receive(deadline + CONN_TOLERANCE)
        timespan = time.time() - timestart

        if isinstance(response, Dead):
            return ConnMessage(message={'pass': {'punter': ID}}, 
                               error=response.reason,
                               timespan=0)

        if isinstance(response, Timeout):
            self.timeouts[ID] += 1
            bot.send({'timeout': 1})   # ??? should we really send timelimit?
            if self.timeouts[ID] >= 10:
                bot.reason_dead = 'timeouts limit exceeded'
            return ConnMessage(message={'pass': {'punter': ID}}, 
                               error='timeout',
                               timespan=0)

        if isinstance(response, dict):
            return ConnMessage(message=response, 
                               error=None, 
                               timespan=timespan)

        assert False, request


    def zombify(self, ID: int, msg: str):
        self.bots[ID].kick(reason=msg)
        self.bots[ID].reason_dead = 'zombie'


    def close_all(self):
        for bot in self.bots:
            bot.close()
