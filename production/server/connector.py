from typing import NamedTuple
import time
from connection import *

import logging;
logger = logging.getLogger(__name__)

CONN_TOLERANCE = 0


class ConnMessage(NamedTuple):
    message: dict
    timespan: float
    error: str


def passmove(N):
    return {'pass': {'punter': N}}


class Connector:
    def __init__(self, conns):
        self.bots = conns
        self.timeouts = [0] * len(conns)


    def send(self, ID, response, timelimit):
        bot = self.bots[ID]
        if not bot.alive:
            error = bot.reason_dead
            bot.reason_dead = 'zombie'
            return ConnMessage(message=passmove(ID), 
                               error=error,
                               timespan=0)

        bot.skip_received()
        timestart = time.time()
        bot.send(response)
        request = bot.receive(time.time() + timelimit + CONN_TOLERANCE)
        timespan = time.time() - timestart

        if isinstance(request, Dead):
            return ConnMessage(message=passmove(ID), 
                               error=request.reason,
                               timespan=0)

        if isinstance(request, Timeout):
            self.timeouts[ID] += 1
            bot.send({'timeout': timelimit})
            if self.timeouts[ID] >= 10:
                bot.reason_dead = 'timeouts limit exceeded'
            return ConnMessage(message=passmove(ID), 
                               error='timeout',
                               timespan=0)

        if isinstance(request, dict):
            return ConnMessage(message=request, 
                               error=None, 
                               timespan=timespan)

        assert False, request


    def zombify(self, ID: int, msg: str):
        self.bots[ID].kick(reason=msg)
        self.bots[ID].reason_dead = 'zombie'


    def close_all(self):
        for bot in self.bots:
            bot.close()

