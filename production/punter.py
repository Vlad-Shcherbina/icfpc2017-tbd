'''entrypoint for the offline shell script'''

from production.utils import config_logging
from production.comms import offline_mainloop
from production.dumb_bots import FirstMoveBot as Bot

config_logging()
offline_mainloop('tbd punter', Bot())