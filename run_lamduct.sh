#!/bin/sh
# optionally specify port as an argument
rm -f lamduct.log 
lamduct --log-level=3 --game-port=${1:-9126} --client-instance-logfile=lamduct.log scripts/punter
