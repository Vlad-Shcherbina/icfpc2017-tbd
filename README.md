## Setup

On Windows, you'll need C++ compiler. Install Visual Studio 2015 Community or Express edition _or_ Visual C++ Build Tools 2015. Visual Studio 2017 wouldn't suffice, you'd still need 2015 Build Tools.

CPython 3.6.2 (the latest stable release).

Virtualenv is optional.

`pip3 install -r requirements.txt`

Copy `git_hooks/pre-push` to `.git/hooks/`.


## Running stuff

Root of this repository should be in `PYTHONPATH`, because we use absolute imports (`from production import utils`). There are several ways to achieve that:
  - add project path to the environment variable
  - create the file `<python installation or venv>/lib/python3.6/site-packages/tbd.pth` whose content is a single line `/path/to/icfpc2017-tbd`
  - configure your favorite IDE appropriately
  - use `python3 -m production.some_script` instead of `python3 production/some_script.py`

## Building stuff

Cleanup your local repository (all distribution resources are taken from .):
```text
git reset --hard origin/HEAD
```

To produce a tar ready for submission:

0. docker rm tbd_all
1. docker build . -t tbd_all # this takes ages on the first run
2. docker create --name tbd_all tbd_all
3. docker cp tbd_all:/root/icfpc2017-tbd/icfp-761516ab-2a60-4b4e-a8e8-486e75c4c850.tar.gz .

Testing with lamduct:
```text
docker run -it tbd_all lamduct --log-level=3 --game-port=`~/tbd_python/bin/python3.6 -m production.scraper --slots=1 --random` --client-instance-logfile=/dev/stderr ./punter
```
