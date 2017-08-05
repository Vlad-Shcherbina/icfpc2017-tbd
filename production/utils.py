from pathlib import Path
import logging, sys

def project_root():
    return (Path(__file__)/'..'/'..').resolve()


def config_logging():
    logging.basicConfig(
            level=logging.DEBUG,
            stream=sys.stderr,
            format='%(levelname).1s %(module)10.10s:%(lineno)-4d %(message)s')

