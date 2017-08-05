#!/usr/bin/env bash

# Uninstall all
~/tbd_python/bin/pip3.6 uninstall -y -r requirements.txt

# Install
~/tbd_python/bin/pip3.6 install -r requirements.txt

# Cleanup
find ~/tbd_python/ -name '__pycache__' | xargs rm -rf
find ~/tbd_python/ -name '*.a' | xargs rm
rm -rf ~/tbd_python/lib/python3.6/test

# Pack
tar -cvJf ../tbd_python.tar.xz ~/tbd_python

