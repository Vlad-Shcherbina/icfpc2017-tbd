They provide an image with CPython 3.5.3, no network, no root.
We want CPython 3.6.2 and a bunch of third-party packages.

This document describes how to prepare all this stuff for their environment.

------------

Download and import their VM image (http://events.inf.ed.ac.uk/icfpcontest2017/post/welcome/).
Make sure that network adapters are disabled.
Make a VM snapshot for testing (this is how things are meant to be: no root, no network).

Enable NAT network adapter.

Login as root, password "icfp2017".

```
apt-get build-dep python3.5 -y
```

Login as punter, password "icfp2017".

```
curl -O --ssl-reqd https://www.python.org/ftp/python/3.6.2/Python-3.6.2.tgz
tar -zxvf Python-3.6.2.tgz

cd Python-3.6.2
./configure --prefix=$HOME/tbd_python --exec-prefix=$HOME/tbd_python --enable-optimizations --with-lto
make -s -j2  # this will take a while (less than an hour)

# Note: I tried `make test` and it failed,
# most likely because of https://bugs.python.org/issue30714 ,
# let's hope it's not significant.

make altinstall
```

```
# This step is optional, in case we need z3py.
Z3_VERSION=z3-4.5.1.0f1583309d08-x64-debian-8.5
curl -O --ssl-reqd https://raw.githubusercontent.com/Z3Prover/bin/master/nightly/$Z3_VERSION.zip
unzip $Z3_VERSION.zip
cd $Z3_VERSION
cp -r bin/python/z3 ~/tbd_python/lib/python3.6/site-packages/
cp bin/libz3.so ~/tbd_python/lib/python3.6/site-packages/z3
cp LICENSE.txt ~/tbd_python/lib/python3.6/site-packages/z3
```

```
# (Copy our repository icfpc2017-tbd to the VM somehow)
cd ~/icfpc2017-tbd
~/tbd_python/bin/pip3.6 install -r requirements.txt

~/tbd_python/bin/python3.6 -m production.test_all  # should be green
```

```
cd ~/tbd_python
find . -name '__pycache__' | xargs rm -rf
find . -name '*.a' | xargs rm
rm -rf lib/python3.6/test
```

```
cd ~
tar -cvJf tbd_python.tar.xz tbd_python
# (Copy tbd_python.tar.xz to the host machine somehow)
```

Now revert the VM to the original pristine snapshot, to transplant tbd_python there.

Login as punter, password "icfp2017".

```
# (Copy tbd_python.tar.xz to the VM somehow)
tar xf tbd_python.tar.xz

# (Copy our repository icfpc2017-tbd to the VM somehow)
cd ~/icfpc2017-tbd

# Things should work
~/tbd_python/bin/python3.6 -m production.test_all
```
