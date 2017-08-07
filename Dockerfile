FROM debian:9

WORKDIR /root/

RUN echo "deb http://ftp.debian.org/debian/ stretch main contrib non-free" >> /etc/apt/sources.list && \
    echo "deb-src http://ftp.debian.org/debian/ stretch  main contrib non-free" >> /etc/apt/sources.list

RUN apt-get update

RUN apt-get build-dep python3.5 -y

RUN apt-get install curl -y

RUN curl -O --ssl-reqd https://www.python.org/ftp/python/3.6.2/Python-3.6.2.tgz && \
    tar -zxvf Python-3.6.2.tgz

RUN cd Python-3.6.2 && \
    ./configure --prefix=$HOME/tbd_python --exec-prefix=$HOME/tbd_python --enable-optimizations --with-lto && \
    make -s && \
    make altinstall && \
    cd .. && \
    rm -rf Python-3.6.2

COPY requirements.txt requirements.txt

RUN /root/tbd_python/bin/pip3.6 install -r requirements.txt

WORKDIR /root/tbd_python

RUN find . -name '__pycache__' | xargs rm -rf

RUN find . -name '*.a' | xargs rm

RUN rm -rf lib/python3.6/test

WORKDIR /root/

RUN tar -cJf /root/tbd_python.tar.xz tbd_python

RUN mkdir icfpc2017-tbd

COPY . icfpc2017-tbd/

WORKDIR icfpc2017-tbd

RUN find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf

RUN ~/tbd_python/bin/python3.6 -m production.test_all

RUN rm -rf production/cpp/build && rm production/cpp/stuff.cpython*.so

RUN make clean && make 

RUN rm -rf /root/tbd_python && \
    rm -rf /root/Python-3.6.2

RUN mkdir /home/someuser

RUN cp icfp-761516ab-2a60-4b4e-a8e8-486e75c4c850.tar.gz /home/someuser

WORKDIR /home/someuser

ENV HOME /home/someuser

RUN tar xf icfp-761516ab-2a60-4b4e-a8e8-486e75c4c850.tar.gz

RUN ./install

ADD http://icfpcontest2017.github.io/static/lamduct /usr/local/bin/lamduct

RUN chmod +x /usr/local/bin/lamduct
