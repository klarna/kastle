FROM erlang:latest

MAINTAINER Shi Zaiming <shi.zaiming@klarna.com>, Ivan Dyachkov <ivan.dyachkov@klarna.com>

RUN mkdir /kastle
WORKDIR /kastle
ADD rel ./rel
ADD src ./src
ADD priv ./priv
ADD erlang.mk ./erlang.mk
ADD Makefile ./Makefile
ADD relx.config ./relx.config
ADD scripts ./scripts
RUN make

EXPOSE 8092

## Mount /etc/kastle/sys.config to override the default sys.config

CMD ["/kastle/scripts/kastle-daemon.sh", "console"]

