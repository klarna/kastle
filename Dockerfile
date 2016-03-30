FROM erlang:latest

MAINTAINER TheSyndicate <thesyndicate.e@klarna.com>

RUN mkdir /kastle
WORKDIR /kastle
ADD rel ./rel
ADD src ./src
ADD erlang.mk ./erlang.mk
ADD Makefile ./Makefile
ADD relx.config ./relx.config
ADD scripts ./scripts
RUN make

EXPOSE 8092

## Mount /etc/kastel/sys.config to override the default sys.config

CMD ["/kastle/scripts/kastle-daemon.sh", "console"]

