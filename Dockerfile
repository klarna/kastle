FROM erlang:latest

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

CMD ["/kastle/scripts/kastle-daemon.sh", "console"]

