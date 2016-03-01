#!/bin/bash -xe

THIS_DIR="$(dirname $(readlink -f $0))"

KASTLE_HOME="$THIS_DIR/../_rel/kastle"

REL_CONFIG="$THIS_DIR/../relx.config"

VSN="$(erl -noshell -eval "{ok, RelConf} = file:consult(\"$REL_CONFIG\"), {release,{_,Vsn},_} = hd(RelConf), io:format(Vsn), halt(0)")"

SYS_CONFIG=$KASTLE_HOME/releases/$VSN/sys.config

## copy a sys.config file if it is not found in release dir
if [ ! -f $SYS_CONFIG ]; then
  cp $THIS_DIR/../rel/sys.config.example $SYS_CONFIG
fi

## stop the daemon first if still running
$KASTLE_HOME/bin/kastle stop || true

## start kafka cluster
$THIS_DIR/start-test-brokers.sh

## start kastle daemon
$KASTLE_HOME/bin/kastle start

## NOTE, assuming kastle starts quick enough that there is no need to wait for the listening port to be ready
##       otherwise `grep "kastle is listening on port 8092" $KASTLE_HOME/log/info.log` to check

