#!/bin/bash -xe

## Usage kastel-daemon.sh stop | start | console

THIS_DIR="$(dirname $(readlink -f $0))"

KASTLE_HOME="$THIS_DIR/../_rel/kastle"

$KASTLE_HOME/bin/kastle stop || true

if [ "$1" = "stop" ]; then
  ## only to stop the daemon
  exit 0
fi

REL_CONFIG="$THIS_DIR/../relx.config"
VSN="$(erl -noshell -eval "{ok, RelConf} = file:consult(\"$REL_CONFIG\"), {release,{_,Vsn},_} = hd(RelConf), io:format(Vsn), halt(0)")"
SYS_CONFIG=$KASTLE_HOME/releases/$VSN/sys.config

SYS_CONFIG_SRC=/etc/kastle/sys.config

## use /etc/kastle/sys.config if exist
if [ -f $SYS_CONFIG_SRC ]; then
  cp $SYS_CONFIG_SRC $SYS_CONFIG
fi

## start kastle daemon
$KASTLE_HOME/bin/kastle $1

