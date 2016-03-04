#!/bin/bash -xe

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

## copy a sys.config file if it is not found in release dir
if [ ! -f $SYS_CONFIG ]; then
  cp $THIS_DIR/../rel/sys.config.example $SYS_CONFIG
fi

## start kastle daemon
$KASTLE_HOME/bin/kastle start

