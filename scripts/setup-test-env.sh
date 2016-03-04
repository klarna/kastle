#!/bin/bash -xe

THIS_DIR="$(dirname $(readlink -f $0))"

$THIS_DIR/kastle-daemon.sh stop

## start kafka cluster
$THIS_DIR/start-test-brokers.sh

## start kastle daemon
$THIS_DIR/kastle-daemon.sh start

## NOTE, assuming kastle starts quick enough that there is no need to wait for the listening port to be ready
##       otherwise `grep "kastle is listening on port 8092" $KASTLE_HOME/log/info.log` to check

