#!/bin/sh
set -e

function failed(){
    echo "cmp failed"
    exit 1
}

function cleanup(){
    rm -f vol7d_dballe_test_out.bufr 
}

trap '{ cleanup; }' EXIT

echo "check  vol7d_dballe_test.bufr"
cmp -b vol7d_dballe_test.test vol7d_dballe_test_out.bufr || failed

