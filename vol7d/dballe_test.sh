#!/bin/sh
set -e

function failed(){
    echo "cmp failed"
    exit 1
}

function cleanup(){
    rm -f dballe_test_copy1f.bufr dballe_test_copy1fmem.bufr dballe_test2.bufr dballe_test.sqlite
}

trap '{ cleanup; }' EXIT

echo "check  dballe_test_copy1f.bufr"
cmp -b dballe_test_copy1f.bufr dballe_test_copy1f.test || failed

echo "check  dballe_test_copy1fmem.bufr"
cmp -b dballe_test_copy1fmem.bufr dballe_test_copy1fmem.test || failed

echo "check  dballe_test2.bufr"
cmp -b dballe_test2.bufr dballe_test2.test || failed

