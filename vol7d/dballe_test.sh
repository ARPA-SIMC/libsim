#!/bin/sh
set -e

function failed(){
    echo "cmp failed"
    exit 1
}

function cleanup(){
    rm -f dballe_test_copy1f.bufr dballe_test_copy1fmem.bufr dballe_test.sqlite dballe_test2.bufr dballe_test3.bufr
}

trap '{ cleanup; }' EXIT

echo "check  dballe_test_copy1f.bufr"
cmp -b dballe_test_copy1f.bufr dballe_test_copy1f.test || failed

echo "check  dballe_test_copy1fmem.bufr"
cmp -b dballe_test_copy1fmem.bufr dballe_test_copy1fmem.test || failed

echo "check  dballe_test2.bufr"
cmp -b dballe_test2.bufr dballe_test2.test || failed
cmp -b dballe_test2_memdb.bufr dballe_test2_memdb.test || failed

echo "check  dballe_test3.bufr"
cmp -b dballe_test3.bufr dballe_test3.test || failed
