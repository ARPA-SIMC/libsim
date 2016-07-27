#!/bin/sh
set -e

function failed(){
echo "cmp failled"
exit 1
}

echo "check  dballe_test_copy1f.bufr"
cmp -b dballe_test_copy1f.bufr dballe_test_copy1f.test || failed

echo "check  dballe_test_copy1fmem.bufr"
cmp -b dballe_test_copy1fmem.bufr dballe_test_copy1fmem.test || failed

echo "check  dballe_test2.bufr"
cmp -b dballe_test2.bufr dballe_test2.test || failed
cmp -b dballe_test2_memdb.bufr dballe_test2.test || failed

