#!/bin/sh
set -e

function failed(){
echo "cmp failled"
exit 1
}

echo "check  example_dballe_copy1f.bufr"
cmp -b dballe_test_copy1f.bufr dballe_test_copy1f.test || failed

echo "check  example_dballe_copy1fmem.bufr"
cmp -b dballe_test_copy1fmem.bufr dballe_test_copy1fmem.test || failed

