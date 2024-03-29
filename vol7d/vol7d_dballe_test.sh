#!/bin/sh
set -e

failed() {
    echo "cmp failed"
    exit 1
}

cleanup() {
    rm -f vol7d_dballe_test_out.bufr 
}

trap '{ cleanup; }' EXIT

echo "check  vol7d_dballe_test.bufr"
cmp -b vol7d_dballe_test.test vol7d_dballe_test_out.bufr || failed

echo "check  vol7d_dballe_test2.bufr"
cmp -b vol7d_dballe_test2.test vol7d_dballe_test2_out.bufr || failed

