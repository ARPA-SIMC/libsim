#!/bin/sh
set -e

function failed(){
echo "cmp failled"
exit 1
}

echo "check  dba_qcfilter"
./dba_qcfilter -i dba_qcfilter.test -o dba_qcfilter.out
cmp -b dba_qcfilter.test dba_qcfilter.out  || failed

