#!/bin/sh

set -e
#set -x
LOG4C_PRIORITY=warning
tmpfile=test_$$

base64 -d <<EOF > test_tp_$$.grib
R1JJQv//AAIAAAAAAAAAswAAABUBAGIAAAQAAQfXAxcMAAAAAgAAAEgDAAAAAfAAAAAABv//////
/////////////wAAABAAAAAfAAAAAP////8Dk4cAAAAAADAAAAAAAcnDgAAehIAAHoSAAAAAACIE
AAAAAAAAAP+AAAAAAQAAAAAB//////////////8AAAAVBQAAAfAAAD+AAACACgAAAAAAAAAGBv8A
AAAFBzc3Nzc=
EOF
# refdate 2007032312

for i in `seq 1 6`; do
    grib_set -s productDefinitionTemplateNumber=8,parameterCategory=1,parameterNumber=52,typeOfStatisticalProcessing=1,lengthOfTimeRange=$i,forecastTime=0 \
	     -d $(($i*($i+1)/2)) test_tp_$$.grib test_tp_${i}_$$.grib
done

cat test_tp_*_$$.grib > test_tp_all_grow.grib

for td in 0 1; do
    echo "working with time definition $td"
    

    echo "Accumulation on 1 hour"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 01' \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_all_01.grib

    grib_get -p stepRange test_tp_all_01.grib > diff_$$
    diff -b - diff_$$ <<EOF
0-1
1-2
2-3
3-4
4-5
5-6
EOF

    grib_get -i 1 test_tp_all_01.grib > diff_$$
    diff -b - diff_$$ <<EOF
1
2
3
4
5
6
EOF

    echo "Accumulation on 1 hour, full steps"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 01' \
		     --comp-full-steps \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_all_01.grib

    grib_get -p stepRange test_tp_all_01.grib > diff_$$
    diff -b - diff_$$ <<EOF
0-1
1-2
2-3
3-4
4-5
5-6
EOF

    grib_get -i 1 test_tp_all_01.grib > diff_$$
    diff -b - diff_$$ <<EOF
1
2
3
4
5
6
EOF

    echo "Accumulation on 3 hours"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 03' \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_all_03.grib

    grib_get -p stepRange test_tp_all_03.grib > diff_$$
    diff -b - diff_$$ <<EOF
0-3
1-4
2-5
3-6
EOF

    grib_get -i 1 test_tp_all_03.grib > diff_$$
    diff -b - diff_$$ <<EOF
6
9
12
15
EOF

    echo "Accumulation on 3 hours, full steps"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 03' \
		     --comp-full-steps \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_full_03.grib

    grib_get -p stepRange test_tp_full_03.grib > diff_$$
    diff -b - diff_$$ <<EOF
0-3
3-6
EOF

    grib_get -i 1 test_tp_full_03.grib > diff_$$
    diff -b - diff_$$ <<EOF
6
15
EOF

    echo "Accumulation on 4 hours"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 04' \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_all_04.grib

    grib_get -p stepRange test_tp_all_04.grib > diff_$$
    diff -b - diff_$$ <<EOF
0-4
1-5
2-6
EOF

    grib_get -i 1 test_tp_all_04.grib > diff_$$
    diff -b - diff_$$ <<EOF
10
14
18
EOF

    echo "Accumulation on 4 hours, full steps"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 04' --comp-full-steps \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_full_04.grib

    grib_get -p stepRange test_tp_full_04.grib > diff_$$
    diff -b - diff_$$ <<EOF
0-4
EOF

    grib_get -i 1 test_tp_full_04.grib > diff_$$
    diff -b - diff_$$ <<EOF
10
EOF

    echo "Accumulation on 3 hours, start"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 03' \
		     --comp-start=2007-03-23T14:00 \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_full_03.grib

    grib_get -p stepRange test_tp_full_03.grib > diff_$$
    diff -b - diff_$$ <<EOF
2-5
3-6
EOF

    echo "Accumulation on 3 hours, start before"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 03' \
		     --comp-start=2007-03-22T14:00 \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_full_03.grib

    grib_get -p stepRange test_tp_full_03.grib > diff_$$
    diff -b - diff_$$ <<EOF
0-3
1-4
2-5
3-6
EOF

    echo "Accumulation on 3 hours, start, full steps"
    ./vg6d_transform --comp-stat-proc=1:1 --comp-step='00 03' \
		     --comp-full-steps --comp-start=2007-03-23T14:00 \
		     --time-definition=$td \
		     test_tp_all_grow.grib test_tp_full_03.grib

    grib_get -p stepRange test_tp_full_03.grib > diff_$$
    diff -b - diff_$$ <<EOF
2-5
EOF


done
cp test_tp_all_grow.grib p.grib
rm -f test_tp_$$.grib  test_tp_*_$$.grib test_tp_all_grow.grib \
   test_tp_all_??.grib test_tp_full_??.grib diff_$$ 
