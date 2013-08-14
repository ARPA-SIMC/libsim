#!/bin/sh

set -e
tmpfile=test_$$
if [ "$1" = "installed" ]; then
    pref=""
else
    pref=./
fi

# vg6d_getpoint with nearest point test
${pref}vg6d_getpoint --trans-type=inter --sub-type=near \
    --output-format=native ../data/t_p.grb $tmpfile.v7d

${pref}v7d_transform --input-format=native --output-format=csv $tmpfile.v7d $tmpfile.csv

cat > $tmpfile.check.csv <<EOF
written by v7d_transform
Date,Time range,P1,P2,Longitude,Latitude,Level1,L1,Level2,L2,Report,B12101,B10004
2011-08-18 12:00:00,254,43200,0,10.00000,45.00000,105,29,105,30,generic,293.667969,88460.9375
EOF

diff $tmpfile.check.csv $tmpfile.csv
rm -f $tmpfile.check.csv $tmpfile.csv
echo "vg6d_getpoint with nearest point test passed"

# vg6d_getpoint with polygons test
${pref}vg6d_getpoint --trans-type=polyinter --sub-type=average \
    --coord-format=shp --coord-file=../data/macroaree_er \
    --output-format=native ../data/t_p.grb $tmpfile.v7d

${pref}v7d_transform --input-format=native --output-format=csv $tmpfile.v7d $tmpfile.csv

cat > $tmpfile.check.csv <<EOF
written by v7d_transform
Date,Time range,P1,P2,Longitude,Latitude,Level1,L1,Level2,L2,Report,B01192,B12101,B10004
2011-08-18 12:00:00,254,43200,0,12.14052,44.00126,105,29,105,30,generic,1,290.872894,84530.0781
2011-08-18 12:00:00,254,43200,0,12.16615,44.16511,105,29,105,30,generic,2,293.900208,88468.6328
2011-08-18 12:00:00,254,43200,0,11.35038,44.32506,105,29,105,30,generic,3,291.539825,84737.7422
2011-08-18 12:00:00,254,43200,0,11.75953,44.61603,105,29,105,30,generic,4,294.172485,88755.1875
2011-08-18 12:00:00,254,43200,0,10.62697,44.48177,105,29,105,30,generic,5,289.914581,83044.0469
2011-08-18 12:00:00,254,43200,0,10.88700,44.73881,105,29,105,30,generic,6,293.723846,88514.7344
2011-08-18 12:00:00,254,43200,0,9.77320,44.64666,105,29,105,30,generic,7,288.863953,82225.8984
2011-08-18 12:00:00,254,43200,0,9.88318,44.90399,105,29,105,30,generic,8,292.667694,87197.0078
EOF

diff $tmpfile.check.csv $tmpfile.csv
rm -f $tmpfile.check.csv $tmpfile.csv
echo "vg6d_getpoint with polygons test passed"
