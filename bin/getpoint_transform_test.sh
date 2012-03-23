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
2011-08-18 12:00:00,254,43200,0,10.000000000000000,45.000000000000000,105,29,105,30,generic,293.667969,88460.9375
EOF

diff $tmpfile.check.csv $tmpfile.csv
rm -f $tmpfile.???
echo "vg6d_getpoint with nearest point test passed"

# vg6d_getpoint with polygons test
${pref}vg6d_getpoint --trans-type=polyinter --sub-type=average \
    --coord-format=shp --coord-file=../data/macroaree_er \
    --output-format=native ../data/t_p.grb $tmpfile.v7d

${pref}v7d_transform --input-format=native --output-format=csv $tmpfile.v7d $tmpfile.csv

cat > $tmpfile.check.csv <<EOF
written by v7d_transform
Date,Time range,P1,P2,Longitude,Latitude,Level1,L1,Level2,L2,Report,B01192,B12101,B10004
2011-08-18 12:00:00,254,43200,0,12.140520475803619,44.001258900072415,105,29,105,30,generic,1,290.872894,84530.0781
2011-08-18 12:00:00,254,43200,0,12.166152386438647,44.165107720671152,105,29,105,30,generic,2,293.900208,88468.6328
2011-08-18 12:00:00,254,43200,0,11.350380162610579,44.325062327794399,105,29,105,30,generic,3,291.539825,84737.7422
2011-08-18 12:00:00,254,43200,0,11.759532162769597,44.616033204118906,105,29,105,30,generic,4,294.172485,88755.1875
2011-08-18 12:00:00,254,43200,0,10.626966051484303,44.481774074750149,105,29,105,30,generic,5,289.914581,83044.0469
2011-08-18 12:00:00,254,43200,0,10.887003617583582,44.738807291884605,105,29,105,30,generic,6,293.723846,88514.7344
2011-08-18 12:00:00,254,43200,0,9.7732043841311551,44.646657588389772,105,29,105,30,generic,7,288.863953,82225.8984
2011-08-18 12:00:00,254,43200,0,9.8831788646577774,44.903990892470922,105,29,105,30,generic,8,292.667694,87197.0078
EOF

diff $tmpfile.check.csv $tmpfile.csv
rm -f $tmpfile.???
echo "vg6d_getpoint with polygons test passed"
