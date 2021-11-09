#!/bin/sh

set -e
tmpfile=test_$$
if [ "$1" = "installed" ]; then
    pref=""
else
    pref=./
    export LIBSIM_DATA=../data
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
2011-08-18 12:00:00,254,43200,0,12.13863,44.00177,105,29,105,30,generic,1,290.872894,84530.0781
2011-08-18 12:00:00,254,43200,0,12.16570,44.16615,105,29,105,30,generic,2,293.900208,88468.6328
2011-08-18 12:00:00,254,43200,0,11.34861,44.32512,105,29,105,30,generic,3,291.539825,84737.7422
2011-08-18 12:00:00,254,43200,0,11.76025,44.61643,105,29,105,30,generic,4,294.172485,88755.1875
2011-08-18 12:00:00,254,43200,0,10.62764,44.48127,105,29,105,30,generic,5,289.914581,83044.0469
2011-08-18 12:00:00,254,43200,0,10.88643,44.73951,105,29,105,30,generic,6,293.723846,88514.7344
2011-08-18 12:00:00,254,43200,0,9.77186,44.64729,105,29,105,30,generic,7,288.863953,82225.8984
2011-08-18 12:00:00,254,43200,0,9.88116,44.90400,105,29,105,30,generic,8,292.667694,87197.0078
EOF

diff $tmpfile.check.csv $tmpfile.csv
rm -f $tmpfile.check.csv $tmpfile.csv
rm -f $tmpfile.v7d
echo "vg6d_getpoint with polygons test passed"


# vg6d_transform with bilin test
${pref}vg6d_transform --trans-mode=p --trans-type=inter --sub-type=bilin \
    --type=regular_ll --nx=11 --ny=11 --x-min=5. --x-max=15. \
    --y-min=40. --y-max=50. \
    ../data/t_p.grb ${tmpfile}_p.grb

${pref}vg6d_getpoint --trans-type=inter --sub-type=near --lon=8. --lat=47. \
    --output-format=native ${tmpfile}_p.grb ${tmpfile}_p.v7d

${pref}v7d_transform --input-format=native --output-format=csv \
    ${tmpfile}_p.v7d ${tmpfile}_p.csv

${pref}vg6d_transform --trans-mode=s --trans-type=inter --sub-type=bilin \
    --type=regular_ll --nx=21 --ny=21 --x-min=0. --x-max=20. \
    --y-min=35. --y-max=55. \
    ../data/t_p.grb ${tmpfile}_s.grb

# ${pref}vg6d_getpoint --trans-type=metamorphosis --sub-type=coordbb \
#     --ilon=7.5 --flon=8.5 --ilat=46.5 --flat=47.5 \
#     --output-format=native ${tmpfile}_s.grb ${tmpfile}_s.v7d

${pref}vg6d_getpoint --trans-type=inter --sub-type=near --lon=8. --lat=47. \
    --output-format=native ${tmpfile}_s.grb ${tmpfile}_s.v7d

${pref}v7d_transform --input-format=native --output-format=csv \
    ${tmpfile}_s.v7d ${tmpfile}_s.csv

diff ${tmpfile}_p.csv ${tmpfile}_s.csv
rm -f ${tmpfile}_*.*
echo "vg6d_transform with bilin test passed"

