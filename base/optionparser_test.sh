#!/bin/sh

set -e
tmpfile=test_$$
if [ "$1" = "installed" ]; then
    pref=""
else
    pref=./
    export LIBSIM_DATA=../data
fi

# generic runs
#${pref}optionparser_test
${pref}optionparser_test --help > /dev/null || (echo "error with --help"; false)
${pref}optionparser_test --version > /dev/null || (echo "error with --version"; false)
# empty run
${pref}optionparser_test defaultname,100,712.0,,489.0,0
# variations
${pref}optionparser_test -n changed --nx=12 changed,12,712.0,,489.0,0
${pref}optionparser_test --name=changed --nx 12 changed,12,712.0,,489.0,0
${pref}optionparser_test --yval=85.71 -v -v -v defaultname,100,712.0,85.71,489.0,3


