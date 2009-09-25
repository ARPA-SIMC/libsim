#!/bin/sh

# This script converts the fortran files to be preprocessed from the
# "traditional" format (used by cpp called by gfortran) to the ISO cpp
# format (used by other compilers), it replaces the occurences of 2
# identifiers separated by an empty C comment, e.g. MACRO/**/_TYPE to
# a concatenating macro e.g. PASTE(MACRO,_TYPE)

if [ "$1" = "-f" ]; then
    for file in `find . -name \*.F90`; do
	sed -e 's?\([a-zA-Z][a-zA-Z0-9_]*\)/\*\*/\([a-zA-Z0-9_][a-zA-Z0-9_]*\)?PASTE(\1,\2)?g' $file > $file.modif
	if ! diff $file $file.modif>/dev/null; then
	    mv $file $file.orig
	    mv $file.modif $file
	    echo "$file modified, original saved in $file.orig"
	else
	    rm $file.modif
	fi
    done
    echo "Rerun $0 without options to restore the original files"
else
    for file in `find . -name \*.F90.orig`; do
	mv $file ${file%.orig}
	echo "$file restored"
    done
    echo "Run $o -f to convert the files to the ISO format"
fi
