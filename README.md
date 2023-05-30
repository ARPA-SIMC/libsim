[![Build Status](https://simc.arpae.it/moncic-ci/libsim/centos7.png)](https://simc.arpae.it/moncic-ci/libsim/)
[![Build Status](https://simc.arpae.it/moncic-ci/libsim/centos8.png)](https://simc.arpae.it/moncic-ci/libsim/)
[![Build Status](https://simc.arpae.it/moncic-ci/libsim/fedora36.png)](https://simc.arpae.it/moncic-ci/libsim/)
[![Build Status](https://copr.fedorainfracloud.org/coprs/simc/stable/package/libsim/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/simc/stable/package/libsim/)

# LIBSIM

## Introduction

Libsim is a collection of Fortran libraries and command-line tools.

The libraries include a general purpose ''base'' library with modules
for handling character variables, csv files, command-line arguments,
physical constants, date and time computations, georeferenced
coordinates, growable arrays and list structures of any type and
other.

Another set of libraries is specific to Meteorology and Earth Science
and allows to work with gridded and sparse georeferenced data, perform
interpolations, statistical processing in time, data quality control,
thermodynamic computations.

The ready-to-use command-line tools allow to perform many kinds of
space interpolations and time computations on georeferenced data in
GRIB and BUFR format.

Libsim strongly relies on
[DB-All.e](https://github.com/ARPA-SIMC/dballe)/[wreport](https://github.com/ARPA-SIMC/dballe)
for handling sparse point data and on [ECMWF
grib_api](https://software.ecmwf.int/wiki/display/GRIB/Home) for
gridded data in GRIB format. It can also understand shapefiles and all
the Gdal-supported raster formats.

Libsim is written in modern object-oriented-style Fortran 90; since
its development started in 2006, only a few modules have been written
with real O-O F2003 syntax.

## Build and dependencies

The package is meant to be built on Linux with the GNU Compiler
Collection (gcc/gfortran, etc.).  With limitations, it can be built
also on other POSIX-compliant operating systems and/or with other
Fortran compilers.

The build process is not trivial and requires the installation of many
depending packages.

The [SMND](https://github.com/dcesari/smnd) package provides a
framework for compiling the main libsim dependencies and libsim
itself, as well as an universal binary package suitable to be
installed on a generic state-of-the-art x86_64 Linux system.

Versioning
----------

From version 7.0.0, libsim follows [semver 2.0](https://semver.org/) for API
and ABI changes. The library version (`-version-info`) follows the [libtools
conventions](https://www.gnu.org/software/libtool/manual/html_node/Updating-version-info.html).

Documentation
-------------

The documentation for all command-line tools can be found in their
manpage.  All command-line tools also have extensive command-line help
that can be accessed using the "--help" option.

The Fortran API documentation is not available online at the moment,
it can be generated in the package build process through doxygen.

## GRIB and BUFR variables association

The file [vargrib2bufr.csv](data/vargrib2bufr.csv) contains a descriptive 
association between WMO BUFR B table variables hosted in 
[dballe.txt](https://github.com/ARPA-SIMC/dballe/blob/master/tables/dballe.txt) 
and GRIB parameters.

The csv fields are:
 1. B CODE (in the form Bxxyyy, the numeric part corresponds to the first field of the [dballe.txt](https://github.com/ARPA-SIMC/dballe/blob/master/tables/dballe.txt) table)
 2. description
 3. GRIB parameter unit
 4. GRIB originating centre
 5. Table (GRIB1) / Category (GRIB2)
 6. Parameter
 7. Discipline (GRIB2) / 255 (GRIB1)
 8. Scale (to corresponding B CODE units)
 9. Offset (to corresponding B CODE units)

Additional notes:
- Multiple GRIB parameters can be associated to the same B table variable, but not vice versa (B codes are unique for each phisical variable).
- BUFR B Table variables are identified by the first field only, while GRIB variables are identified by fields 3 to 7).
- Values can be missing (empty string for char fields, 255 for numeric fields), and it will be interpreted as a wildcard.
- If field 5 is ≥ 128 then the orinating centre is considered mandatory and it can't be missing (255).
- If a variable is present both in GRIB1 and GRIB2 formats, it needs two separate records, one with missing Discipline (255) and one with GRIB2 Disicpline value.
- A local version of `vargrib2bufr.csv` can be used by assigning to the environment variable `LIBSIM_DATA` a path containing a modified version of the file.


## Contact and copyright information

The authors of libsim are:  
Davide Cesari <dcesari@arpae.it>  
Paolo Patruno <ppatruno@arpae.it>  

libsim is Copyright (C) 2010-2023  ARPAE-SIMC <urpsim@arpae.it>

Libsim is licensed under the terms of the GNU General Public License version
2 or successive.  Please see the file COPYING for details.

Contact informations for ARPAE-SIMC (formerly ARPA-SIM):

  Agenzia Regionale Prevenzione Ambiente e Energia dell'Emilia-Romagna (ARPAE)  
  Servizio Idro-Meteo-Clima (SIMC)  

  Address: Viale Silvani 6, 40122 Bologna, Italy  
  Tel: + 39 051 6497511  
  Fax: + 39 051 6497501  
  Email: urpsim@arpae.it  
  Website: https://www.arpae.it/
