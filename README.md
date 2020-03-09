LIBSIM
======

[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/libsim?branch=master&env=DOCKER_IMAGE=centos:7&label=centos7)](https://travis-ci.org/ARPA-SIMC/libsim)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/libsim?branch=master&env=DOCKER_IMAGE=centos:8&label=centos8)](https://travis-ci.org/ARPA-SIMC/libsim)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/libsim?branch=master&env=DOCKER_IMAGE=fedora:30&label=fedora30)](https://travis-ci.org/ARPA-SIMC/libsim)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/libsim?branch=master&env=DOCKER_IMAGE=fedora:31&label=fedora31)](https://travis-ci.org/ARPA-SIMC/libsim)
[![Build Status](https://badges.herokuapp.com/travis/ARPA-SIMC/libsim?branch=master&env=DOCKER_IMAGE=fedora:rawhide&label=fedorarawhide)](https://travis-ci.org/ARPA-SIMC/libsim)

[![Build Status](https://copr.fedorainfracloud.org/coprs/simc/stable/package/libsim/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/simc/stable/package/libsim/)

Introduction
------------

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

Build and dependencies
----------------------

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

Documentation
-------------

The documentation for all command-line tools can be found in their
manpage.  All command-line tools also have extensive command-line help
that can be accessed using the "--help" option.

The Fortran API documentation is not available online at the moment,
it can be generated in the package build process through doxygen.


Contact and copyright information
---------------------------------

The authors of libsim are:
Davide Cesari <dcesari@arpa.emr.it>
Paolo Patruno <ppatruno@arpa.emr.it>

libsim is Copyright (C) 2010-2020  ARPAE-SIMC <urpsim@arpae.it>

Libsim is licensed under the terms of the GNU General Public License version
2 or successive.  Please see the file COPYING for details.

Contact informations for ARPAE-SIMC (formerly ARPA-SIM):

  Agenzia Regionale Prevenzione Ambiente e Energia dell'Emilia-Romagna (ARPAE)
  Servizio Idro-Meteo-Clima (SIMC)

  Address: Viale Silvani 6, 40122 Bologna, Italy
  Tel: + 39 051 6497511
  Fax: + 39 051 6497501
  Email: urpsim@arpae.it
  Website: http://arpae.it/sim/
