# rpmbuild with argument --define='with_vapor 1'
# to enable vapor support requiring stiff dependencies

# Note: define srcarchivename in CI build only.
%{!?srcarchivename: %global srcarchivename %{name}-%{version}-%{release}}

Summary: Fortran utility libraries
Name: libsim
Version: 7.2.5
Release: 1
License: GPL2+
Group: Applications/Meteo
URL: https://github.com/arpa-simc/%{name}
Packager: Davide Cesari <dcesari@arpae.it>
Source: https://github.com/arpa-simc/%{name}/archive/v%{version}-%{release}.tar.gz#/%{srcarchivename}.tar.gz

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot

%{?with_vapor:BuildRequires: vapor-devel}

BuildRequires: pkgconfig(libdballef) >= 7.6
BuildRequires: pkgconfig(libdballe)
BuildRequires: eccodes-devel
BuildRequires: eccodes-simc
BuildRequires: help2man
BuildRequires: log4c log4c-devel
BuildRequires: gdal-devel
BuildRequires: ncl-devel
%if 0%{?rhel} == 7
# ncl-devel needs cairo-devel but the dependency is missing in CentOS 7
BuildRequires: cairo-devel
%endif
BuildRequires: doxygen
BuildRequires: graphviz
BuildRequires: texlive-latex-bin
BuildRequires: texlive-dvips-bin
BuildRequires: texlive-iftex
BuildRequires: texlive-metafont
BuildRequires: texlive-cm
%{?fedora:BuildRequires: texlive-lwarp}
BuildRequires: gcc-c++
BuildRequires: libtool
BuildRequires: fortrangis-devel
BuildRequires: netcdf-fortran-devel
BuildRequires: shapelib-devel
BuildRequires: proj-devel
BuildRequires: popt-devel
BuildRequires: freetype-devel
Requires: eccodes

%package -n libsim-devel

Requires: fortrangis-devel
Requires: libdballef-devel >= 7.6
Requires: libdballe-devel
Requires: eccodes-devel
Requires: help2man
Requires: log4c
Requires: log4c-devel
Requires: gdal-devel
Requires: ncl-devel
Requires: freetype-devel

Summary:  libsim development files
Group: Applications/Meteo

%package -n libsim-doc
Summary:  libsim documentation
Group: Applications/Meteo

%description -n libsim-devel
Libsim is a collection of Fortran libraries and command-line tools.

This package contains the development files, necessary for building
applications using libsim.

%description -n libsim-doc
Libsim is a collection of Fortran libraries and command-line tools.

This package contains the doxygen documentation for libsim.

%description
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

%prep
%setup -q -n %{srcarchivename}
sh autogen.sh

%build

%configure FCFLAGS="%{optflags} -I%{_fmoddir}" %{?with_vapor:--enable-vapor} --enable-alchimia --enable-shapelib --enable-netcdf --enable-gribapi --disable-static --enable-gdal

make
make check

%install
[ "%{buildroot}" != / ] && rm -rf %{buildroot}
%makeinstall
mkdir -p $RPM_BUILD_ROOT%{_fmoddir}
mv $RPM_BUILD_ROOT%{_includedir}/*.mod $RPM_BUILD_ROOT%{_fmoddir}

%files
%defattr(-,root,root)
%{_libdir}/*.so.*
%{_bindir}/*
%{_datadir}/%{name}/*
%{_mandir}/man1
%dir %{_libexecdir}/%{name}
%{_libexecdir}/%{name}/*

%files -n libsim-devel
%defattr(-,root,root)
%exclude %{_libdir}/*.la
%{_libdir}/*.so
%{_libdir}/pkgconfig/%{name}.pc
%{_fmoddir}/*.mod
%{?with_vapor:%{_includedir}/vdf4f_c.h}

%files -n libsim-doc
%defattr(-,root,root)
%doc examples/*.f90
%doc %{_docdir}/%{name}/html

%clean
rm -rf %{buildroot}

%changelog
* Thu Jun 26 2025 Davide Cesari <dcesari@arpae.it> - 7.2.5-1
- extend OpenMP parallelisation
- introduce time_definition=2 for converting to analysis

* Thu Mar 20 2025 Davide Cesari <dcesari@arpae.it> - 7.2.4-1
- extend use of comp_start also to forecasts

* Mon Jan 13 2025 Davide Cesari <dcesari@arpae.it> - 7.2.3-1
- add compute_projgrib utility
- improve intersearch interpolation

* Thu Sep  5 2024 Daniele Branchini <dbranchini@arpae.it> - 7.2.1-1
- avoid error with typeOfTimeIncrement 255
- add intersearch interpolation (with real nearest point)
- take into account Icon vertical level setup for surface orography field

* Tue Jun 11 2024 Daniele Branchini <dbranchini@arpae.it> - 7.2.0-1
- add computation of total solid precipitation (B13237)
- use difference rather than aggregation method when there are already data on the desired interval
- added/fixed grib2 entries (#112)
- allow stat_proc to be different for input and output also for gridded data in aggregation method
- removed alchimia option (always enabled)
- remove dba_qcfilter from Makefile (#81)

* Wed Feb 21 2024 Daniele Branchini <dbranchini@arpae.it> - 7.1.11-1
- Intelligently merge volumes after stat_proc in order not to loose precious data

* Mon Jan 15 2024 Daniele Branchini <dbranchini@arpae.it> - 7.1.10-1
- Added grib2 cloud cover
- Fixed value of saturated water vapour pressure at 0C and improve computation of Td
- Dropped support for fedora <=24

* Tue Dec  5 2023 Daniele Branchini <dbranchini@arpae.it> - 7.1.9-1
- Added theta-e for grib2 (#109)
- Flatten network in volumes used as interpolation list to avoid losing points
- Add 4 new transformations lemaskvalid, ltmaskvalid, etc for ICON zero deg isotherm height

* Thu Nov 23 2023 Daniele Branchini <dbranchini@arpae.it> - 7.1.8-1
- Set correct timerange when rounding

* Mon Sep 25 2023 Daniele Branchini <dbranchini@arpae.it> - 7.1.7-1
- Workaround for gfortran 13
- Code modernization and cleanup
- Added grib2 entries for solar radiation
- Do not package dba_qcfilter (#105)

* Tue Mar 14 2023 Davide Cesari <dcesari@arpae.it> - 7.1.6-1
- bug fix

* Thu Mar 2 2023 Davide Cesari <dcesari@arpae.it> - 7.1.5-1
- fix another minor bug in derived variables
- initial basic handling of unstructured grids

* Thu Feb 23 2023 Daniele Branchini <dbranchini@arpae.it> - 7.1.4-1
- Fixed minor bugs in processing of levels and computation of derived variables
- Implemented coding of Mercator grid anche check for edition independence

* Tue Jan 31 2023 Daniele Branchini <dbranchini@arpae.it> - 7.1.3-1
- Implemented different styles of grib2 analysis output
- Implemented grib spherical Mercator projection
- Added JSON format in vg6d_getpoint_pkauf

* Mon Sep  5 2022 Daniele Branchini <dbranchini@arpae.it> - 7.1.2-1
- Allow averaging from any stat-proc for sparse data only

* Wed Jun 15 2022 Daniele Branchini <dbranchini@arpae.it> - 7.1.1-1
- fix bugs in cumulation by differences for gridded fields (#99)

* Fri Jun  3 2022 Daniele Branchini <dbranchini@arpae.it> - 7.1.0-1
- Added json format for sparse point data
- Refactored statistical processing of intervals (#98)
- Added a comprehensive stat_proc_test
- Added grib2 entries for variuos precipitation fractions
- Implement a filter on some metadata for generic vol7d volumes, now also native format can be filtered in input in v7d_transform

* Wed Apr  6 2022 Daniele Branchini <dbranchini@arpae.it> - 7.0.2-2
- Added wet bulb temperature in vargrib2bufr

* Tue Mar 15 2022 Daniele Branchini <dbranchini@arpae.it> - 7.0.2-1
- specify input format in v7d_pollini by namelist, default remains BUFR
- revert wrong modification on analysis-like end of processing period
- miscellaneous improvements to autotools files

* Thu Mar  3 2022 Daniele Branchini <dbranchini@arpae.it> - 7.0.1-1
- add `--comp-var-from-lev` option to vg6d_transform
- remove indirect dependencies from spec and from configure.ac (#90)

* Thu Jan 27 2022 Daniele Branchini <dbranchini@arpae.it> - 7.0.0-1
- major change in .so versioning
- updated sea variables in vargrib2bufr.csv and improved doc
- convert all station data to real in vol7d_convr if requested (#69)
- more aggressive algorithm for the aggregation of forecasts with time_definition=1 (verification time) (#43)
- add checks for latex and dvips (#57)
- avoid paroblem with second order grib packaging (#66)
- equalize grid coordinates for some sparse point to grid transformations and vice versa
- improve managing of periodicity in in/out grids
- implement full interpretaion of the various "accumulated analyses" grib2 conventions
- Various minor bug fixes

* Tue Nov  9 2021 Daniele Branchini <dbranchini@arpae.it> - 6.5.3-1
- implement perentile post-transformation
- fix computation of polygon centroid when it's already closed
- add grib2 soil temperature

* Wed May 26 2021 Daniele Branchini <dbranchini@arpae.it> - 6.5.2-1
- fix computation of prevailing wind direction, exclude wind calms

* Tue May 25 2021 Daniele Branchini <dbranchini@arpae.it> - 6.5.1-1
- add global (sw*lw) radiation and corresponding functions

* Thu May 13 2021 Daniele Branchini <dbranchini@arpae.it> - 6.5.0-1
- refactoring removing F2003_FEATURES and legacy sources
- added UV index and grib2 wind gust entries
- cleaned up optionparser (#83)

* Tue Jan 26 2021 Daniele Branchini <dbranchini@arpae.it> - 6.4.10-1
- fix for reading cosmo grib2 accumulated analyses
- add maskgen:grid transformation
- when start of comp_stat period non specified, force it to be modulo step
- implement full_steps also for time processing by aggregation

* Tue Dec 15 2020 Daniele Branchini <dbranchini@arpae.it> - 6.4.9-1
- fixed reading error in simc_nc2grib
- add volgrid6d_compute_vert_coord_var subrotuine
  for converting vertical coordinate to a volgrid volume
- add surface lifted index

* Wed Dec  9 2020 Daniele Branchini <dbranchini@arpae.it> - 6.4.8-1
- added netcdf to grib utility (simc_nc2grib)
- added sea level, salinity, tidal elevation to vargrib2bufr.csv

* Thu Oct 29 2020 Daniele Branchini <dbranchini@arpae.it> - 6.4.7-1
- add web bulb temperature function
- added variables to vargrib2bufr.csv
- add temptative multivariate stat proc

* Wed Sep 23 2020 Emanuele Di Giacomo <edigiacomo@arpae.it> - 6.4.6-1
- enable json format for dballe, fix bug disabling formats other than BUFR
- consider timerange 200 as timerange 0 for statistical processing

* Fri Jun  5 2020 Daniele Branchini <dbranchini@arpae.it> - 6.4.5-1
- handle ECMWF 3 hour min/max temperatures (#84)

* Tue May 12 2020 Daniele Branchini <dbranchini@arpae.it> - 6.4.4-1
- Updated vargrib2bufr, fixed compiler errors for F32

* Tue Mar 17 2020 Emanuele Di Giacomo <edigiacomo@arpae.it> - 6.4.3-4
- Exclude dba_qcfilter command from package

* Thu Jan 23 2020 Daniele Branchini <dbranchini@arpae.it> - 6.4.3-3
- CentOS 8 build: re-enabling gdal and ncl dependencies

* Mon Dec  9 2019 Daniele Branchini <dbranchini@arpae.it> - 6.4.3-2
- fixed texlive dependencies

* Mon Oct  7 2019 Daniele Branchini <dbranchini@arpae.it> - 6.4.2-1
- fixed but in metamorphosis:mask
- improve and extend handling of level numeric conversion in import/export and vertint

* Thu Sep 26 2019 Daniele Branchini <dbranchini@arpae.it> - 6.4.1-1
- fixed bug on statistical processing (#75)
- adapted specfile for centos 8 builds

* Wed Jul 17 2019 Davide Cesari <dcesari@arpae.it> - 6.4.0-1
- new tools vg6d_getpoint_pkauf and vg6d_tcorr
- move specific tools in libexec

* Wed Feb 27 2019 Daniele Branchini <dbranchini@arpae.it> - 6.3.1-1
- improvement of tesat, implementation of termolib tests
- add functions for air density and virtual temperature
- add lwc method for computing visibility

* Thu Dec 6 2018 Davide Cesari <dcesari@arpae.it> - 6.3.0-1
- refactoring of log4c interface, remove dependency on cnf

* Thu Nov 29 2018 Davide Cesari <dcesari@arpae.it> - 6.2.9-1
- removed support for oracle@sim

* Thu Sep 13 2018 Daniele Branchini <dbranchini@arpae.it> - 6.2.8-2
- fixed issue in eccodes support (see #52)

* Fri Jun 15 2018 Davide Cesari <dcesari@arpae.it> - 6.2.8-1
- implement frequency sub-type, closes #54

* Mon May 7 2018 Daniele Branchini <dbranchini@arpae.it> - 6.2.7-1
- enabled eccodes support
- reversed oracle and vapor flags (disabled by default)
- fixed #55

* Mon Mar 26 2018 Davide Cesari <dcesari@arpae.it> - 6.2.6-1
- fix bug with bufr overflow and improve UTM precision

* Tue Feb 27 2018 Daniele Branchini <dbranchini@arpae.it> - 6.2.5-1
- allow processing period to start before reference time in forecast mode, closes #50

* Wed Feb 14 2018 Daniele Branchini <dbranchini@arpae.it> - 6.2.4-1
- fix v7d_dballe import with multiple networks specified, close #49
- fix bug in maskinvalid transformation
- fix in file_utilities and improvement in the search algorithm for coord-file
- deprecate vg6d_subarea and replace it with a shell script
- add alchemy method for computing visibility with Boudala formula
- swan products for centre 80 in vargrib2bufr

* Tue Nov 21 2017 Daniele Branchini <dbranchini@arpae.it> - 6.2.2-1
- Centre 80 for COSMO and ECMWF entries (fix #38)

* Tue Oct 24 2017 Davide Cesari <dcesari@arpae.it> - 6.2.1-1
- minor fixes and documentation

* Wed Aug 30 2017 Davide Cesari <dcesari@arpae.it> - 6.2.0-1
- new statistical processing approach
- Shapiro transform
- various minor fixes

* Tue Apr 4 2017 Daniele Branchini <dbranchini@arpae.it> - 6.1.15-1
- allow computing max and min from any input stat-proc
- allow simple rot/unrot of wind components
- memory/time optimization in common case
- improved doc

* Wed Feb 22 2017 Davide Cesari <dcesari@arpae.it> - 6.1.14-1
- fixed #33 #34, new ECMWF wind gust and bug on long timeranges
- fixes for use of stdin/stdout
- other minor bugfixes

* Tue Jan 3 2017 Daniele Branchini <dbranchini@arpae.it> - 6.1.13-1
- fixed #27
- allow array for --lon and --lat + pretty printing of default
- add time-independent option in volgrid and in vg6d_transform

* Thu Nov 10 2016 Daniele Branchini <dbranchini@arpae.it> - 6.1.12-1
- fixed #30, #31
- other fixes in quality control modules

* Wed Oct 12 2016 Daniele Branchini <dbranchini@arpae.it> - 6.1.11-1
- Implement time integration/differentiation for sparse data with --comp-stat-proc=0:1 or --comp-stat-proc=1:0.
- Allow to convert identically radiation variables, from B14021 to B14198 and viceversa

* Thu Sep 15 2016 Daniele Branchini <dbranchini@arpae.it> - 6.1.10-1
- better confidence computatin in qctem
- fixed #23
- do not alter data on export in output

* Mon Jul 11 2016 Daniele Branchini <dbranchini@arpae.it> - 6.1.7-1
- changes following dballe issue #61 (removing cnf)

* Mon Jun 20 2016 dbranchini <dbranchini@arpa.emr.it> - 6.1.6-1
- merged qcspa branch (quality control improvements)

* Mon Feb 22 2016 dcesari <dcesari@arpa.emr.it> - 6.1.5-1
- prefer seconds for timedelta to avoid problems with long timeranges
- timedelta can be initialised in sec as well
- fix bitmap key in grib2
- handle better missing values in vertical levels
- correct order for B14018
- handle correctly stdin and stdout in dballe_class
- implemented n-1 variant of variance and stddev, variance guaranteed >=0

* Mon Jan 18 2016 dcesari <dcesari@arpa.emr.it> - 6.1.4-1
- devel package
- stddev subtype in stencilinter type

* Wed Dec 2 2015 dbranchini <dbranchini@arpa.emr.it> - 6.1.3-1%{dist}
- no querybest in ana
- sort level and timerange after rounding, fix timerange 254 in rounding

* Mon Nov 23 2015 dbranchini <dbranchini@arpa.emr.it> - 6.1.2-1%{dist}
- fix bug on endofftimeinterval, avoid timerange 206 which is now valid

* Tue Oct 27 2015 dbranchini <dbranchini@arpa.emr.it> - 6.1.1-1%{dist}
- fixed dba_qcfilter bug

* Thu Oct 1 2015 dbranchini <dbranchini@arpa.emr.it> - 6.1.0-1505%{dist}
- implemented heap sort for all vol7d objects
- stdin/stdout in dballe fortran api change
- fix for grib2 output on Lamber conformal
- use qc without data_id
- new function in termo for specific humidity
- fix in termo for missing values when scaled values

* Thu Feb 12 2015 dbranchini <dbranchini@arpa.emr.it> - 6.0.1-1462%{dist}
- Fixed too short dns buffer
- Fixed not initialized list in v7d2dba and other not initialized var in vol7d_dballe_init

* Wed Jan 16 2013 dcesari <dcesari@arpa.emr.it> - 5.0.0-1215%{dist}
- New version requiring latest vapor version

* Thu May 31 2012 dbranchini <dbranchini@arpa.emr.it> - 4.5.0-1128%{dist}
- aggiunta gestione (correzione) campi cumulati ecmwf

* Mon May 14 2012 dbranchini <dbranchini@arpa.emr.it> - 4.5.0-1121%{dist}
- modifiche per variabili leaf wetness e evapotranspiration.

* Tue Mar 17 2009 root <root@strip.metarpa> - 2.6.7-1
- cambiato il mone degli eseguibili con prefix vg6d_

* Mon Jul  7 2008 Davide Cesari <cesari@malina.metarpa> - 2.2-1
- New version, -I/usr/include now as FCFLAGS, ORA_VER added

* Wed Apr 16 2008 Davide Cesari <cesari@malina.metarpa> - 2.1-1
- New version

* Fri Jan 18 2008 Davide Cesari <cesari@malina.metarpa> - 1.4-2
- Add search path for oracle libraries.

* Thu Jan 17 2008 Davide Cesari <cesari@malina.metarpa> - 1.4-1
- New version.

* Mon Dec 10 2007 Davide Cesari <cesari@malina.metarpa> - 1.3-2
- Fixed makeinstall for Fedora 8.

* Thu Nov 22 2007 Davide Cesari <cesari@malina.metarpa> - 1.2-1
- Nuova versione

* Fri Jun 15 2007 Davide Cesari <cesari@malina.metarpa> - 1.1-1
- Prima pacchettizzazione su Fedora Core 4

