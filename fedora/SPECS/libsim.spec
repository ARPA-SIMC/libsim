# run rpmbuild with arguments --define='with_oracle 1' --define='with_vapor 1'
# to enable oracle and/or vapor support requiring stiff dependencies
Summary: Fortran utility libraries
Name: libsim
Version: 6.2.8
Release: 3
License: GPL2+
Group: Applications/Meteo
URL: https://github.com/arpa-simc/%{name}
Packager: Davide Cesari <dcesari@arpae.it>
Source: https://github.com/arpa-simc/%{name}/archive/v%{version}-%{release}.tar.gz#/%{name}-%{version}-%{release}.tar.gz  
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot

%if 0%{?fedora} < 9
%define _fmoddir %{_libdir}/gfortran/modules
%endif

%if 0%{?fedora} <= 24
# grib_api is used only on older fedoras
%define grib_sw grib_api
%else
%define grib_sw eccodes
BuildRequires: eccodes-simc
%endif

# expliciting eccodes for centos7
%if 0%{?el7}
%define grib_sw eccodes
BuildRequires: eccodes-simc
%endif

%{?with_oracle:BuildRequires: oracle-instantclient-devel}
%{?with_vapor:BuildRequires: vapor-devel}

BuildRequires: libdballef-devel >= 7.6 %{grib_sw}-devel ncl-devel gdal-devel libdballe-devel help2man log4c log4c-devel
BuildRequires: doxygen graphviz texlive-latex-bin texlive-dvips-bin
BuildRequires: libtool cnf-devel libpng-devel fortrangis-devel netcdf-fortran-devel shapelib-devel jasper-devel proj-devel popt-devel openjpeg-devel cairo-devel
Requires: libdballef4 >= 7.6 %{grib_sw}


%package -n libsim-devel

%{?with_oracle:Requires: oracle-instantclient-devel}
Requires: fortrangis-devel libdballef-devel >= 7.6 %{grib_sw}-devel ncl-devel gdal-devel libdballe-devel help2man log4c log4c-devel
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
%setup -q -n %{name}-%{version}-%{release}
sh autogen.sh

%build

%configure FCFLAGS="%{optflags} -I%{_fmoddir}" ORACLE_VER=oracle/11.2/client %{!?with_oracle:--disable-oraclesim} --enable-f2003-features %{?with_vapor:--enable-vapor} --enable-alchimia --enable-shapelib --enable-netcdf --enable-gribapi --enable-gdal --enable-f2003-extended-features --disable-static

make
make check

%install
make DESTDIR=%{buildroot} install
%if 0%{?fedora} >= 9 || 0%{?rhel}
mkdir -p $RPM_BUILD_ROOT%{_fmoddir}
mv $RPM_BUILD_ROOT%{_includedir}/*.mod $RPM_BUILD_ROOT%{_fmoddir}
%endif

%files
%defattr(-,root,root)
%{_libdir}/*.so.*
%{_bindir}/*
%{_datadir}/%{name}/*
%{_mandir}/man1

%files -n libsim-devel
%defattr(-,root,root)
%exclude %{_libdir}/*.la
%{_libdir}/*.so
%{_libdir}/pkgconfig/%{name}.pc
%if 0%{?fedora} >= 9 || 0%{?rhel}
%{_fmoddir}/*.mod
%else
%{_includedir}/*
%endif
%{?with_vapor:%{_includedir}/vdf4f_c.h}

%files -n libsim-doc
%defattr(-,root,root)
%doc examples/*.f90
%doc %{_docdir}/%{name}/html

%clean
rm -rf %{buildroot}

%changelog
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

