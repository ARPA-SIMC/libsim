Summary: libsim: librerie di utilità in Fortran 90
Name: libsim
Version: 6.1.3
Release: 1%{dist}
License: GPL2+
Group: Applications/Meteo
URL: http://www.arpa.emr.it/sim
Packager: Davide Cesari <dcesari@arpa.emr.it>
Source: %{name}-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-buildroot
BuildRequires: fortrangis-devel oracle-instantclient-devel libdballef-devel >= 7.6 grib_api-devel ncl-devel gdal-devel libdballe-devel help2man log4c cnf-devel libpng-devel vapor-devel fortrangis-devel netcdf-fortran-devel shapelib-devel
Requires: libdballef4 >= 7.6 grib_api

#temporaneo
%if 0%{?fedora} < 9
%define _fmoddir       %{_libdir}/gfortran/modules
%endif

%package -n libsim-doc
Summary:  libsim documentation
Group: Applications/Meteo

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
%setup -q

%build

%configure FCFLAGS="%{optflags} -I%{_fmoddir}" ORACLE_VER=oracle/11.2/client --enable-f2003-features --enable-vapor --enable-alchimia --enable-shapelib --enable-netcdf --enable-gribapi --enable-gdal --enable-f2003-extended-features

make 

%install
make DESTDIR=%{buildroot} install
%if 0%{?fedora} >= 9
mkdir -p $RPM_BUILD_ROOT%{_fmoddir}
mv $RPM_BUILD_ROOT%{_includedir}/*.mod $RPM_BUILD_ROOT%{_fmoddir}
%endif

%files
%defattr(-, root, root)
%{_libdir}/*.a
%{_libdir}/*.la
%{_libdir}/*.so*
%{_libdir}/pkgconfig/%{name}.pc
%if 0%{?fedora} < 9
%{_includedir}/*
%else
%{_fmoddir}/*.mod
%endif
%{_bindir}/*
%{_datadir}/%{name}/*
#SOLO PER VAPOR:
%{_includedir}/vdf4f_c.h
%doc examples/*.f90
%{_mandir}/man1

%files -n libsim-doc
%defattr(-,root,root,-)
%doc %{_docdir}/%{name}/html

%clean
rm -rf %{buildroot}

%changelog
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

