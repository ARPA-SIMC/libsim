Summary: libsim: librerie di utilità in Fortran 90
Name: libsim
Version: 2.6.8
Release: 1
License: GPL
Group: Applications/Meteo
URL: http://www.arpa.emr.it/sim
Packager: Davide Cesari <dcesari@arpa.emr.it>
Source: %{name}-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-buildroot
BuildRequires: shapelib-fortran-devel oracle-instantclient-devel libemos libdballef-devel >= 4.0.6 grib_api-devel >= 1.7.0
Requires: libdballef4 >= 4.0.6 grib_api >= 1.7.0

#temporaneo
%define _fmoddir       %{_libdir}/gfortran/modules


%package -n libsim-doc
Summary: libsim documentation
Group: Applications/Meteo

%description -n libsim-doc
Librerie di utilità in Fortran 90, documentazione .

%description
Libsim comprende tre gruppi di moduli di utilità in Fortran 90:

libsim_base definisce moduli e classi di uso generale in applicazioni
scientifiche, come la gestione di errori in esecuzione, la gestione di
dati georeferenziati, di coordinate temporali, ecc.

libsim_grib definisce una serie di classi ad alto livello stratificate
sopra la libreria ECMWF emos per gestire l'I/O di file in formato grib.

libsim_vol7d definisce una serie di classi per facilitare
l'elaborazione di dati osservativi idro-meteo, includendo metodi per
la loro importazione da database tipo DbAll-e e dal database Oracle di
ARPA-SIM.

%prep
%setup -q

%build
%configure FC=gfortran FCFLAGS="-I/usr/include/ -I%{_fmoddir}" ORA_VER=oracle/11.1.0.1/client
make 

%install
make DESTDIR=%{buildroot} install

%files
%defattr(-, root, root)
%{_libdir}/*.a
%{_libdir}/*.la
%{_libdir}/*.so*
%{_includedir}/*
%{_bindir}/*
%{_datadir}/%{name}/*
#%{_docdir}/%{name}/*
%doc examples/*.f90

%files -n libsim-doc
%defattr(-,root,root,-)
%doc %{_docdir}/libsim/html

%clean
rm -rf %{buildroot}

%pre

%post

%preun

%postun

%changelog
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

