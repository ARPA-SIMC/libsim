Summary: libsim: librerie di utilità in Fortran 90
Name: libsim
Version: 1.1
Release: 1
License: GPL
Group: Development
URL: http://www.arpa.emr.it/sim
Packager: Davide Cesari <ppatruno@arpa.emr.it>
Source: %{name}-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-buildroot
BuildRequires: shapelib-fortran

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
FC=pgf90 %configure
make 

%install
%makeinstall


%files
%defattr(-, root, root)
%{_libdir}/*.a
%{_libdir}/*.la
%{_libdir}/*.so*
%{_includedir}/*
%{_bindir}/*
%{_datadir}/%{name}/*
#%{_docdir}/%{name}/*

%clean
rm -rf %{buildroot}

%pre

%post

%preun

%postun

%changelog
* Fri Jun 15 2007 Davide Cesari <cesari@malina.metarpa> - 1.1-1
- Prima pacchettizzazione su Fedora Core 4

