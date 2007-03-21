dnl Process this file with autoconf to produce a configure script.

AC_INIT(libsim, 1.0, [dcesari@arpa.emr.it])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])

AC_LANG([C])
AC_C_BIGENDIAN

dnl Locate Fortran 90 compiler among these if not specified
AC_PROG_FC([pgf90 ifort gfortran g95 xlf90 f90])
F77=$FC
dnl AC_FC_WRAPPERS()dnl Determine how to call C from Fortran

dnl AC_LANG_PUSH([Fortran 77])dnl Do the following checks in Fortran
AC_LANG_PUSH(Fortran)dnl Do the following checks in Fortran
AC_FC_SRCEXT(f90)dnl and use .f90 extension


dnl  -L/usr/local/oracle/10.1.0/db_1/lib  -lclntsh \
dnl /usr/lib/librouMW.a  /usr/lib/librouGW.a /usr/lib/libix.a \
dnl  /usr/lib/libsupport.a

dnl AC_SEARCH_LIBS([gribex], [picogrib emos], ,
dnl                          AC_MSG_RESULT([grib decoding library not found]), )


AC_LANG_POP()dnl back to C
AC_PROG_LIBTOOL

pkgdocdir='${datadir}/doc/${PACKAGE}'
AC_SUBST(pkgdocdir)

AC_CONFIG_FILES([
Makefile
base/Makefile
vol7d/Makefile
examples/Makefile
])

AC_OUTPUT
