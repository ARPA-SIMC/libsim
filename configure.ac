dnl Process this file with autoconf to produce a configure script.

AC_INIT(libsim, 2.0, [dcesari@arpa.emr.it])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])

AC_LANG([C])
AC_C_BIGENDIAN
AC_CHECK_SIZEOF(void *)

if test "x$ac_cv_sizeof_void_p" = "x1"; then
  sizeof_ptr_c="int_b"
elif test "x$ac_cv_sizeof_void_p" = "x2"; then
  sizeof_ptr_c="int_s"
elif test "x$ac_cv_sizeof_void_p" = "x4"; then
  sizeof_ptr_c="int_l"
elif test "x$ac_cv_sizeof_void_p" = "x8"; then
  sizeof_ptr_c="int_ll"
else
  sizeof_ptr_c="__error__"
fi
AC_DEFINE_UNQUOTED(SIZEOF_PTR_C, $sizeof_ptr_c,
 "Fortran KIND defining an INTEGER containing a C pointer")

dnl Locate Fortran 90 compiler among these if not specified
AC_PROG_FC([gfortran pgf90 ifort g95 xlf90 f90])
F77=$FC
dnl AC_FC_WRAPPERS()dnl Determine how to call C from Fortran

dnl search for log4c, if not found build a dummy log4fortran
AC_CHECK_HEADERS([log4c.h] ,
  AC_CHECK_LIB([log4c], [log4c_init], , AC_MSG_ERROR([log4c broken!])),
  AC_MSG_RESULT([Building dummy log4fortran])
  dummy_log4fortran=yes
 )

if test "x$dummy_log4fortran" = "x"; then
AC_CHECK_HEADERS([f77.h] ,
  AC_CHECK_LIB([cnf], [cnfCptr],
 , AC_MSG_ERROR([cnf broken!])),
  AC_MSG_RESULT([Building dummy log4fortran])
  dummy_log4fortran=yes
 )
fi
AM_CONDITIONAL(LOG4FORTRAN, test "x$dummy_log4fortran" != "xyes")

dnl AC_LANG_PUSH([Fortran 77])dnl Do the following checks in Fortran
AC_LANG_PUSH(Fortran)dnl Do the following checks in Fortran
AC_FC_SRCEXT(f90)dnl and use .f90 extension


AC_ARG_ENABLE(shapelib, AC_HELP_STRING([--disable-shapelib],
  [disable shapelib support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([shp-fortran], [shpopen], 
 AC_MSG_RESULT([shapelib code included])
  libshp="-lshp-fortran -lshp"
  AC_SUBST(libshp),
 AC_MSG_ERROR([fortran bindings to shp library not found]), [-lshp] )
else
AC_MSG_RESULT([shapelib code not included])
fi
AM_CONDITIONAL(SHP, test "x$enableval" = "xyes")

AC_ARG_ENABLE(oraclesim, AC_HELP_STRING([--disable-oraclesim],
  [disable oraclesim/meteodata support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
if test -n "$ORACLE_HOME"; then
oralibdir="-L$ORACLE_HOME/lib"
else
oralibdir=" "
fi
AC_CHECK_LIB([rouMW], [n_getgsta], 
 AC_MSG_RESULT([oraclesim code included])
  liboraclesim="$oralibdir -lrouMW -lclntsh -lsupport -lrouGW -lix"
  AC_SUBST(liboraclesim),
 AC_MSG_ERROR([oraclesim/meteodata libraries not found]),
 [$oralibdir -lclntsh -lsupport -lrouGW -lix])
else
AC_MSG_RESULT([oraclesim code not included])
fi
AM_CONDITIONAL(ORSIM, test "x$enableval" = "xyes")

AC_ARG_ENABLE(grib, AC_HELP_STRING([--disable-grib],
  [disable grib support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([emos], [gribex],
 AC_MSG_RESULT([grib code included])
  libgrib="-lemos"
  AC_SUBST(libgrib),
 AC_MSG_ERROR([ECMWF emos library not found]))
else
AC_MSG_RESULT([grib code not included])
fi
AM_CONDITIONAL(GRIB, test "x$enableval" = "xyes")

AC_ARG_ENABLE(dballe, AC_HELP_STRING([--disable-dballe],
  [disable dballe support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_MSG_RESULT([dballe code included])
libdballe="-ldballef -ldballe-core -lcnf -lodbc -lgdbm -lpopt -lm"
AC_SUBST(libdballe)
else
AC_MSG_RESULT([dballe code not included])
fi
AM_CONDITIONAL(DBALLE, test "x$enableval" = "xyes")

AC_LANG_POP()dnl back to C
AC_PROG_LIBTOOL

pkgdocdir='${datadir}/doc/${PACKAGE}'
AC_SUBST(pkgdocdir)

AC_CONFIG_FILES([
Makefile
base/Makefile
grib/Makefile
vol7d/Makefile
examples/Makefile
data/Makefile
qc/Makefile
termolib/Makefile
doc/Doxyfile
doc/Makefile
log4fortran/Makefile
])

AC_OUTPUT
