dnl Process this file with autoconf to produce a configure script.

AC_INIT(libsim, 2.3, [dcesari@arpa.emr.it])
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
AC_FC_WRAPPERS()dnl Determine how to call C from Fortran

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

dnl end of test for log4c
if test "x$dummy_log4fortran" != "xyes" ; then
  AC_DEFINE_UNQUOTED(LOG4FORTRAN, TRUE,"Enable log4c")
fi
AM_CONDITIONAL(LOG4FORTRAN, test "x$dummy_log4fortran" != "xyes")

AC_ARG_ENABLE(oraclesim, AC_HELP_STRING([--disable-oraclesim],
  [disable oraclesim/meteodata support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
save_cppflags=$CPPFLAGS
ora_cppflags=''
ora_ldflags=''
if test -n "$ORACLE_VER"; then
  ora_cppflags="-I/usr/include/$ORACLE_VER"
  for try in $libdir/$ORACLE_VER/lib /usr/lib64/$ORACLE_VER/lib /usr/lib/$ORACLE_VER/lib; do
  if test -d "$try"; then
    ora_ldflags="-L$try"
    break
  fi
done
dnl elif test -n "ORACLE_HOME"; then che fare?
fi
ora_libs='-locci -lclntsh'

CPPFLAGS="$CPPFLAGS $ora_cppflags"
AC_CHECK_HEADER([oci.h],
 ORA_CPPFLAGS="$ora_cppflags",
 AC_MSG_ERROR([Oracle-OCI headers not found - try defining ORACLE_VER=oracle/<version>/client]))
CPPFLAGS=$save_cppflags

AC_CHECK_LIB([occi], [OCIEnvCreate],
ORA_LDFLAGS=$ora_ldflags
ORA_LIBS=$ora_libs
,AC_MSG_ERROR([Oracle-OCI libraries not found - try defining ORACLE_VER=oracle/<version>/client]),
  [$ora_ldflags $ora_libs])

AC_SUBST(ORA_CPPFLAGS)
AC_SUBST(ORA_LDFLAGS)
AC_SUBST(ORA_LIBS)
unset save_cppflags ora_cppflags ora_ldflags ora_lib
fi
AM_CONDITIONAL(ORSIM, test "x$enableval" = "xyes")

dnl AC_LANG_PUSH([Fortran 77])dnl Do the following checks in Fortran
AC_LANG_PUSH(Fortran)dnl Do the following checks in Fortran
AC_FC_SRCEXT(f90)dnl and use .f90 extension


AC_ARG_ENABLE(shapelib, AC_HELP_STRING([--disable-shapelib],
  [disable shapelib support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([shp-fortran], [shpopen], 
 AC_MSG_RESULT([shapelib code included])
  SHP_LIBS="-lshp-fortran -lshp"
  AC_SUBST(SHP_LIBS)
  AC_DEFINE(HAVE_LIBSHP_FORTRAN, 1, "shp-fortran library found"),
 AC_MSG_ERROR([fortran bindings to shp library not found]), [-lshp] )
else
AC_MSG_RESULT([shapelib code not included])
fi
AM_CONDITIONAL(SHP, test "x$enableval" = "xyes")


AC_ARG_ENABLE(grib, AC_HELP_STRING([--disable-grib],
  [disable grib support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([emos], [gribex],
 AC_MSG_RESULT([grib code included])
  GRIB_LIBS="-lemos"
  AC_SUBST(GRIB_LIBS),
 AC_MSG_ERROR([ECMWF emos library not found]))
else
AC_MSG_RESULT([grib code not included])
fi
AM_CONDITIONAL(GRIB, test "x$enableval" = "xyes")

AC_ARG_ENABLE(dballe, AC_HELP_STRING([--disable-dballe],
  [disable dballe support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_MSG_RESULT([dballe code included])
DBALLE_LIBS="-ldballef -ldballe -lcnf -lodbc -lm"
AC_SUBST(DBALLE_LIBS)
else
AC_MSG_RESULT([dballe code not included])
fi
AM_CONDITIONAL(DBALLE, test "x$enableval" = "xyes")


#we here test for f77; will be better develop a test for f90
AC_ARG_ENABLE(netcdf, AC_HELP_STRING([--disable-netcdf],
  [disable netcdf support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([netcdff], [nf_inq_libvers],
 AC_MSG_RESULT([netcdf code included])
 NETCDF_LIBS="-lnetcdff -lnetcdf"
 AC_SUBST(NETCDF_LIBS),
 AC_MSG_ERROR([netcdf library not found]),[-lnetcdf])
else
AC_MSG_RESULT([netcdf code not included])
fi
AM_CONDITIONAL(NETCDF, test "x$enableval" = "xyes")


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
