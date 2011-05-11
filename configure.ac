dnl Process this file with autoconf to produce a configure script.

AC_INIT(libsim, 4.2.0, [dcesari@arpa.emr.it])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])

LIBSIM_VERSION_INFO=`echo $VERSION | awk -F. '{ printf "%d:%d:%d", $1+$2, $3, $2 }'`
AC_SUBST(LIBSIM_VERSION_INFO)

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
 [Fortran KIND defining an INTEGER containing a C pointer])

AC_ARG_ENABLE(f2003-features, AC_HELP_STRING([--enable-f2003-features],
  [enable some F2003 features]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
AC_DEFINE(F2003_FEATURES, 1, [Enable some F2003 features])
fi

dnl Locate Fortran 90 compiler among these if not specified
AC_PROG_FC([gfortran pgf90 ifort g95 xlf90 f90])
F77=$FC
AC_FC_WRAPPERS()dnl Determine how to call C from Fortran

AC_ARG_ENABLE(debug-mode, AC_HELP_STRING([--enable-debug-mode],
  [enable debug mode]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
if test "x$FC" = "xgfortran"; then
FCFLAGS="$FCFLAGS -g -fbounds-check -O0 -fexceptions"
else
FCFLAGS="$FCFLAGS -g -C"
fi
AC_DEFINE(DEBUG, 1, [Enable debugging mode])
fi

dnl options to be added compulsorily for particular compilers
case "$FC" in
  *xlf*)
# -qextname: external names with underscore
# -WF,-C!: remove C comments from preprocessed (Fortran) code
  FCFLAGS="$FCFLAGS -qextname -WF,-C!"
#  FCFLAGS="$FCFLAGS -qextname -F "'$(abs_top_builddir)/portability/xlf.cfg'
;;
esac


AC_ARG_ENABLE(log4c, AC_HELP_STRING([--disable-log4c],
  [disable use of log4c package]), , [enableval=yes])dnl default enable

if test "x$enableval" = "xyes"; then
dnl search for log4c, if not found build a dummy log4fortran
  AC_CHECK_HEADERS([log4c.h],
    log4c_h=yes,
    AC_MSG_RESULT([Package log4c missing])
    dummy_log4fortran=yes)
dnl for some reason, this test does not work when inserted
dnl in the previous AC_CHECK_HEADERS
  if test "x$log4c_h" = "xyes"; then
    AC_CHECK_LIB([log4c], [log4c_init],
      true, AC_MSG_ERROR([Package log4c broken!]))dnl true is an empty operation
    AC_CHECK_HEADERS([f77.h],
      AC_CHECK_LIB([cnf], [cnfCptr],
        LOG4C_LIBS="-llog4c -lcnf"
        AC_SUBST(LOG4C_LIBS)
        AC_DEFINE(HAVE_LIBLOG4C, 1, [Enable log4c])
        AC_DEFINE(HAVE_LIBCNF, 1, [Enable cnf]),
        AC_MSG_ERROR([Package cnf broken!])),
      AC_MSG_RESULT([Package cnf missing])
      dummy_log4fortran=yes)
  fi
else
dnl build a dummy log4fortran
  dummy_log4fortran=yes
fi

if test "x$dummy_log4fortran" = "xyes"; then
  AC_MSG_RESULT([Building dummy log4fortran])
fi
AM_CONDITIONAL(LOG4C, test "x$dummy_log4fortran" != "xyes")

AC_ARG_ENABLE(oraclesim, AC_HELP_STRING([--disable-oraclesim],
  [disable oraclesim/meteodata support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
save_cppflags=$CPPFLAGS
ora_cppflags=''
ora_ldflags=''
if test -n "$ORACLE_VER"; then
  ora_cppflags="-I/usr/include/$ORACLE_VER"
  for try in /usr/lib64/$ORACLE_VER/lib /usr/lib/$ORACLE_VER/lib; do
  if test -d "$try"; then
    ora_ldflags="-L$try"
    break
  fi
done
elif test -n "$ORACLE_HOME"; then
  dnl Non e' semplice dire dove siano gli include
  ora_cppflags="-I$ORACLE_HOME/sdk/include"
  if test -d "$ORACLE_HOME/lib" ;then
    ora_ldflags="-L$ORACLE_HOME/lib"
  fi
else
  dnl tenta il default se qualcuno si e' preso la briga di standardizzare le
  dnl cose. In tal caso si impostera' il -I anche se non e' corretto
  ora_cppflags="-I/usr/include/oracle"
  ora_ldflags=''
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
AC_DEFINE(HAVE_ORSIM, 1, [Enable oraclesim])
unset save_cppflags ora_cppflags ora_ldflags ora_lib
fi
AM_CONDITIONAL(ORSIM, test "x$enableval" = "xyes")


AC_LANG_PUSH(Fortran)dnl Do the following checks in Fortran
AC_FC_SRCEXT(f90)dnl and use .f90 extension


dnl check for Fortran 2003 ISO_C_BINDING intrinsic module
AC_COMPILE_IFELSE(
[AC_LANG_PROGRAM(,[[       USE, INTRINSIC :: iso_c_binding]])],
                          [AC_MSG_RESULT([Fortran 2003 C bindings found!])]
                          f2003_iso_c=yes,
                          [AC_MSG_ERROR([Fortran 2003 C bindings not found!])]
                          f2003_iso_c=no
)


AC_LANG_POP(Fortran)dnl Do the following checks in C


AC_ARG_ENABLE(gdal, AC_HELP_STRING([--enable-gdal],
  [enable gdal support]),,[enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
  if test "x$f2003_iso_c" = "xno"; then
  AC_MSG_ERROR([gdal supports requires a fortran compiler with iso_c_binding])
  fi
  AC_CHECK_LIB([gdal], [GDALOpen],
    AC_MSG_RESULT([gdal code included])
    AC_DEFINE(HAVE_LIBGDAL, 1, [Enable gdal code])
    GDAL_LIBS="-lgdal"
    AC_SUBST(GDAL_LIBS),
    AC_MSG_ERROR([gdal library not found]),)
else
  AC_MSG_RESULT([gdal code not included])
fi
AM_CONDITIONAL(GDAL, test "x$enableval" = "xyes")


AC_LANG_PUSH(Fortran)dnl Do the following checks in Fortran


AC_ARG_ENABLE(shapelib, AC_HELP_STRING([--disable-shapelib],
  [disable shapelib support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([shp-fortran], [shpopen],
 AC_MSG_RESULT([shapelib code included])
 SHP_LIBS="-lshp-fortran -lshp"
 AC_SUBST(SHP_LIBS)
 AC_DEFINE(HAVE_LIBSHP_FORTRAN, 1, [Enable shp-fortran]),
 AC_MSG_ERROR([fortran bindings to shp library not found]), [-lshp] )
else
AC_MSG_RESULT([shapelib code not included])
fi
AM_CONDITIONAL(SHP, test "x$enableval" = "xyes")

enablegrib=no
AC_ARG_ENABLE(emoslib, AC_HELP_STRING([--disable-emoslib],
  [disable grib support with ECMWF emos library]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([emos], [gribex],
 AC_MSG_RESULT([ECMWF emos code included])
  EMOS_LIBS="-lemos"
  AC_SUBST(EMOS_LIBS),
 AC_MSG_ERROR([ECMWF emos library not found]))
else
AC_MSG_RESULT([ECMWF emos code not included])
fi
AM_CONDITIONAL(EMOS, test "x$enableval" = "xyes")
if test "x$enableval" = "xyes"; then
enablegrib=yes
fi

AC_ARG_ENABLE(dwdgrib1, AC_HELP_STRING([--enable-dwdgrib1],
  [enable grib support with DWD grib1 library]),,[enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB([grib1], [grbin1],
 AC_MSG_RESULT([DWD grib1 code included])
  DWDGRIB1_LIBS="-lgrib1"
  AC_SUBST(DWDGRIB1_LIBS),
 AC_MSG_ERROR([DWD grib1 library not found]))
else
AC_MSG_RESULT([DWD grib1 code not included])
fi
AM_CONDITIONAL(DWDGRIB1, test "x$enableval" = "xyes")
if test "x$enableval" = "xyes"; then
enablegrib=yes
fi
AM_CONDITIONAL(GRIB, test "x$enablegrib" = "xyes")

AC_ARG_ENABLE(dballe, AC_HELP_STRING([--disable-dballe],
  [disable dballe support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_MSG_RESULT([dballe code included])
DBALLE_LIBS="-ldballef -ldballe -lcnf -lodbc -lm"
AC_SUBST(DBALLE_LIBS)
AC_DEFINE(HAVE_DBALLE, 1, [Enable dballe])
else
AC_MSG_RESULT([dballe code not included])
fi
AM_CONDITIONAL(DBALLE, test "x$enableval" = "xyes")


AC_ARG_ENABLE(gribapi, AC_HELP_STRING([--disable-gribapi],
  [disable gribapi support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
AC_CHECK_LIB(
	     [grib_api_f90],
	     [grib_f_open_file],
	     [
	      AC_MSG_RESULT([gribapi code included])
	      AC_DEFINE(HAVE_LIBGRIBAPI, 1, [Enable gribapi code])
	      GRIBAPI_LIBS="-lgrib_api_f90 -lgrib_api -ljasper"
	      AC_SUBST(GRIBAPI_LIBS)
	     ],
	     [AC_MSG_ERROR([grib-api library not found])],
	     [-lgrib_api -ljasper]
	    )
else
  AC_MSG_RESULT([gribapi code not included])
fi
AM_CONDITIONAL(GRIBAPI, test "x$enableval" = "xyes")


AC_ARG_ENABLE(ngmath, AC_HELP_STRING([--disable-ngmath],
  [disable ngmath ncarg interpolation support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
dnl search libdir
ngmath_ldflags=''
for try in /usr/lib64/ncarg /usr/lib/ncarg; do
  if test -d "$try"; then
    ngmath_ldflags="-L$try"
    break
  fi
done
  AC_CHECK_LIB([ngmath], [natgridd],
    NGMATH_LDFLAGS=$ngmath_ldflags
    NGMATH_LIBS="-lngmath"
    AC_SUBST(NGMATH_LDFLAGS)
    AC_SUBST(NGMATH_LIBS)
    AC_DEFINE(HAVE_LIBNGMATH, 1, [Enable ngmath code])
    AC_MSG_RESULT([ngmath code included]),
    AC_MSG_ERROR([ngmath ncarg library not found]),
    [$ngmath_ldflags])
else
  AC_MSG_RESULT([ngmath code not included])
fi
AM_CONDITIONAL(NGMATH, test "x$enableval" = "xyes")


AC_ARG_ENABLE(ncarg, AC_HELP_STRING([--disable-ncarg],
  [disable ncar graphics]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
dnl search libdir
ncarg_ldflags=''
for try in /usr/lib64/ncarg /usr/lib/ncarg; do
  if test -d "$try"; then
    ncarg_ldflags="-L$try"
    break
  fi
done
  AC_CHECK_LIB([ncarg], [ngpswk],
    NCARG_LDFLAGS=$ncarg_ldflags
    NCARG_LIBS="-lncarg -lncarg_gks -lncarg_c -lX11 -lpng"
    AC_SUBST(NCARG_LDFLAGS)
    AC_SUBST(NCARG_LIBS)
    AC_DEFINE(HAVE_LIBNCARG, 1, [Enable ncar graphics code])
    AC_MSG_RESULT([ncar graphics code included]),
    AC_MSG_ERROR([ncar graphics library not found]),
    [$ncarg_ldflags -lncarg_gks -lncarg_c -lX11])
else
  AC_MSG_RESULT([ncar graphics code not included])
fi
AM_CONDITIONAL(NCARG, test "x$enableval" = "xyes")


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


dnl define cpp macro for pasting cpp macros without blanks: with GNU
dnl fortran compilers the -traditional-cpp option is used so that the ##
dnl operator is not available
if test "x$FC" = "xgfortran" -o "x$FC" = "xg95"; then
AC_DEFINE_UNQUOTED([PASTE(a,b)], [a/**/b],
 [macro for pasting two macros without blanks, does not work :-(])
else
AC_DEFINE_UNQUOTED([POSTPASTE(a,b)], [a ## b],
 [2nd half of macro for pasting two macros without blanks])
AC_DEFINE_UNQUOTED([PASTE(a,b)], [POSTPASTE(a,b)],
 [macro for pasting two macros without blanks])
AC_MSG_RESULT([Please preprocess your source files by hand with
portability/conv_cpp.sh -f command before compiling the first time,
sorry for the inconvenience.])
fi


AC_LANG_POP()dnl back to C

AC_ARG_ENABLE(versioninfo, AC_HELP_STRING([--disable-versioninfo],
  [disable version information in shared libraries]),,[enableval=yes])dnl default disable
AM_CONDITIONAL(VERSIONINFO, test "x$enableval" = "xyes")

AC_ARG_ENABLE(doxydoc, AC_HELP_STRING([--disable-doxydoc],
  [disable generation of doxygen documentation]),,[enableval=yes])dnl default enable
AM_CONDITIONAL(DOXYDOC, test "x$enableval" = "xyes")


if test "x$prefix" = "xNONE"; then
AC_DEFINE_UNQUOTED(PREFIX, "$ac_default_prefix", [install prefix])
else
AC_DEFINE_UNQUOTED(PREFIX, "$prefix", [install prefix])
fi

AC_CHECK_PROG(help2man, help2man, yes, no,,)
AM_CONDITIONAL(HELP2MAN, test "x$help2man" = "xyes")

AC_PROG_LIBTOOL

pkgdocdir='${datadir}/doc/${PACKAGE}'
AC_SUBST(pkgdocdir)

AC_CONFIG_FILES([
Makefile
base/Makefile
grib/Makefile
vol7d/Makefile
volgrid6d/Makefile
examples/Makefile
data/Makefile
qc/Makefile
termolib/Makefile
doc/Doxyfile
doc/Makefile
log4fortran/Makefile
graphics/Makefile
])

AC_OUTPUT
