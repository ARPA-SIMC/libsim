dnl Process this file with autoconf to produce a configure script.

AC_INIT(libsim, 7.1.5, [dcesari@arpae.it])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_MACRO_DIR([m4])

LIBSIM_VERSION_INFO="12:0:0"
AC_SUBST(LIBSIM_VERSION_INFO)

AC_LANG([C])
AC_C_BIGENDIAN
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(size_t)

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

AC_ARG_ENABLE(examples, AC_HELP_STRING([--enable-examples],
  [enable build of examples]), , [enableval=no])dnl default disable
AM_CONDITIONAL(EXAMPLES, test "x$enableval" = "xyes")

AC_ARG_ENABLE(f2003-full-features, AC_HELP_STRING([--enable-f2003-full-features],
  [enable full F2003 features (gfortran 4.9)]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
  AC_DEFINE(F2003_FULL_FEATURES, 1, [Enable full F2003 features (gfortran 4.9)])
fi
AM_CONDITIONAL(F2003_FULL_FEATURES, test "x$enableval" = "xyes")
#save
MY_F2003_FULL_FEATURES=$enableval

AC_PROG_CXX()

dnl Locate Fortran 90 compiler among these if not specified
AC_PROG_FC([gfortran pgf90 ifort g95 xlf90 f90])
F77=$FC
AC_FC_WRAPPERS()dnl Determine how to call C from Fortran

AC_ARG_ENABLE(debug-mode, AC_HELP_STRING([--enable-debug-mode],
  [enable debug mode]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
case "$FC" in
  *gfortran*)
FCFLAGS="$FCFLAGS -g -fbounds-check -O0 -fexceptions -Wall"
;;
  *)
FCFLAGS="$FCFLAGS -g -C"
esac
AC_DEFINE(DEBUG, 1, [Enable debugging mode])
fi

ELEMENTAL_UNLESSXLF=ELEMENTAL
dnl options to be added compulsorily for particular compilers
dnl -fbackslash required in char_utilities.f90 for '\\'
case "$FC" in
  *xlf*)
# -qextname: external names with underscore
# -WF,-C!: remove C comments from preprocessed (Fortran) code
  FCFLAGS="$FCFLAGS -qextname -WF,-C!"
#  FCFLAGS="$FCFLAGS -qextname -F "'$(abs_top_builddir)/portability/xlf.cfg'
  ELEMENTAL_UNLESSXLF="PURE"
;;
  *gfortran*)
  FCFLAGS="$FCFLAGS -fbackslash"
;;
esac

AC_DEFINE_UNQUOTED(ELEMENTAL_UNLESSXLF, $ELEMENTAL_UNLESSXLF, [Disable some elemental functions on xlf])

dnl Locate fortran module directory on Debian systems
AC_PATH_PROG([DPKG_ARCHITECTURE], [dpkg-architecture])
if test x$DPKG_ARCHITECTURE != x
then
  FNAME=$(basename $(readlink -f $(which $FC)))
  FMODDIR=/usr/lib/`$DPKG_ARCHITECTURE -qDEB_HOST_MULTIARCH`/fortran/$FNAME
  FCFLAGS="$FCFLAGS -I$FMODDIR -I/usr/include"
fi

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
      LOG4C_LIBS="-llog4c"
      AC_SUBST(LOG4C_LIBS)
      AC_DEFINE(HAVE_LIBLOG4C, 1, [Enable log4c]),
      AC_MSG_ERROR([Package log4c broken!]))dnl true is an empty operation
  fi
else
dnl build a dummy log4fortran
  dummy_log4fortran=yes
fi

if test "x$dummy_log4fortran" = "xyes"; then
  AC_MSG_RESULT([Building dummy log4fortran])
fi
AM_CONDITIONAL(LOG4C, test "x$dummy_log4fortran" != "xyes")


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

AC_ARG_ENABLE(vapor, AC_HELP_STRING([--enable-vapor],
  [enable vapor support]),,[enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
  if test "x$f2003_iso_c" = "xno"; then
    AC_MSG_ERROR([VAPOR support requires a fortran compiler with iso_c_binding])
  fi
  AC_MSG_RESULT([vapor code included])
  AC_DEFINE(VAPOR, 1, [Enable vapor code])
  VAPOR_CPPFLAGS="-I/usr/include/vapor"
  AC_SUBST(VAPOR_CPPFLAGS)
  VAPOR_LIBS="-lvdf -lcommon -lnetcdf -lexpat -lproj -lrt -ludunits2 -lstdc++"
  AC_SUBST(VAPOR_LIBS)
else
  AC_MSG_RESULT([vapor code not included])
fi
AM_CONDITIONAL(VAPOR, test "x$enableval" = "xyes")

AC_ARG_ENABLE(alchimia, AC_HELP_STRING([--enable-alchimia],
  [enable derived variables]),,[enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
  if test "x$f2003_iso_c" = "xno"; then
    AC_MSG_ERROR([ALCHIMIA support requires a fortran compiler with 2003 features])
  fi
  AC_MSG_RESULT([alchimia code included])
  AC_DEFINE(ALCHIMIA, 1, [Enable alchimia code])
else
  AC_MSG_RESULT([alchimia code not included])
fi
AM_CONDITIONAL(ALCHIMIA, test "x$enableval" = "xyes")


AC_LANG_PUSH(Fortran)dnl Do the following checks in Fortran

AC_ARG_ENABLE(shapelib, AC_HELP_STRING([--disable-shapelib],
  [disable shapelib support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
  AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM(,[[       USE shapelib]])],
   [AC_MSG_RESULT([shapelib C bindings found!])]
   SHAPELIB_LIBS="-lfortrangis -lshp"
   AC_SUBST(SHAPELIB_LIBS)
   AC_DEFINE(HAVE_SHAPELIB, 1, [Enable shapelib]),
   [AC_MSG_ERROR([shapelib C bindings not found!])]
  )
else
  AC_MSG_RESULT([shapelib code not included])
fi
AM_CONDITIONAL(SHAPELIB, test "x$enableval" = "xyes")

AC_ARG_ENABLE(gdal, AC_HELP_STRING([--enable-gdal],
  [enable gdal support]),,[enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
  AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM(,[[       USE gdal]])],
   [AC_MSG_RESULT([gdal C bindings found!])]
   GDAL_LIBS="-lfortrangis -lgdal"
   AC_SUBST(GDAL_LIBS)
   AC_DEFINE(HAVE_LIBGDAL, 1, [Enable gdal]),
   [AC_MSG_ERROR([gdal C bindings not found!])]
  )
else
  AC_MSG_RESULT([gdal code not included])
fi
AM_CONDITIONAL(GDAL, test "x$enableval" = "xyes")


AC_ARG_ENABLE(dballe, AC_HELP_STRING([--disable-dballe],
  [disable dballe support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then

  if test "x$MY_F2003_EXTENDED_FEATURES" = "xno"; then
     AC_MSG_ERROR([dballe enabled without F2003_EXTENDED_FEATURES])
  fi
  PKG_CHECK_EXISTS([libdballef >= 6.0], [enableval=yes], [enableval=no])
  if test "x$enableval" = "xyes"; then
    AC_MSG_RESULT([dballe code requested and found])
    PKG_CHECK_MODULES(DBALLE, [libdballef >= 6.0])
    AC_DEFINE(HAVE_DBALLE, 1, [Enable dballe])
    dnl DBALLE_LIBS="-ldballef -ldballe -lcnf -lodbc -lm"
    dnl AC_SUBST(DBALLE_LIBS)
  else
    AC_MSG_WARN([dballe code requested but not found])
  fi

else
  AC_MSG_RESULT([dballe code not included])
fi
AM_CONDITIONAL(DBALLE, test "x$enableval" = "xyes")


AC_ARG_ENABLE(gribapi, AC_HELP_STRING([--disable-gribapi],
  [disable gribapi support]),,[enableval=yes])dnl default enable
if test "x$enableval" = "xyes"; then
grib_api_flavor=
PKG_CHECK_EXISTS([eccodes_f90], [enableval=yes], [enableval=no])
if test "x$enableval" = "xyes"; then
  AC_MSG_RESULT([eccodes code included])
  PKG_CHECK_MODULES(GRIBAPI, [eccodes_f90])
  AC_DEFINE(HAVE_LIBGRIBAPI, 1, [Enable gribapi code])
  AC_SUBST(GRIBAPI_LIBS)
  GRIBAPI_PKGCONFIG="eccodes_f90"
  AC_SUBST(GRIBAPI_PKGCONFIG)
  grib_api_flavor=eccodes
else
  AC_CHECK_LIB(
       [eccodes_f90],
       [grib_f_open_file],
       [
        AC_MSG_RESULT([eccodes code included])
        AC_DEFINE(HAVE_LIBGRIBAPI, 1, [Enable gribapi code])
        GRIBAPI_LIBS="-leccodes_f90 -leccodes -ljasper"
        AC_SUBST(GRIBAPI_LIBS)
        GRIBAPI_PKGCONFIG="eccodes_f90"
        AC_SUBST(GRIBAPI_PKGCONFIG)
        grib_api_flavor=eccodes
       ],
       [AC_MSG_RESULT([eccodes library not found, trying with legacy grib_api])],
       [-leccodes -ljasper]
      )
fi

if test "x$grib_api_flavor" = "x"; then
AC_CHECK_LIB(
	     [grib_api_f90],
	     [grib_f_open_file],
	     [
	      AC_MSG_RESULT([gribapi code included])
	      AC_DEFINE(HAVE_LIBGRIBAPI, 1, [Enable gribapi code])
	      GRIBAPI_LIBS="-lgrib_api_f90 -lgrib_api"
	      AC_SUBST(GRIBAPI_LIBS)
	      GRIBAPI_PKGCONFIG="grib_api_f90"
	      AC_SUBST(GRIBAPI_PKGCONFIG)
	      grib_api_flavor=grib_api
	     ],
	     [AC_MSG_ERROR([eccodes and grib-api library not found])],
	     [-lgrib_api]
	    )
fi
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
    NCARG_LIBS="-lncarg -lncarg_gks -lncarg_c -lX11 -lcairo -lfreetype"
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
    NETCDF_LIBS="-lnetcdff -lnetcdf"
    AC_SUBST(NETCDF_LIBS)
    AC_DEFINE(HAVE_LIBNETCDF, 1, [Enable netcdf code])
    AC_MSG_RESULT([netcdf code included]),
    AC_MSG_ERROR([netcdf library not found]),
    [-lnetcdff -lnetcdf])
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
if test "x$enableval" = "xyes"; then
   AC_CHECK_PROG(HAVE_DOT, dot, YES, NO,,)
   AC_SUBST(HAVE_DOT)
   AC_CHECK_PROG(ladv, latex, yes, no,,)
   if test "x$ladv" = "xno"; then
     AC_MSG_ERROR([latex required for documentation!])
   fi
   AC_CHECK_PROG(ladv, dvips, yes, no,,)
   if test "x$ladv" = "xno"; then
     AC_MSG_ERROR([dvips required for documentation!])
   fi
fi

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
libsim.pc
base/Makefile
vol7d/Makefile
volgrid6d/Makefile
bin/Makefile
examples/Makefile
data/Makefile
qc/Makefile
termolib/Makefile
doc/Doxyfile
doc/Makefile
log4fortran/Makefile
graphics/Makefile
alchimia/Makefile
])

AC_OUTPUT
