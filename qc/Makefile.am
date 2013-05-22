lib_LTLIBRARIES = libsim_qc.la
libsim_qc_la_SOURCES = modqc.F90 modqctem.F90

AM_FCFLAGS = -I../base -I../vol7d -I../log4fortran
libsim_qc_la_LIBADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

if VERSIONINFO
libsim_qc_la_LDFLAGS = -version-info @LIBSIM_VERSION_INFO@
endif

if SHAPELIB
libsim_qc_la_LIBADD += $(SHAPELIB_LIBS)
libsim_qc_la_SOURCES += modqccli.F90
endif

if NCARG
#libsim_qc_la_LIBADD += $(NCARG_LIBS)
libsim_qc_la_SOURCES += modqcspa.F90
#libsim_qc_la_LDFLAGS = $(NCARG_LDFLAGS)
endif

noinst_HEADERS = modqc_peeled_include.F90  modqc_peeling_include.F90  modqc_peel_util_include.F90

# c'e` una maniera migliore?
tmpvar = $(libsim_qc_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
