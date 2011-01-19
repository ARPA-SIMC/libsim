lib_LTLIBRARIES = libsim_qc.la
libsim_qc_la_SOURCES = modqc.f90 modqccli.F90

AM_FCFLAGS = -I../base -I../vol7d -I../log4fortran
libsim_qc_la_LIBADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

if VERSIONINFO
libsim_qc_la_LDFLAGS = -version-info @LIBSIM_VERSION_INFO@
endif

# c'e` una maniera migliore?
tmpvar = $(libsim_qc_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
