lib_LTLIBRARIES = libsim_termolib.la
libsim_termolib_la_SOURCES =  termolib.f90

AM_FCFLAGS = -I../base
libsim_termolib_la_LIBADD = ../base/libsim_base.la

if VERSIONINFO
libsim_termolib_la_LDFLAGS = -version-info @LIBSIM_VERSION_INFO@
endif

# c'e` una maniera migliore?
tmpvar = $(libsim_termolib_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
