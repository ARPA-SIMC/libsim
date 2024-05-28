lib_LTLIBRARIES = libsim_termolib.la
libsim_termolib_la_SOURCES =  termolib.f90

AM_FCFLAGS = -I../base
libsim_termolib_la_LIBADD = ../base/libsim_base.la

libsim_termolib_la_SOURCES +=  termo.F03
# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS += -I../alchimia
libsim_termolib_la_LIBADD += ../alchimia/libsim_alchimia.la


if VERSIONINFO
libsim_termolib_la_LDFLAGS = -version-info @LIBSIM_VERSION_INFO@
endif


# c'e` una maniera migliore?
tmpvar = $(libsim_termolib_la_SOURCES:.f90=.mod)
tmpvar1 = $(tmpvar:.f03=.mod)
tmpvar2 = $(tmpvar1:.F03=.mod)
nodist_include_HEADERS = $(tmpvar2:.F90=.mod)

check_PROGRAMS = termolib_test
TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = $(top_srcdir)/testenv

termolib_test_SOURCES = termolib_test.f90
termolib_test_LDADD = libsim_termolib.la ../base/libsim_base.la


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
