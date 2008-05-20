lib_LTLIBRARIES = liblog4fortran.la
liblog4fortran_la_SOURCES = log4fortran.F90

AM_FCFLAGS = -I../base

if LOG4FORTRAN
liblog4fortran_la_SOURCES +=  log4fortran_c.c
endif


nodist_include_HEADERS = log4fortran.mod



mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
