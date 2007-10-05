lib_LTLIBRARIES = libsim_termolib.la
libsim_termolib_la_SOURCES =  phisical_constant.f90 termolib.f90

AM_FCFLAGS = -I../base -I../vol7d
#libsim_termolib_la_LIBADD = ../base/libsim_base.la
#noinst_HEADERS =  phisical_constant.f90 termolib.f90

# c'e` una maniera migliore?
tmpvar = $(libsim_termolib_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)


#modqc.$(OBJEXT): modqc.$(OBJEXT) 
#modqc.lo: modqc.$(OBJEXT) 



mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
