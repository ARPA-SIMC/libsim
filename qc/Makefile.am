lib_LTLIBRARIES = libsim_qc.la
libsim_qc_la_SOURCES = modqc.f90 modqccli.f90

AM_FCFLAGS = -I../base -I../vol7d
libsim_qc_la_LIBADD = ../base/libsim_base.la
#noinst_HEADERS = modqc.f90 modqccli.f90

# c'e` una maniera migliore?
tmpvar = $(libsim_qc_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)


#modqc.$(OBJEXT): modqc.$(OBJEXT) 
#modqc.lo: modqc.$(OBJEXT) 



mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
