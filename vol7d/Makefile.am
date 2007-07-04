lib_LTLIBRARIES = libsim_vol7d.la
libsim_vol7d_la_SOURCES = vol7d_ana_class.F90 vol7d_class.F90 \
 vol7d_level_class.F90 vol7d_network_class.F90 \
 vol7d_timerange_class.F90 vol7d_utilities.F90 \
 vol7d_var_class.f90 vol7d_varvect_class.f90
if ORSIM
libsim_vol7d_la_SOURCES += vol7d_oraclesim_class.f90
endif
if DBALLE
libsim_vol7d_la_SOURCES += vol7d_dballe_class.f90
endif

#libsim_vol7d_la_FFLAGS = -I../base
AM_FCFLAGS = -I../base
libsim_vol7d_la_LIBADD = ../base/libsim_base.la
noinst_HEADERS = vol7d_class_get_vol.F90 vol7d_class_remap.F90 \
 vol7d_class_merge_final.F90 vol7d_distinct.F90
# c'e` una maniera migliore?
tmpvar = $(libsim_vol7d_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)


#vol7d_ana_class.$(OBJEXT):
vol7d_class.$(OBJEXT): vol7d_utilities.$(OBJEXT) vol7d_ana_class.$(OBJEXT) \
 vol7d_timerange_class.$(OBJEXT) vol7d_level_class.$(OBJEXT) \
 vol7d_network_class.$(OBJEXT) vol7d_varvect_class.$(OBJEXT) \
 vol7d_class_get_vol.F90 vol7d_class_remap.F90 vol7d_class_merge_final.F90
vol7d_class.lo: vol7d_utilities.$(OBJEXT) vol7d_ana_class.$(OBJEXT) \
 vol7d_timerange_class.$(OBJEXT) vol7d_level_class.$(OBJEXT) \
 vol7d_network_class.$(OBJEXT) vol7d_varvect_class.$(OBJEXT) \
 vol7d_class_get_vol.F90 vol7d_class_remap.F90 vol7d_class_merge_final.F90
#vol7d_level_class.$(OBJEXT):
#vol7d_network_class.$(OBJEXT):
if ORSIM
vol7d_oraclesim_class.$(OBJEXT): vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
vol7d_oraclesim_class.lo: vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
endif

if DBALLE
vol7d_dballe_class.$(OBJEXT): vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
vol7d_dballe_class.lo: vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
endif

#vol7d_timerange_class.$(OBJEXT):
#vol7d_utilities.$(OBJEXT):
#vol7d_utilities_orig.$(OBJEXT):
#vol7d_var_class.$(OBJEXT):
vol7d_varvect_class.$(OBJEXT): vol7d_var_class.$(OBJEXT)
vol7d_varvect_class.lo: vol7d_var_class.$(OBJEXT)


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
