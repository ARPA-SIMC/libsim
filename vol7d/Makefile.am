lib_LTLIBRARIES = libsim_vol7d.la
libsim_vol7d_la_SOURCES = vol7d_ana_class.f90 vol7d_class.F90 \
 vol7d_level_class.f90 vol7d_network_class.f90 \
 vol7d_timerange_class.f90 vol7d_utilities.f90 \
 vol7d_var_class.f90 vol7d_varvect_class.f90 \
 vol7d_oraclesim_class.f90
#libsim_vol7d_la_FFLAGS = -I../base
AM_FCFLAGS = -I../base
libsim_vol7d_la_LIBADD = ../base/libsim_base.la
noinst_HEADERS = vol7d_class_get_vol.F90 vol7d_class_remap.F90 \
 vol7d_class_merge_final.F90
include_HEADERS = $(libsim_vol7d_la_SOURCES:.f90=.mod)

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
vol7d_oraclesim_class.$(OBJEXT): vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
vol7d_oraclesim_class.lo: vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
#vol7d_timerange_class.$(OBJEXT):
#vol7d_utilities.$(OBJEXT):
#vol7d_utilities_orig.$(OBJEXT):
#vol7d_var_class.$(OBJEXT):
vol7d_varvect_class.$(OBJEXT): vol7d_var_class.$(OBJEXT)
vol7d_varvect_class.lo: vol7d_var_class.$(OBJEXT)


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
