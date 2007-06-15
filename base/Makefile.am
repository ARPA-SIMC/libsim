lib_LTLIBRARIES = libsim_base.la
libsim_base_la_SOURCES = char_utilities.f90 datetime_class.f90 err_handling.f90 \
 file_utilities.f90 geo_coord_class.F90 \
 io_units.f90 kinds.f90 missing_values.f90 phys_const.f90

# c'e` una maniera migliore?
tmpvar = $(libsim_base_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

datetime_class.$(OBJEXT): err_handling.$(OBJEXT) missing_values.$(OBJEXT)
datetime_class.lo: err_handling.$(OBJEXT) missing_values.$(OBJEXT)
err_handling.$(OBJEXT): io_units.$(OBJEXT)
err_handling.lo: io_units.$(OBJEXT)
file_utilities.$(OBJEXT): err_handling.$(OBJEXT)
file_utilities.lo: err_handling.$(OBJEXT)
geo_coord_class.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
geo_coord_class.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
missing_values.$(OBJEXT): kinds.$(OBJEXT)
missing_values.lo: kinds.$(OBJEXT)

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
