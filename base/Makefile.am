lib_LTLIBRARIES = libsim_base.la
libsim_base_la_SOURCES = char_utilities.f90 datetime_class.f90 err_handling.f90 \
 optional_values.f90 file_utilities.f90 geo_coord_class.F90 geo_grid_class.f90 \
 io_units.f90 kinds.F90 missing_values.f90 phys_const.f90 getopt_m.f90 \
 simple_stat.f90

if SHP
libsim_base_la_LIBADD = $(SHP_LIBS)
endif

# c'e` una maniera migliore?
tmpvar = $(libsim_base_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

char_utilities.$(OBJEXT): kinds.$(OBJEXT)
char_utilities.lo: kinds.$(OBJEXT)
datetime_class.$(OBJEXT): err_handling.$(OBJEXT) missing_values.$(OBJEXT)
datetime_class.lo: err_handling.$(OBJEXT) missing_values.$(OBJEXT)
err_handling.$(OBJEXT): io_units.$(OBJEXT)
err_handling.lo: io_units.$(OBJEXT)
file_utilities.$(OBJEXT): kinds.$(OBJEXT) char_utilities.$(OBJEXT) missing_values.$(OBJEXT) err_handling.$(OBJEXT)
file_utilities.lo: kinds.$(OBJEXT) char_utilities.$(OBJEXT) missing_values.$(OBJEXT) err_handling.$(OBJEXT)
geo_coord_class.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
geo_coord_class.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
geo_grid_class.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT) char_utilities.$(OBJEXT) err_handling.$(OBJEXT)
geo_grid_class.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT) char_utilities.$(OBJEXT) err_handling.$(OBJEXT)
missing_values.$(OBJEXT): kinds.$(OBJEXT)
missing_values.lo: kinds.$(OBJEXT)
optional_values.$(OBJEXT): missing_values.$(OBJEXT)
optional_values.lo: missing_values.$(OBJEXT)
getopt_m.$(OBJEXT): err_handling.$(OBJEXT)
getopt_m.lo: err_handling.$(OBJEXT)
simple_stat.$(OBJEXT): missing_values.$(OBJEXT)
simple_stat.lo: missing_values.$(OBJEXT)

check_PROGRAMS = char_utilities_test datetime_test simple_stat_test
TESTS = $(check_PROGRAMS)

char_utilities_test_SOURCES = char_utilities_test.f90
char_utilities_test_LDADD = libsim_base.la 

datetime_test_SOURCES = datetime_test.f90
datetime_test_LDADD = libsim_base.la 

simple_stat_test_SOURCES = simple_stat_test.f90
simple_stat_test_LDADD = libsim_base.la

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
