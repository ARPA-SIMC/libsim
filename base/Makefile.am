lib_LTLIBRARIES = libsim_base.la
libsim_base_la_SOURCES = io_units.F90 kinds.F90 phys_const.f90 missing_values.f90 \
 char_utilities.f90 err_handling.f90 datetime_class.F90 optional_values.f90 file_utilities.F90 utilities.F90 \
 geo_coord_class.F90 geo_coord_with_utm_class.F90 geo_grid_class.f90 \
 optionparser_class.F90 \
 simple_stat.f90 geo_proj_class.F90

# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS = -I../log4fortran

libsim_base_la_LIBADD = ../log4fortran/liblog4fortran.la
if VERSIONINFO
libsim_base_la_LDFLAGS = -version-info @LIBSIM_VERSION_INFO@
endif

if SHP
libsim_base_la_LIBADD += $(SHP_LIBS)
endif

noinst_HEADERS = arrayof_pre.F90 arrayof_post.F90

# c'e` una maniera migliore?
tmpvar = $(libsim_base_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

missing_values.$(OBJEXT): kinds.$(OBJEXT)
missing_values.lo: kinds.$(OBJEXT)
char_utilities.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT)
char_utilities.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT)
optional_values.lo: optional_values.$(OBJEXT)
err_handling.$(OBJEXT): io_units.$(OBJEXT)
err_handling.lo: io_units.$(OBJEXT)
datetime_class.$(OBJEXT): err_handling.$(OBJEXT) missing_values.$(OBJEXT) arrayof_pre.F90 arrayof_post.F90
datetime_class.lo: err_handling.$(OBJEXT) missing_values.$(OBJEXT) arrayof_pre.F90 arrayof_post.F90
file_utilities.$(OBJEXT): kinds.$(OBJEXT) char_utilities.$(OBJEXT) missing_values.$(OBJEXT) err_handling.$(OBJEXT) optional_values.$(OBJEXT)
file_utilities.lo: kinds.$(OBJEXT) char_utilities.$(OBJEXT) missing_values.$(OBJEXT) err_handling.$(OBJEXT) optional_values.$(OBJEXT)
geo_coord_class.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
geo_coord_class.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
geo_coord_with_utm_class.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
geo_coord_with_utm_class.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
geo_grid_class.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT) char_utilities.$(OBJEXT) err_handling.$(OBJEXT) geo_coord_with_utm_class.$(OBJEXT)
geo_grid_class.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT) char_utilities.$(OBJEXT) err_handling.$(OBJEXT) geo_coord_with_utm_class.$(OBJEXT)
optionparser_class.$(OBJEXT): err_handling.$(OBJEXT) kinds.$(OBJEXT) char_utilities.$(OBJEXT)
optionparser_class.lo: err_handling.$(OBJEXT) kinds.$(OBJEXT) char_utilities.$(OBJEXT)
simple_stat.$(OBJEXT): missing_values.$(OBJEXT)
simple_stat.lo: missing_values.$(OBJEXT)
geo_transforms.$(OBJEXT): phys_const.$(OBJEXT)
geo_transforms.lo: phys_const.$(OBJEXT)
geo_proj_class.$(OBJEXT): phys_const.$(OBJEXT) missing_values.$(OBJEXT) optional_values.$(OBJEXT)
geo_proj_class.lo: phys_const.$(OBJEXT) missing_values.$(OBJEXT) optional_values.$(OBJEXT)

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
