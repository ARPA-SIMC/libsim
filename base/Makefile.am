lib_LTLIBRARIES = libsim_base.la
libsim_base_la_SOURCES = for2r.f90 io_units.F90 kinds.F90 phys_const.f90 missing_values.f90 \
 char_utilities.F90 err_handling.f90 optional_values.f90 datetime_class.F90 array_utilities.F90 file_utilities.F90 \
 geo_proj_class.F90 geo_coord_class.F90 georef_coord_class.F90 \
 optionparser_class.F90 \
 simple_stat.f90 \
 space_utilities.F90

 libsim_base_la_SOURCES += list_link.F03 \
 list_linkchar.F03 \
 list_abstract.F03 \
 list_abstractforchar.F03 \
 list_mix.F03 \
 list_integer.F03 \
 list_doubleprecision.F03 \
 list_character.F03 \
 list_byte.F03 \
 list_real.F03


# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS = -I../log4fortran

libsim_base_la_LIBADD = ../log4fortran/liblog4fortran.la
if VERSIONINFO
libsim_base_la_LDFLAGS = -version-info @LIBSIM_VERSION_INFO@
endif

if SHAPELIB
libsim_base_la_LIBADD += $(SHAPELIB_LIBS)
endif

noinst_HEADERS = arrayof_pre.F90 arrayof_post.F90 array_utilities_pre.F90 array_utilities_inc.F90 arrayof_pre_nodoc.F90 arrayof_post_nodoc.F90 

# c'e` una maniera migliore?
tmpvar = $(libsim_base_la_SOURCES:.f90=.mod)
tmpvar1 = $(tmpvar:.f03=.mod)
tmpvar2 = $(tmpvar1:.F03=.mod)
nodist_include_HEADERS = $(tmpvar2:.F90=.mod)

missing_values.$(OBJEXT): kinds.$(OBJEXT)
missing_values.lo: kinds.$(OBJEXT)
char_utilities.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT) io_units.$(OBJEXT)
char_utilities.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT) io_units.$(OBJEXT)

list_integer.$(OBJEXT): list_link.$(OBJEXT) list_abstract.$(OBJEXT)
list_integer.lo: list_link.$(OBJEXT) list_abstract.$(OBJEXT)
list_real.$(OBJEXT): list_link.$(OBJEXT) list_abstract.$(OBJEXT)
list_real.lo: list_link.$(OBJEXT) list_abstract.$(OBJEXT)
list_doubleprecision.$(OBJEXT): list_link.$(OBJEXT) list_abstract.$(OBJEXT)
list_doubleprecision.lo: list_link.$(OBJEXT) list_abstract.$(OBJEXT)
list_character.$(OBJEXT): list_linkchar.$(OBJEXT) list_abstractforchar.$(OBJEXT)
list_character.lo: list_linkchar.$(OBJEXT) list_abstractforchar.$(OBJEXT)
list_byte.$(OBJEXT): list_link.$(OBJEXT) list_abstract.$(OBJEXT) kinds.$(OBJEXT)
list_byte.lo: list_link.$(OBJEXT) list_abstract.$(OBJEXT) kinds.$(OBJEXT)
list_mix.$(OBJEXT): list_link.$(OBJEXT) list_abstract.$(OBJEXT) kinds.$(OBJEXT)
list_mix.lo: list_link.$(OBJEXT) list_abstract.$(OBJEXT) kinds.$(OBJEXT)

optional_values.$(OBJEXT): missing_values.$(OBJEXT)
optional_values.lo: missing_values.$(OBJEXT)

err_handling.$(OBJEXT): io_units.$(OBJEXT)
err_handling.lo: io_units.$(OBJEXT)
datetime_class.$(OBJEXT): err_handling.$(OBJEXT) missing_values.$(OBJEXT) optional_values.$(OBJEXT) char_utilities.$(OBJEXT) arrayof_pre.F90 arrayof_post.F90  array_utilities_inc.F90
datetime_class.lo: err_handling.$(OBJEXT) missing_values.$(OBJEXT) optional_values.$(OBJEXT) char_utilities.$(OBJEXT) arrayof_pre.F90 arrayof_post.F90  array_utilities_inc.F90
file_utilities.$(OBJEXT): kinds.$(OBJEXT) char_utilities.$(OBJEXT) missing_values.$(OBJEXT) err_handling.$(OBJEXT) optional_values.$(OBJEXT)
file_utilities.lo: kinds.$(OBJEXT) char_utilities.$(OBJEXT) missing_values.$(OBJEXT) err_handling.$(OBJEXT) optional_values.$(OBJEXT)
array_utilities.$(OBJEXT): datetime_class.$(OBJEXT)
array_utilities.lo: datetime_class.$(OBJEXT)
geo_proj_class.$(OBJEXT): phys_const.$(OBJEXT) missing_values.$(OBJEXT) optional_values.$(OBJEXT)
geo_proj_class.lo: phys_const.$(OBJEXT) missing_values.$(OBJEXT) optional_values.$(OBJEXT)
geo_coord_class.$(OBJEXT): kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
geo_coord_class.lo: kinds.$(OBJEXT) missing_values.$(OBJEXT) phys_const.$(OBJEXT)
georef_coord_class.$(OBJEXT): err_handling.$(OBJEXT) missing_values.$(OBJEXT) optional_values.$(OBJEXT) geo_proj_class.$(OBJEXT)
georef_coord_class.lo: err_handling.$(OBJEXT) missing_values.$(OBJEXT) optional_values.$(OBJEXT) geo_proj_class.$(OBJEXT)
optionparser_class.$(OBJEXT): err_handling.$(OBJEXT) kinds.$(OBJEXT) char_utilities.$(OBJEXT)
optionparser_class.lo: err_handling.$(OBJEXT) kinds.$(OBJEXT) char_utilities.$(OBJEXT)
simple_stat.$(OBJEXT): missing_values.$(OBJEXT) optional_values.$(OBJEXT) array_utilities.$(OBJEXT)
simple_stat.lo: missing_values.$(OBJEXT) optional_values.$(OBJEXT) array_utilities.$(OBJEXT)
geo_transforms.$(OBJEXT): phys_const.$(OBJEXT)
geo_transforms.lo: phys_const.$(OBJEXT)

check_PROGRAMS = char_utilities_test datetime_test file_utilities_test simple_stat_test optionparser_test list_test
dist_check_SCRIPTS = optionparser_test.sh
TESTS = $(check_PROGRAMS) $(dist_check_SCRIPTS)
TESTS_ENVIRONMENT = $(top_srcdir)/testenv

char_utilities_test_SOURCES = char_utilities_test.f90
char_utilities_test_LDADD = libsim_base.la 

datetime_test_SOURCES = datetime_test.f90
datetime_test_LDADD = libsim_base.la 

file_utilities_test_SOURCES = file_utilities_test.f90
file_utilities_test_LDADD = libsim_base.la 

simple_stat_test_SOURCES = simple_stat_test.f90
simple_stat_test_LDADD = libsim_base.la

optionparser_test_SOURCES = optionparser_test.f90
optionparser_test_LDADD = libsim_base.la

list_test_SOURCES = list_test.f90
list_test_LDADD = libsim_base.la

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
