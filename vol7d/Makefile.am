lib_LTLIBRARIES = libsim_vol7d.la
libsim_vol7d_la_SOURCES = vol7d_ana_class.F90 vol7d_level_class.F90 \
 vol7d_network_class.F90 vol7d_timerange_class.F90 \
 vol7d_var_class.F90 vol7d_varvect_class.f90 \
 vol7d_class.F90 stat_proc_engine.F90 vol7d_class_compute.F90

# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS = -I../log4fortran -I../base
# Opzioni C
AM_CFLAGS = -D_REENTRANT
# Opzioni preprocessore
AM_CPPFLAGS =
# Opzioni linker
AM_LDFLAGS =
libsim_vol7d_la_LDFLAGS =
# Librerie aggiuntive
libsim_vol7d_la_LIBADD = ../log4fortran/liblog4fortran.la ../base/libsim_base.la

if F2003_FEATURES
libsim_vol7d_la_SOURCES += vol7d_serialize_class.F03 vol7d_serialize_csv_class.F03 \
 vol7d_serialize_csvdba_class.F03 vol7d_serialize_geojson_class.F03 
endif

if DBALLE
AM_FCFLAGS += -I/usr/include/dballe
libsim_vol7d_la_LIBADD += $(DBALLE_LIBS)
if F2003_EXTENDED_FEATURES
libsim_vol7d_la_SOURCES += dballe_class.F03
libsim_vol7d_la_SOURCES += vol7d_serialize_dballe_class.F03
libsim_vol7d_la_SOURCES += vol7d_dballe_class.F03
#libsim_vol7d_la_SOURCES += vol7d_dballeold_class.F90
endif
endif


if NETCDF
libsim_vol7d_la_SOURCES += vol7d_netcdf_class.f90
libsim_vol7d_la_LIBADD += $(NETCDF_LIBS)
endif

if VERSIONINFO
libsim_vol7d_la_LDFLAGS += -version-info @LIBSIM_VERSION_INFO@
endif

noinst_HEADERS = vol7d_class_type_templ.F90 vol7d_class_desc_templ.F90 \
vol7d_class_diff.F90 vol7d_class_diff_only.F90 \
vol7d_dballe_class_nana.F90 vol7d_dballe_class_ndati.F90 \
vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90 vol7d_dballe_class_var_du.F90


# c'e` una maniera migliore?
tmpvar = $(libsim_vol7d_la_SOURCES:.f90=.mod)
tmpvar1 = $(tmpvar:.f03=.mod)
tmpvar2 = $(tmpvar1:.F03=.mod)
tmpvar3 = $(tmpvar2:vdf4f_c.cpp=)
nodist_include_HEADERS = $(tmpvar3:.F90=.mod)

dist_check_SCRIPTS = 
check_PROGRAMS = 
EXTRA_DIST =
TESTS = $(check_PROGRAMS) $(dist_check_SCRIPTS)

vol7d_varvect_class.$(OBJEXT): vol7d_var_class.$(OBJEXT)
vol7d_varvect_class.lo: vol7d_var_class.$(OBJEXT)
vol7d_class.$(OBJEXT): vol7d_ana_class.$(OBJEXT) \
 vol7d_timerange_class.$(OBJEXT) vol7d_level_class.$(OBJEXT) \
 vol7d_network_class.$(OBJEXT) vol7d_varvect_class.$(OBJEXT) \
 vol7d_class_type_templ.F90 vol7d_class_desc_templ.F90 vol7d_class_diff.F90 vol7d_class_diff_only.F90
vol7d_class.lo: vol7d_ana_class.$(OBJEXT) \
 vol7d_timerange_class.$(OBJEXT) vol7d_level_class.$(OBJEXT) \
 vol7d_network_class.$(OBJEXT) vol7d_varvect_class.$(OBJEXT) \
 vol7d_class_type_templ.F90 vol7d_class_desc_templ.F90 vol7d_class_diff.F90 vol7d_class_diff_only.F90
stat_proc_engine.$(OBJEXT): vol7d_class.$(OBJEXT)
stat_proc_engine.lo: vol7d_class.$(OBJEXT)
vol7d_class_compute.$(OBJEXT): vol7d_class.$(OBJEXT) stat_proc_engine.$(OBJEXT)
vol7d_class_compute.lo: vol7d_class.$(OBJEXT) stat_proc_engine.$(OBJEXT)

if DBALLE
#vol7d_dballeold_class.$(OBJEXT): vol7d_class.$(OBJEXT) \
#vol7d_dballe_class_nana.F90 vol7d_dballe_class_ndati.F90 \
#vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
#vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90 vol7d_dballe_class_var_du.F90
#vol7d_dballeold_class.lo: vol7d_class.$(OBJEXT) \
#vol7d_dballe_class_nana.F90 vol7d_dballe_class_ndati.F90 \
#vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
#vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90  vol7d_dballe_class_var_du.F90

vol7d_dballe_class.$(OBJEXT): vol7d_class.$(OBJEXT) \
vol7d_dballe_class_var_du.F90
vol7d_dballe_class.lo: vol7d_class.$(OBJEXT) \
vol7d_dballe_class_var_du.F90

check_PROGRAMS += vol7d_dballe_test  vol7d_dballe_test2
vol7d_dballe_test_SOURCES = vol7d_dballe_test.F90
vol7d_dballe_test_LDADD = libsim_vol7d.la
vol7d_dballe_test2_SOURCES = vol7d_dballe_test2.F90
vol7d_dballe_test2_LDADD = libsim_vol7d.la
dist_check_SCRIPTS += vol7d_dballe_test.sh

if F2003_EXTENDED_FEATURES
check_PROGRAMS += dballe_test 
check_PROGRAMS += dballe_test2
check_PROGRAMS += dballe_test3
dballe_test_SOURCES = dballe_test.F03
dballe_test2_SOURCES = dballe_test2.F03
dballe_test3_SOURCES = dballe_test3.F03
dballe_test_LDADD = libsim_vol7d.la
dballe_test2_LDADD = libsim_vol7d.la
dballe_test3_LDADD = libsim_vol7d.la
dist_check_SCRIPTS += dballe_test.sh
endif
endif

if ALCHIMIA
libsim_vol7d_la_SOURCES += vol7d_alchimia_class.F03
AM_FCFLAGS += -I../alchimia
libsim_vol7d_la_LIBADD += ../alchimia/libsim_alchimia.la
endif

EXTRA_DIST += \
  dballe_test.bufr dballe_test_ana.bufr\
  dballe_test_copy1fmem.test  dballe_test_copy1f.test\
  dballe_test2_memdb.test  dballe_test2.test\
  dballe_test3.test\
  vol7d_dballe_test.bufr     vol7d_dballe_test.test \
  vol7d_dballe_test2.bufr    vol7d_dballe_test2.test

if NETCDF
vol7d_netcdf_class.$(OBJEXT): vol7d_class.$(OBJEXT)
vol7d_netcdf_class.lo: vol7d_class.$(OBJEXT)
endif

if F2003_FEATURES
vol7d_serialize_csv_class.$(OBJEXT): vol7d_serialize_class.$(OBJEXT)
vol7d_serialize_csv_class.lo: vol7d_serialize_class.$(OBJEXT)
vol7d_serialize_csvdba_class.$(OBJEXT): vol7d_serialize_class.$(OBJEXT)
vol7d_serialize_csvdba_class.lo: vol7d_serialize_class.$(OBJEXT)
vol7d_serialize_geojson_class.$(OBJEXT): vol7d_serialize_class.$(OBJEXT)
vol7d_serialize_geojson_class.lo: vol7d_serialize_class.$(OBJEXT)
if DBALLE
vol7d_serialize_dballe_class.$(OBJEXT): vol7d_serialize_class.$(OBJEXT)
vol7d_serialize_dballe_class.lo: vol7d_serialize_class.$(OBJEXT)
endif
endif


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
