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

if ORSIM
libsim_vol7d_la_SOURCES += vol7d_oraclesim_class.f90 oraclesim.c
AM_CPPFLAGS += $(ORA_CPPFLAGS)
libsim_vol7d_la_LDFLAGS += $(ORA_LDFLAGS)
libsim_vol7d_la_LIBADD += $(ORA_LIBS)
endif

if DBALLE
libsim_vol7d_la_SOURCES += vol7d_dballe_class.F90
AM_FCFLAGS += -I/usr/include/dballe
libsim_vol7d_la_LIBADD += $(DBALLE_LIBS)
endif

if NETCDF
libsim_vol7d_la_SOURCES += vol7d_netcdf_class.f90
libsim_vol7d_la_LIBADD += $(NETCDF_LIBS)
endif

if F2003_FEATURES
libsim_vol7d_la_SOURCES += vol7d_serialize_class.F03 vol7d_serialize_csv_class.F03
endif

if VERSIONINFO
libsim_vol7d_la_LDFLAGS += -version-info @LIBSIM_VERSION_INFO@
endif

noinst_HEADERS = vol7d_class_type_templ.F90 vol7d_class_desc_templ.F90 \
vol7d_oraclesim_class_vartable.F90 \
vol7d_class_diff.F90 vol7d_class_diff_only.F90 \
vol7d_dballe_class_nana.F90 vol7d_dballe_class_ndati.F90 \
vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90 vol7d_dballe_class_var_du.F90

if ALCHIMIA
libsim_vol7d_la_SOURCES += vol7d_alchimia_class.F03
AM_FCFLAGS += -I../alchimia
libsim_vol7d_la_LIBADD += ../alchimia/libsim_alchimia.la
endif

# c'e` una maniera migliore?
tmpvar = $(libsim_vol7d_la_SOURCES:.f90=.mod)
tmpvar1 = $(tmpvar:.f03=.mod)
tmpvar2 = $(tmpvar1:.F03=.mod)
tmpvar3 = $(tmpvar2:vdf4f_c.cpp=)
tmpvar4 = $(tmpvar3:oraclesim.c=)
nodist_include_HEADERS = $(tmpvar4:.F90=.mod)

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
if ORSIM
vol7d_oraclesim_class.$(OBJEXT): vol7d_class.$(OBJEXT) vol7d_oraclesim_class_vartable.F90
vol7d_oraclesim_class.lo: vol7d_class.$(OBJEXT) vol7d_oraclesim_class_vartable.F90
endif

if DBALLE
vol7d_dballe_class.$(OBJEXT): vol7d_class.$(OBJEXT) \
vol7d_dballe_class_nana.F90 vol7d_dballe_class_ndati.F90 \
vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90 vol7d_dballe_class_var_du.F90
vol7d_dballe_class.lo: vol7d_class.$(OBJEXT) \
vol7d_dballe_class_nana.F90 vol7d_dballe_class_ndati.F90 \
vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90  vol7d_dballe_class_var_du.F90
endif

if NETCDF
vol7d_netcdf_class.$(OBJEXT): vol7d_class.$(OBJEXT)
vol7d_netcdf_class.lo: vol7d_class.$(OBJEXT)
endif

if F2003_FEATURES
vol7d_serialize_csv_class.$(OBJEXT): vol7d_serialize_class.$(OBJEXT)
vol7d_serialize_csv_class.lo: vol7d_serialize_class.$(OBJEXT)
endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
