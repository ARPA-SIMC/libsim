lib_LTLIBRARIES = libsim_vol7d.la
libsim_vol7d_la_SOURCES = vol7d_ana_class.F90 vol7d_class.F90 \
 vol7d_level_class.F90 vol7d_network_class.F90 \
 vol7d_timerange_class.F90 vol7d_utilities.F90 \
 vol7d_var_class.F90 vol7d_varvect_class.f90 vol7d_class_compute.F90
# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS = -I../base -I../log4fortran
# Opzioni C
AM_CFLAGS = -D_REENTRANT
# Opzioni preprocessore
AM_CPPFLAGS =
# Opzioni linker
AM_LDFLAGS =
# Librerie aggiuntive
libsim_vol7d_la_LIBADD = ../base/libsim_base.la

if ORSIM
libsim_vol7d_la_SOURCES += vol7d_oraclesim_class.f90 oraclesim.c
AM_CPPFLAGS += $(ORA_CPPFLAGS)
AM_LDFLAGS += $(ORA_LDFLAGS)
libsim_vol7d_la_LIBADD += $(ORA_LIBS)
endif

if DBALLE
libsim_vol7d_la_SOURCES += vol7d_dballe_class.F90
AM_FCFLAGS += -I/usr/include/dballe
libsim_vol7d_la_LIBADD += $(DBALLE_LIBS)
endif

if NETCDF
libsim_vol7d_la_SOURCES += vol7d_netcdf_class.f90
AM_FCFLAGS += -I/usr/include
libsim_vol7d_la_LIBADD += $(NETCDF_LIBS)
endif

noinst_HEADERS = vol7d_class_type_templ.F90 vol7d_class_desc_templ.F90 \
vol7d_distinct.F90 vol7d_class_diff.F90 vol7d_class_diff_only.f90 \
vol7d_dballe_class_nana.f90 vol7d_dballe_class_ndati.f90 \
vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90 vol7d_dballe_class_var_du.f90

# c'e` una maniera migliore?
tmpvar = $(libsim_vol7d_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

vol7d_class_compute.$(OBJEXT): vol7d_class.$(OBJEXT)
vol7d_class_compute.lo: vol7d_class.$(OBJEXT)
#vol7d_ana_class.$(OBJEXT):
vol7d_class.$(OBJEXT): vol7d_utilities.$(OBJEXT) vol7d_ana_class.$(OBJEXT) \
 vol7d_timerange_class.$(OBJEXT) vol7d_level_class.$(OBJEXT) \
 vol7d_network_class.$(OBJEXT) vol7d_varvect_class.$(OBJEXT) \
 vol7d_class_type_templ.F90 vol7d_class_desc_templ.F90 vol7d_class_diff.F90 vol7d_class_diff_only.f90
vol7d_class.lo: vol7d_utilities.$(OBJEXT) vol7d_ana_class.$(OBJEXT) \
 vol7d_timerange_class.$(OBJEXT) vol7d_level_class.$(OBJEXT) \
 vol7d_network_class.$(OBJEXT) vol7d_varvect_class.$(OBJEXT) \
 vol7d_class_type_templ.F90 vol7d_class_desc_templ.F90 vol7d_class_diff.F90 vol7d_class_diff_only.f90
#vol7d_level_class.$(OBJEXT):
#vol7d_network_class.$(OBJEXT):
if ORSIM
vol7d_oraclesim_class.$(OBJEXT): vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
vol7d_oraclesim_class.lo: vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
endif

if DBALLE
vol7d_dballe_class.$(OBJEXT): vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT) \
vol7d_dballe_class_nana.f90 vol7d_dballe_class_ndati.f90 \
vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90 vol7d_dballe_class_var_du.f90


vol7d_dballe_class.lo: vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT) \
vol7d_dballe_class_nana.f90 vol7d_dballe_class_ndati.f90 \
vol7d_dballe_class_dati.F90 vol7d_dballe_class_datiattr.F90 \
vol7d_dballe_class_ana.F90 vol7d_dballe_class_anaattr.F90  vol7d_dballe_class_var_du.f90
endif


if NETCDF
vol7d_netcdf_class.$(OBJEXT): vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
vol7d_netcdf_class.lo: vol7d_class.$(OBJEXT) vol7d_utilities.$(OBJEXT)
endif


#vol7d_timerange_class.$(OBJEXT):
#vol7d_utilities.$(OBJEXT):
#vol7d_utilities_orig.$(OBJEXT):
#vol7d_var_class.$(OBJEXT):
vol7d_varvect_class.$(OBJEXT): vol7d_var_class.$(OBJEXT)
vol7d_varvect_class.lo: vol7d_var_class.$(OBJEXT)

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
