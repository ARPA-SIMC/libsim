AM_FCFLAGS = -I../base -I../volgrid6d -I../vol7d -I../qc -I../termolib -I../log4fortran  -I../graphics -I../alchimia
#EXTRA_DIST = log4crc
bin_PROGRAMS =
dist_check_SCRIPTS = 
check_PROGRAMS = 
dist_bin_SCRIPTS =
pkglibexec_PROGRAMS =

if NCARG
bin_PROGRAMS += v7d_plot_sound
v7d_plot_sound_SOURCES = v7d_plot_sound.F90
v7d_plot_sound_LDADD = ../graphics/libsim_graphics.la  ../vol7d/libsim_vol7d.la  ../log4fortran/liblog4fortran.la ../termolib/libsim_termolib.la ../base/libsim_base.la
endif

if DBALLE

bin_PROGRAMS += dba_qcfilter dba_transform
dba_qcfilter_SOURCES = dba_qcfilter.F03 
dba_qcfilter_LDADD =  ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la ../base/libsim_base.la

dba_transform_SOURCES = dba_transform.F03 
dba_transform_LDADD =  ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la ../base/libsim_base.la

dist_check_SCRIPTS += dba_qcfilter_test.sh

if SHAPELIB
bin_PROGRAMS += v7d_qccli v7d_qctem

v7d_qccli_SOURCES = v7d_qccli.F90
v7d_qccli_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la

v7d_qctem_SOURCES = v7d_qctem.F90
v7d_qctem_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la

if NCARG
# v7d_qcspa is now here because it requires modqcspa
# which requires NCAR for triangulations
bin_PROGRAMS += v7d_qcspa

v7d_qcspa_SOURCES = v7d_qcspa.F90
v7d_qcspa_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la

v7d_qcspa_LDADD += ../graphics/libsim_graphics.la ../termolib/libsim_termolib.la
endif
endif
endif


if DBALLE
pkglibexec_PROGRAMS += v7d_pollini

v7d_pollini_SOURCES = v7d_pollini.f90
v7d_pollini_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la
endif

bin_PROGRAMS += v7d_transform
v7d_transform_SOURCES = v7d_transform.F90
v7d_transform_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../qc/libsim_qc.la ../volgrid6d/libsim_volgrid6d.la ../log4fortran/liblog4fortran.la

if ALCHIMIA
v7d_transform_LDADD += ../alchimia/libsim_alchimia.la ../termolib/libsim_termolib.la
endif

pkglibexec_PROGRAMS += v7d_add_wmo_info
v7d_add_wmo_info_SOURCES = v7d_add_wmo_info.F90
v7d_add_wmo_info_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

bin_PROGRAMS += vg6d_getpoint vg6d_transform

if GRIBAPI
vg6d_getpoint_SOURCES = grib_api_csv.F90 vg6d_getpoint.F90
pkglibexec_PROGRAMS += vg6d_getpoint_pkauf
vg6d_getpoint_pkauf_SOURCES = grib_api_csv.F90 vg6d_getpoint_pkauf.F90
if DBALLE
pkglibexec_PROGRAMS += vg6d_tcorr
#noinst_PROGRAMS = vg6d_column
vg6d_tcorr_SOURCES = vg6d_tcorr.F90
vg6d_tcorr_LDADD = ../volgrid6d/libsim_volgrid6d.la  ../vol7d/libsim_vol7d.la ../base/libsim_base.la ../log4fortran/liblog4fortran.la
#vg6d_column_SOURCES = vg6d_column.F90
#vg6d_column_LDADD = ../volgrid6d/libsim_volgrid6d.la  ../vol7d/libsim_vol7d.la ../base/libsim_base.la ../log4fortran/liblog4fortran.la
endif
else
vg6d_getpoint_SOURCES = vg6d_getpoint.F90
endif

if NETCDF
bin_PROGRAMS += simc_nc2grib
simc_nc2grib_SOURCES = simc_nc2grib.f90
simc_nc2grib_LDADD = ../volgrid6d/libsim_volgrid6d.la  ../vol7d/libsim_vol7d.la ../base/libsim_base.la ../log4fortran/liblog4fortran.la $(NETCDF_LIBS)
endif

vg6d_getpoint_LDADD = ../volgrid6d/libsim_volgrid6d.la  ../vol7d/libsim_vol7d.la ../base/libsim_base.la ../log4fortran/liblog4fortran.la
vg6d_getpoint_pkauf_LDADD = ../volgrid6d/libsim_volgrid6d.la  ../vol7d/libsim_vol7d.la ../base/libsim_base.la ../log4fortran/liblog4fortran.la

vg6d_transform_SOURCES = vg6d_transform.F90
vg6d_transform_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

if ALCHIMIA
vg6d_transform_LDADD += ../alchimia/libsim_alchimia.la ../termolib/libsim_termolib.la  
endif

if GRIBAPI
dist_bin_SCRIPTS += vg6d_subarea
endif

# improve
if GRIBAPI
if SHAPELIB
dist_check_SCRIPTS += getpoint_transform_test.sh
endif
dist_check_SCRIPTS += stat_proc_test.sh
endif

TESTS = $(check_PROGRAMS) $(dist_check_SCRIPTS)
TESTS_ENVIRONMENT = $(top_srcdir)/testenv

if HELP2MAN
man_MANS = v7d_transform.1 v7d_add_wmo_info.1 vg6d_getpoint.1 vg6d_transform.1

if NCARG
man_MANS += v7d_plot_sound.1
endif
if DBALLE
man_MANS += dba_qcfilter.1 dba_transform.1
endif

%.1: %
	help2man --name=$< --section=1 --no-info --output=$@ ./$<
endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod *.1
