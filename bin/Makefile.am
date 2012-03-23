AM_FCFLAGS = -I../base -I../grib -I../volgrid6d -I../vol7d -I../qc -I../termolib -I../log4fortran  -I../graphics -I../alchimia
#EXTRA_DIST = log4crc
bin_PROGRAMS =

if NCARG
bin_PROGRAMS += v7d_plot_sound

v7d_plot_sound_SOURCES = v7d_plot_sound.F90
v7d_plot_sound_LDADD = ../graphics/libsim_graphics.la  ../vol7d/libsim_vol7d.la  ../log4fortran/liblog4fortran.la ../termolib/libsim_termolib.la ../base/libsim_base.la
endif

if DBALLE
bin_PROGRAMS += v7d_qccli v7d_qcspa

v7d_qccli_SOURCES = v7d_qccli.f90 
v7d_qccli_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la

v7d_qcspa_SOURCES = v7d_qcspa.F90 
v7d_qcspa_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la

if NCARG
v7d_qcspa_LDADD += ../graphics/libsim_graphics.la ../termolib/libsim_termolib.la
endif
endif


if ORSIM
bin_PROGRAMS += v7d_pollini

v7d_pollini_SOURCES = v7d_pollini.f90
v7d_pollini_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la
endif

bin_PROGRAMS += v7d_transform

v7d_transform_SOURCES = vol7d_csv.F90 v7d_transform.F90
v7d_transform_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../qc/libsim_qc.la ../volgrid6d/libsim_volgrid6d.la ../log4fortran/liblog4fortran.la

if ALCHIMIA
v7d_transform_LDADD += ../alchimia/libsim_alchimia.la ../termolib/libsim_termolib.la  
endif


bin_PROGRAMS += vg6d_getpoint vg6d_transform

if GRIBAPI
vg6d_getpoint_SOURCES = grib_api_csv.F90 vg6d_getpoint.F90
else
vg6d_getpoint_SOURCES = vg6d_getpoint.F90
endif
vg6d_getpoint_LDADD = ../volgrid6d/libsim_volgrid6d.la  ../vol7d/libsim_vol7d.la ../base/libsim_base.la ../log4fortran/liblog4fortran.la

vg6d_transform_SOURCES = vg6d_transform.F90
vg6d_transform_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

if ALCHIMIA
vg6d_transform_LDADD += ../alchimia/libsim_alchimia.la ../termolib/libsim_termolib.la  
endif

if GRIBAPI
bin_PROGRAMS += vg6d_subarea
vg6d_subarea_SOURCES = vg6d_subarea.F90
vg6d_subarea_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la
endif

if HELP2MAN

man_MANS = v7d_transform.1 vg6d_getpoint.1 vg6d_transform.1

if GRIBAPI
man_MANS += vg6d_subarea.1
endif
if NCARG
man_MANS += v7d_plot_sound.1
endif

# improve
if GRIBAPI
if SHAPELIB
check_SCRIPTS = getpoint_transform_test.sh
TESTS = $(check_SCRIPTS)
endif
endif

%.1: %
	help2man --name=$< --section=1 --no-info --output=$@ ./$<

endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod *.1
