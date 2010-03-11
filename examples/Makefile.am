AM_FCFLAGS = -I../base -I../grib -I../volgrid6d -I../vol7d -I../qc -I../termolib -I../log4fortran  -I../graphics
EXTRA_DIST = log4crc
bin_PROGRAMS =

noinst_PROGRAMS = esempio_file_utilities esempio_v7d esempio_termolib log4fortran example_getopt

esempio_file_utilities_SOURCES = esempio_file_utilities.f90
esempio_file_utilities_LDADD = ../base/libsim_base.la

esempio_v7d_SOURCES = esempio_v7d.f90
esempio_v7d_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_termolib_SOURCES  = esempio_termolib.f90 
esempio_termolib_LDADD = ../base/libsim_base.la ../termolib/libsim_termolib.la 

log4fortran_SOURCES = log4fortran.f90
log4fortran_LDADD =  ../log4fortran/liblog4fortran.la ../base/libsim_base.la

example_getopt_SOURCES = example_getopt.f90
example_getopt_LDADD =  ../log4fortran/liblog4fortran.la ../base/libsim_base.la


if GRIB
noinst_PROGRAMS += esempio_regrib

esempio_regrib_SOURCES = esempio_regrib.f90
esempio_regrib_LDADD = ../base/libsim_base.la ../grib/libsim_grib.la
endif


if DBALLE
noinst_PROGRAMS += esempio_v7ddballe esempio_v7ddballe_multi esempio_v7ddballe_import_export esempio_v7ddballe_export esempio_qccli esempio_qc_convert example_v7ddballe_v7d2dba esempio_v7ddballe_move_and_collapse

esempio_v7ddballe_SOURCES = esempio_v7ddballe.f90
esempio_v7ddballe_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_multi_SOURCES = esempio_v7ddballe_multi.f90
esempio_v7ddballe_multi_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_import_export_SOURCES = esempio_v7ddballe_import_export.f90
esempio_v7ddballe_import_export_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_export_SOURCES = esempio_v7ddballe_export.f90
esempio_v7ddballe_export_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_v7ddballe_v7d2dba_SOURCES = example_v7ddballe_v7d2dba.f90
example_v7ddballe_v7d2dba_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_move_and_collapse_SOURCES = esempio_v7ddballe_move_and_collapse.f90
esempio_v7ddballe_move_and_collapse_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la


if NETCDF
noinst_PROGRAMS += esempio_v7ddballe_export_file esempio_v7ddballe_import_file esempio_v7dnetcdf

esempio_v7ddballe_export_file_SOURCES = esempio_v7ddballe_export_file.f90
esempio_v7ddballe_export_file_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_import_file_SOURCES = esempio_v7ddballe_import_file.f90
esempio_v7ddballe_import_file_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7dnetcdf_SOURCES = esempio_v7dnetcdf.f90
esempio_v7dnetcdf_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la  ../log4fortran/liblog4fortran.la
endif

if NCARG
noinst_PROGRAMS += example_ncarg_sounding

example_ncarg_sounding_SOURCES = example_ncarg_sounding.f90
example_ncarg_sounding_LDADD = ../graphics/libsim_graphics.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la  ../log4fortran/liblog4fortran.la ../termolib/libsim_termolib.la


bin_PROGRAMS += v7d_plot_sound

v7d_plot_sound_SOURCES = v7d_plot_sound.f90
v7d_plot_sound_LDADD = ../graphics/libsim_graphics.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la  ../log4fortran/liblog4fortran.la ../termolib/libsim_termolib.la


endif

esempio_qccli_SOURCES = esempio_qccli.f90
esempio_qccli_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la

esempio_qc_convert_SOURCES = esempio_qc_convert.f90
esempio_qc_convert_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la

endif


if ORSIM
noinst_PROGRAMS += esempio_v7doracle esempio_v7doracle_multi

esempio_v7doracle_SOURCES = esempio_v7doracle.f90
esempio_v7doracle_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7doracle_multi_SOURCES = esempio_v7doracle_multi.f90
esempio_v7doracle_multi_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

bin_PROGRAMS += v7d_pollini

v7d_pollini_SOURCES = v7d_pollini.f90
v7d_pollini_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

endif

bin_PROGRAMS += v7d_transform

v7d_transform_SOURCES = v7d_transform.F90
v7d_transform_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

if SHP
noinst_PROGRAMS += esempio_geo_coordvect

esempio_geo_coordvect_SOURCES = esempio_geo_coordvect.f90
esempio_geo_coordvect_LDADD =  ../base/libsim_base.la
endif

if GRIBAPI
noinst_PROGRAMS += example_vg6d_1 example_vg6d_2 example_vg6d_3 example_vg6d_4 example_vg6d_5

example_vg6d_1_SOURCES = example_vg6d_1.f90
example_vg6d_1_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_vg6d_2_SOURCES = example_vg6d_2.f90
example_vg6d_2_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_vg6d_3_SOURCES = example_vg6d_3.f90
example_vg6d_3_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_vg6d_4_SOURCES = example_vg6d_4.f90
example_vg6d_4_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_vg6d_5_SOURCES = example_vg6d_5.f90
example_vg6d_5_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

if DBALLE
noinst_PROGRAMS += example_vg6d_6 example_vg6d_7 example_vg6d_8

example_vg6d_6_SOURCES = example_vg6d_6.f90
example_vg6d_6_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_vg6d_7_SOURCES = example_vg6d_7.f90
example_vg6d_7_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_vg6d_8_SOURCES = example_vg6d_8.f90
example_vg6d_8_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

endif

bin_PROGRAMS += vg6d_getpoint vg6d_subarea vg6d_transform

vg6d_getpoint_SOURCES = vg6d_getpoint.f90
vg6d_getpoint_LDADD = ../volgrid6d/libsim_volgrid6d.la  ../vol7d/libsim_vol7d.la ../base/libsim_base.la ../log4fortran/liblog4fortran.la

vg6d_subarea_SOURCES = vg6d_subarea.f90
vg6d_subarea_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

vg6d_transform_SOURCES = vg6d_transform.f90
vg6d_transform_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la
endif

