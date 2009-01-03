AM_FCFLAGS = -I../base -I../grib -I../volgrid6d -I../vol7d -I../qc -I../termolib -I../log4fortran 
EXTRA_DIST = log4crc

noinst_PROGRAMS = esempio_file_utilities esempio_v7d esempio_qc_convert esempio_termolib log4fortran example_getopt

esempio_file_utilities_SOURCES = esempio_file_utilities.f90
esempio_file_utilities_LDADD = ../base/libsim_base.la

esempio_v7d_SOURCES = esempio_v7d.f90
esempio_v7d_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_qc_convert_SOURCES = esempio_qc_convert.f90
esempio_qc_convert_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_termolib_SOURCES  = esempio_termolib.f90 
esempio_termolib_LDADD = ../base/libsim_base.la ../termolib/libsim_termolib.la 

log4fortran_SOURCES = log4fortran.f90
log4fortran_LDADD =  ../log4fortran/liblog4fortran.la ../base/libsim_base.la

example_getopt_SOURCES = example_getopt.f90
example_getopt_LDADD =  ../base/libsim_base.la


if GRIB
noinst_PROGRAMS += esempio_regrib

esempio_regrib_SOURCES = esempio_regrib.f90
esempio_regrib_LDADD = ../base/libsim_base.la ../grib/libsim_grib.la
endif

if ORSIM
noinst_PROGRAMS += esempio_v7doracle esempio_v7doracle_multi

esempio_v7doracle_SOURCES = esempio_v7doracle.f90
esempio_v7doracle_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7doracle_multi_SOURCES = esempio_v7doracle_multi.f90
esempio_v7doracle_multi_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la
endif

if DBALLE
noinst_PROGRAMS += esempio_v7ddballe esempio_v7ddballe_multi esempio_v7ddballe_import_export esempio_v7ddballe_export esempio_qccli esempio_v7ddballe_export_file esempio_v7ddballe_import_file 

esempio_v7ddballe_SOURCES = esempio_v7ddballe.f90
esempio_v7ddballe_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_multi_SOURCES = esempio_v7ddballe_multi.f90
esempio_v7ddballe_multi_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_import_export_SOURCES = esempio_v7ddballe_import_export.f90
esempio_v7ddballe_import_export_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_export_SOURCES = esempio_v7ddballe_export.f90
esempio_v7ddballe_export_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_export_file_SOURCES = esempio_v7ddballe_export_file.f90
esempio_v7ddballe_export_file_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7ddballe_import_file_SOURCES = esempio_v7ddballe_import_file.f90
esempio_v7ddballe_import_file_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_qccli_SOURCES = esempio_qccli.f90
esempio_qccli_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la
endif

if NETCDF
noinst_PROGRAMS += esempio_v7dnetcdf

esempio_v7dnetcdf_SOURCES = esempio_v7dnetcdf.f90
esempio_v7dnetcdf_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la  ../log4fortran/liblog4fortran.la
endif

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

endif
