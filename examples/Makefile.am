if EXAMPLES

AM_FCFLAGS = -I../base -I../grib -I../volgrid6d -I../vol7d -I../qc -I../termolib -I../log4fortran  -I../graphics -I../alchimia
EXTRA_DIST = log4crc

noinst_PROGRAMS = esempio_file_utilities esempio_v7d esempio_v7d_compute esempio_termolib log4fortran example_optionparser example_char_utilities example_simple_stat example_space_utilities example_for2r

esempio_file_utilities_SOURCES = esempio_file_utilities.f90
esempio_file_utilities_LDADD = ../base/libsim_base.la

example_simple_stat_SOURCES = example_simple_stat.f90
example_simple_stat_LDADD = ../base/libsim_base.la

example_space_utilities_SOURCES = example_space_utilities.F90
example_space_utilities_LDADD = ../base/libsim_base.la

example_for2r_SOURCES = example_for2r.f90
example_for2r_LDADD = ../base/libsim_base.la

if NCARG
example_space_utilities_LDADD += ../graphics/libsim_graphics.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la  ../log4fortran/liblog4fortran.la ../termolib/libsim_termolib.la

endif

esempio_v7d_SOURCES = esempio_v7d.f90
esempio_v7d_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_v7d_compute_SOURCES = esempio_v7d_compute.f90
esempio_v7d_compute_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

esempio_termolib_SOURCES  = esempio_termolib.f90 
esempio_termolib_LDADD = ../base/libsim_base.la ../termolib/libsim_termolib.la 

log4fortran_SOURCES = log4fortran.f90
log4fortran_LDADD =  ../log4fortran/liblog4fortran.la ../base/libsim_base.la

example_optionparser_SOURCES = example_optionparser.F90
example_optionparser_LDADD =  ../log4fortran/liblog4fortran.la ../base/libsim_base.la

example_char_utilities_SOURCES = example_char_utilities.f90
example_char_utilities_LDADD =   ../base/libsim_base.la


example_list_SOURCES = example_list.F03
example_list_LDADD = ../base/libsim_base.la
noinst_PROGRAMS += example_list


if DBALLE
noinst_PROGRAMS += esempio_v7ddballe esempio_v7ddballe_multi esempio_v7ddballe_import_export esempio_v7ddballe_export  example_v7ddballe_v7d2dba esempio_v7ddballe_move_and_collapse

noinst_PROGRAMS += example_dballe example_dballe_read_list example_dballe_read_reallist example_dballe_read_vector example_dballe_read_real example_v7ddballenew_dba2v7d

example_dballe_SOURCES = example_dballe.F03
example_dballe_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_dballe_read_list_SOURCES = example_dballe_read_list.F03
example_dballe_read_list_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_dballe_read_reallist_SOURCES = example_dballe_read_reallist.F03
example_dballe_read_reallist_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_dballe_read_vector_SOURCES = example_dballe_read_vector.F03
example_dballe_read_vector_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_dballe_read_real_SOURCES = example_dballe_read_real.F03
example_dballe_read_real_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_v7ddballenew_dba2v7d_SOURCES = example_v7ddballenew_dba2v7d.f90
example_v7ddballenew_dba2v7d_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

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

# example_ncarg_sounding cannnot work without dballe
if NCARG
noinst_PROGRAMS += example_ncarg_sounding

example_ncarg_sounding_SOURCES = example_ncarg_sounding.f90
example_ncarg_sounding_LDADD = ../graphics/libsim_graphics.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la  ../log4fortran/liblog4fortran.la ../termolib/libsim_termolib.la

endif

if SHAPELIB
noinst_PROGRAMS +=esempio_qc_convert
esempio_qc_convert_SOURCES = esempio_qc_convert.f90
esempio_qc_convert_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la ../qc/libsim_qc.la
endif

endif


if GRIBAPI
noinst_PROGRAMS += example_vg6d_1 example_vg6d_2 example_vg6d_3 example_vg6d_4 example_vg6d_5 example_vg6d_9

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

example_vg6d_9_SOURCES = example_vg6d_9.f90
example_vg6d_9_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

if DBALLE
noinst_PROGRAMS += example_vg6d_6 example_vg6d_7 example_vg6d_8

example_vg6d_6_SOURCES = example_vg6d_6.f90
example_vg6d_6_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_vg6d_7_SOURCES = example_vg6d_7.f90
example_vg6d_7_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

example_vg6d_8_SOURCES = example_vg6d_8.f90
example_vg6d_8_LDADD = ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../log4fortran/liblog4fortran.la

endif
endif

noinst_PROGRAMS += example_alchimia example_alchimiavg6d 

example_alchimia_SOURCES = example_alchimia.f03
example_alchimia_LDADD = ../alchimia/libsim_alchimia.la  ../log4fortran/liblog4fortran.la

example_alchimiavg6d_SOURCES = example_alchimiavg6d.f03
example_alchimiavg6d_LDADD = ../alchimia/libsim_alchimia.la  ../volgrid6d/libsim_volgrid6d.la ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../termolib/libsim_termolib.la ../log4fortran/liblog4fortran.la

if DBALLE
noinst_PROGRAMS += example_alchimiav7d

example_alchimiav7d_SOURCES = example_alchimiav7d.f03
example_alchimiav7d_LDADD = ../alchimia/libsim_alchimia.la  ../vol7d/libsim_vol7d.la ../base/libsim_base.la  ../termolib/libsim_termolib.la ../log4fortran/liblog4fortran.la

endif

if VAPOR
noinst_PROGRAMS += example_vapor_interface

example_vapor_interface_SOURCES = example_vapor_interface.F03
example_vapor_interface_LDADD = ../volgrid6d/libsim_volgrid6d.la

endif

# EXAMPLES
endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod *.1
