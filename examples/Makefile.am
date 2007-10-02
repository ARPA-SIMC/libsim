bin_PROGRAMS = esempio_v7d esempio_regrib esempio_qc_convert esempio_termolib

if ORSIM
bin_PROGRAMS += esempio_v7doracle esempio_v7doracle_multi
endif

if DBALLE
bin_PROGRAMS += esempio_v7ddballe esempio_v7ddballe_multi esempio_v7ddballe_import_export esempio_qccli
endif

if SHP
bin_PROGRAMS += esempio_geo_coordvect
endif

#LDADD = ../base/libsim_base.la ../grib/libsim_grib.la ../vol7d/libsim_vol7d.la

AM_FCFLAGS = -I../base -I../grib -I../vol7d -I../qc -I../termolib

esempio_termolib_SOURCES  = esempio_termolib.f90 
esempio_v7d_SOURCES = esempio_v7d.f90
esempio_v7d_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la $(liboraclesim) $(libdballe)

esempio_regrib_SOURCES = esempio_regrib.f90
#esempio_regrib_LDADD = $(LDADD) -lemos  $(liboraclesim) $(libdballe)
esempio_regrib_LDADD = ../base/libsim_base.la ../grib/libsim_grib.la -lemos

esempio_qc_convert_SOURCES = esempio_qc_convert.f90
esempio_qc_convert_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la $(liboraclesim) $(libdballe)

esempio_termolib_LDADD = ../base/libsim_base.la ../termolib/libsim_termolib.la 

if ORSIM
esempio_v7doracle_SOURCES = esempio_v7doracle.f90
esempio_v7doracle_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la $(liboraclesim) $(libdballe)

esempio_v7doracle_multi_SOURCES = esempio_v7doracle_multi.f90
esempio_v7doracle_multi_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la $(liboraclesim) $(libdballe)
endif

if DBALLE
esempio_v7ddballe_SOURCES = esempio_v7ddballe.f90
esempio_v7ddballe_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la $(liboraclesim) $(libdballe)

esempio_v7ddballe_multi_SOURCES = esempio_v7ddballe_multi.f90
esempio_v7ddballe_multi_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la $(liboraclesim) $(libdballe)

esempio_v7ddballe_import_export_SOURCES = esempio_v7ddballe_import_export.f90
esempio_v7ddballe_import_export_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la $(liboraclesim) $(libdballe)

esempio_qccli_SOURCES = esempio_qccli.f90
esempio_qccli_LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../qc/libsim_qc.la $(liboraclesim) $(libdballe)

endif

if SHP
esempio_geo_coordvect_SOURCES = esempio_geo_coordvect.f90
esempio_geo_coordvect_LDADD =  ../base/libsim_base.la ../vol7d/libsim_vol7d.la $(liboraclesim) $(libdballe)
endif
