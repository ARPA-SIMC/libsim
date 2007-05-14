bin_PROGRAMS = esempio_v7d esempio_regrib

if ORSIM
bin_PROGRAMS += esempio_v7doracle esempio_v7doracle_multi
endif

if SHP
bin_PROGRAMS += esempio_geo_coordvect
endif

LDADD = ../base/libsim_base.la ../grib/libsim_grib.la ../vol7d/libsim_vol7d.la

AM_FCFLAGS = -I../base -I../grib -I../vol7d

esempio_v7d_SOURCES = esempio_v7d.f90
esempio_v7d_LDADD = $(LDADD) $(liboraclesim)

esempio_regrib_SOURCES = esempio_regrib.f90
esempio_regrib_LDADD = $(LDADD) -lemos

if ORSIM
esempio_v7doracle_SOURCES = esempio_v7doracle.f90
esempio_v7doracle_LDADD = $(LDADD) $(liboraclesim)

esempio_v7doracle_multi_SOURCES = esempio_v7doracle_multi.f90
esempio_v7doracle_multi_LDADD = $(LDADD) $(liboraclesim)
endif

if SHP
esempio_geo_coordvect_SOURCES = esempio_geo_coordvect.f90
esempio_geo_coordvect_LDADD = $(LDADD) $(liboraclesim)
endif
