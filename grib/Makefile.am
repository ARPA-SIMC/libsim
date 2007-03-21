lib_LTLIBRARIES = libsim_grib.la
libsim_grib_la_SOURCES = geo_grid.f90 grib_io.f90 \ #impgrib.f90
 grib_grid.f90 matchgrib.f90
#libsim_grib_la_FFLAGS = -I../base
AM_FFLAGS = -I../base
libsim_grib_la_LDADD = ../base/libsim_base.la

include_HEADERS = $(libsim_grib_la_SOURCES:.o=.mod)

grib_grid.$(OBJEXT): geo_grid.$(OBJEXT) grib_io.$(OBJEXT)
grib_io.$(OBJEXT): matchgrib_class.$(OBJEXT)

