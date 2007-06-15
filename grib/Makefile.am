lib_LTLIBRARIES = libsim_grib.la
libsim_grib_la_SOURCES = geo_grid_class.f90 grib_io_class.f90 grib_grid_class.f90 matchgrib_class.f90
#impgrib.f90

#libsim_grib_la_FFLAGS = -I../base
AM_FCFLAGS = -I../base
libsim_grib_la_LIBADD = ../base/libsim_base.la

# c'e` una maniera migliore?
tmpvar = $(libsim_grib_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

grib_grid_class.$(OBJEXT): geo_grid_class.$(OBJEXT) grib_io_class.$(OBJEXT)
grib_grid_class.lo: geo_grid_class.$(OBJEXT) grib_io_class.$(OBJEXT)
grib_io_class.$(OBJEXT): matchgrib_class.$(OBJEXT)
grib_io_class.lo: matchgrib_class.$(OBJEXT)

