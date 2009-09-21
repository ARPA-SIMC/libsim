if GRIB
lib_LTLIBRARIES = libsim_grib.la
libsim_grib_la_SOURCES =
AM_FCFLAGS = -I../base
libsim_grib_la_LIBADD = ../base/libsim_base.la 

if VERSIONINFO
libsim_grib_la_LDFLAGS = -version-info @LIBSIM_VERSION_INFO@
endif

if EMOS
libsim_grib_la_SOURCES += grib_io_class.F90 geo_grid_grib.F90 matchgrib_class.f90
libsim_grib_la_LIBADD += $(EMOS_LIBS)
endif

if DWDGRIB1
libsim_grib_la_SOURCES += dwdgrib1_io_class.F90 geo_grid_dwdgrib1.F90
libsim_grib_la_LIBADD += $(DWDGRIB1_LIBS)
endif

noinst_HEADERS = grib_io_class_templ.F90 geo_grid_grib_templ.F90

# c'e` una maniera migliore?
tmpvar = $(libsim_grib_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

if EMOS
grib_io_class.$(OBJEXT): matchgrib_class.$(OBJEXT) grib_io_class_templ.F90
grib_io_class.lo: matchgrib_class.$(OBJEXT) grib_io_class_templ.F90
geo_grid_grib.$(OBJEXT): grib_io_class.$(OBJEXT) geo_grid_grib_templ.F90
geo_grid_grib.lo: grib_io_class.$(OBJEXT) geo_grid_grib_templ.F90
endif
if DWDGRIB1
dwdgrib1_io_class.$(OBJEXT): grib_io_class_templ.F90
dwdgrib1_io_class.lo: grib_io_class_templ.F90
geo_grid_dwdgrib1.$(OBJEXT): dwdgrib1_io_class.$(OBJEXT) geo_grid_grib_templ.F90
geo_grid_dwdgrib1.lo: dwdgrib1_io_class.$(OBJEXT) geo_grid_grib_templ.F90
endif

endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod

