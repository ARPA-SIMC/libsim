lib_LTLIBRARIES = libsim_volgrid6d.la
libsim_volgrid6d_la_SOURCES = grid_dim_class.F90 grid_rect_class.F90

if GDAL
libsim_volgrid6d_la_SOURCES += gdal.f90
endif

libsim_volgrid6d_la_SOURCES += grid_id_class.F90 grid_class.F90 \
 grid_transform_class.F90 volgrid6d_var_class.F90 gridinfo_class.F90 \
 volgrid6d_class.F90

# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS = -I../log4fortran -I../base -I../vol7d
# Opzioni linker
AM_LDFLAGS =
libsim_volgrid6d_la_LDFLAGS =
# Librerie aggiuntive
libsim_volgrid6d_la_LIBADD = ../log4fortran/liblog4fortran.la \
 ../base/libsim_base.la ../vol7d/libsim_vol7d.la

#noinst_HEADERS = ../vol7d/vol7d_distinct.F90

# c'e` una maniera migliore?
tmpvar = $(libsim_volgrid6d_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)

if GRIBAPI
libsim_volgrid6d_la_LIBADD += $(GRIBAPI_LIBS)
endif

if GDAL
libsim_volgrid6d_la_LIBADD += $(GDAL_LIBS)
endif

if NGMATH
libsim_volgrid6d_la_LIBADD += $(NGMATH_LIBS)
libsim_volgrid6d_la_LDFLAGS += $(NCARG_LDFLAGS)

endif

if VERSIONINFO
libsim_volgrid6d_la_LDFLAGS += -version-info @LIBSIM_VERSION_INFO@
endif

if GDAL
grid_id_class.o: gdal.$(OBJEXT)
grid_id_class.lo: gdal.$(OBJEXT)
endif

grid_class.o: grid_dim_class.$(OBJEXT) grid_rect_class.$(OBJEXT) grid_id_class.$(OBJEXT) ../vol7d/vol7d_distinct.F90
grid_class.lo: grid_dim_class.$(OBJEXT) grid_rect_class.$(OBJEXT) grid_id_class.$(OBJEXT) ../vol7d/vol7d_distinct.F90
grid_transform_class.o: grid_class.$(OBJEXT) 
grid_transform_class.lo: grid_class.$(OBJEXT) 
volgrid6d_var_class.o: ../vol7d/vol7d_distinct.F90
volgrid6d_var_class.lo: ../vol7d/vol7d_distinct.F90
gridinfo_class.o: volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT) grid_id_class.$(OBJEXT)
gridinfo_class.lo: volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT) grid_id_class.$(OBJEXT)
volgrid6d_class.o: volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT) grid_transform_class.$(OBJEXT) gridinfo_class.$(OBJEXT) grid_id_class.$(OBJEXT)
volgrid6d_class.lo: volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT) grid_transform_class.$(OBJEXT) gridinfo_class.$(OBJEXT) grid_id_class.$(OBJEXT)


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
