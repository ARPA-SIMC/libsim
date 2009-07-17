lib_LTLIBRARIES = libsim_volgrid6d.la

# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS = -I../log4fortran -I../base -I../vol7d
# Opzioni linker
AM_LDFLAGS =
# Librerie aggiuntive
libsim_volgrid6d_la_LIBADD = ../log4fortran/liblog4fortran.la \
 ../base/libsim_base.la ../vol7d/libsim_vol7d.la

#noinst_HEADERS = ../vol7d/vol7d_distinct.F90

# c'e` una maniera migliore?
tmpvar = $(libsim_volgrid6d_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)


if GRIBAPI
libsim_volgrid6d_la_SOURCES = grid_dim_class.F90 gridpar_generic_class.F90 \
 gridpar_rotated_class.F90 gridpar_polarproj_class.F90 gridpar_stretched_class.F90 \
 grid_class.F90 volgrid6d_var_class.F90 volgrid6d_class.F90 \
 gridinfo_class.F90
libsim_volgrid6d_la_LIBADD += $(GRIBAPI_LIBS)
endif

if NGMATH
AM_LDFLAGS += $(NGMATH_LDFLAGS)
libsim_volgrid6d_la_LIBADD += $(NGMATH_LIBS)
endif

if GRIBAPI
grid_class.o: grid_dim_class.$(OBJEXT) gridpar_generic_class.$(OBJEXT) gridpar_rotated_class.$(OBJEXT) gridpar_stretched_class.$(OBJEXT) gridpar_polarproj_class.$(OBJEXT)
grid_class.lo: grid_dim_class.$(OBJEXT) gridpar_generic_class.$(OBJEXT) gridpar_rotated_class.$(OBJEXT) gridpar_stretched_class.$(OBJEXT) gridpar_polarproj_class.$(OBJEXT)
volgrid6d_var_class.o: ../vol7d/vol7d_distinct.F90
volgrid6d_var_class.lo: ../vol7d/vol7d_distinct.F90
volgrid6d_class.o: volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT) gridinfo_class.$(OBJEXT)
volgrid6d_class.lo: volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT) gridinfo_class.$(OBJEXT)
gridinfo_class.o: volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT)
gridinfo_class.lo: volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT)
endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
