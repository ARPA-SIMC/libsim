lib_LTLIBRARIES = libsim_volgrid6d.la


# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS = -I../base -I../log4fortran -I../vol7d
# Opzioni linker
AM_LDFLAGS =
# Librerie aggiuntive
libsim_volgrid6d_la_LIBADD = ../base/libsim_base.la


#noinst_HEADERS = ../vol7d/vol7d_distinct.F90

# c'e` una maniera migliore?
tmpvar = $(libsim_volgrid6d_la_SOURCES:.f90=.mod)
nodist_include_HEADERS = $(tmpvar:.F90=.mod)


if GRIBAPI
libsim_volgrid6d_la_SOURCES = grid_class.F90 rotated_ll_class.f90 regular_ll_class.f90 volgrid6d_var_class.F90 volgrid6d_class.F90 gridinfo_class.f90
libsim_volgrid6d_la_LIBADD += $(GRIBAPI_LIBS)
endif



if GRIBAPI
grid_class.o:  regular_ll_class.$(OBJEXT) rotated_ll_class.$(OBJEXT)
grid_class.lo:  regular_ll_class.$(OBJEXT) rotated_ll_class.$(OBJEXT)
rotated_ll.o:  regular_ll_class.$(OBJEXT)
rotated_ll.lo:  regular_ll_class.$(OBJEXT)
volgrid6d_var_class.o:  ../vol7d/vol7d_distinct.F90
volgrid6d_var_class.lo:  ../vol7d/vol7d_distinct.F90
volgrid6d_class.o:  volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT) gridinfo_class.$(OBJEXT)
volgrid6d_class.lo:  volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT) gridinfo_class.$(OBJEXT)
gridinfo_class.o:  volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT)
gridinfo_class.lo:  volgrid6d_var_class.$(OBJEXT) grid_class.$(OBJEXT)
endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
