if NCARG
lib_LTLIBRARIES = libsim_graphics.la
libsim_graphics_la_SOURCES = 

AM_FCFLAGS = -I../base -I../vol7d -I../termolib -I../log4fortran
AM_LDFLAGS =

AM_LDFLAGS += $(NCARG_LDFLAGS)
libsim_graphics_la_SOURCES += ncar_plot_class.F90
libsim_graphics_la_LIBADD = $(NCARG_LIBS)


nodist_include_HEADERS = ncar_plot_class.mod

endif


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
