
if ALCHIMIA

lib_LTLIBRARIES = libsim_alchimia.la

include_HEADERS =
AM_CPPFLAGS =

libsim_alchimia_la_SOURCES = alchimia.F03 pentolone.F03 forno.F03


# Opzioni Fortran (-I... sono da considerarsi opzioni fortran
# e non del preprocessore perche' servono per USE e non per #include)
AM_FCFLAGS = -I../log4fortran -I../base -I../vol7d -I../volgrid6d
# Opzioni linker
AM_LDFLAGS =
libsim_alchimia_la_LDFLAGS =
# Librerie aggiuntive
libsim_alchimia_la_LIBADD = ../log4fortran/liblog4fortran.la \
 ../base/libsim_base.la ../vol7d/libsim_vol7d.la ../volgrid6d/libsim_volgrid6d.la


# c'e` una maniera migliore?
tmpvar = $(libsim_alchimia_la_SOURCES:.f90=.mod)
tmpvar1 = $(tmpvar:.f03=.mod)
tmpvar2 = $(tmpvar1:.F03=.mod)
tmpvar3 = $(tmpvar2:vdf4f_c.cpp=)
nodist_include_HEADERS = $(tmpvar3:.F90=.mod)


if VERSIONINFO
libsim_alchimia_la_LDFLAGS += -version-info @LIBSIM_VERSION_INFO@
endif

endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod


