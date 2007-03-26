bin_PROGRAMS = esempio_v7d

if ORSIM
bin_PROGRAMS += esempio_v7doracle esempio_v7doracle_multi
endif

LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la

AM_FCFLAGS = -I../base -I../vol7d

esempio_v7d_SOURCES = esempio_v7d.f90
esempio_v7d_LDADD = $(LDADD) $(liboraclesim)

if ORSIM
esempio_v7doracle_SOURCES = esempio_v7doracle.f90
esempio_v7doracle_LDADD = $(LDADD) $(liboraclesim)

esempio_v7doracle_multi_SOURCES = esempio_v7doracle_multi.f90
esempio_v7doracle_multi_LDADD = $(LDADD) $(liboraclesim)
endif
