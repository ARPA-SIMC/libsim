bin_PROGRAMS = esempio_v7d esempio_v7doracle esempio_v7doracle_multi

LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la

LDADD += -L/usr/local/oracle/10.1.0/db_1/lib  -lclntsh -lrouMW -lsupport -lrouGW -lix -lemos 
AM_FCFLAGS = -I../base -I../vol7d

esempio_v7d_SOURCES = esempio_v7d.f90

esempio_v7doracle_SOURCES = esempio_v7doracle.f90

esempio_v7doracle_multi_SOURCES = esempio_v7doracle_multi.f90
