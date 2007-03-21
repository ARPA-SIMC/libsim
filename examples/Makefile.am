bin_PROGRAMS = esempio_v7d esempio_v7doracle

LDADD = ../base/libsim_base.la ../vol7d/libsim_vol7d.la
#LDADD += -L/usr/local/oracle/10.1.0/db_1/lib  -lclntsh /usr/lib/librouMW.a -L/usr/local/pgi/linux86/6.0/lib -L/usr/lib -L/usr/lib/gcc/i386-redhat-linux/4.0.2 -lpgf90 -lpgf90_rpm1 -lpgf902 -lpgf90rtl -lpgftnrtl -lnspgc -lpgc -lm
#LDADD += -L/usr/local/oracle/10.1.0/db_1/lib  -lclntsh -lrouMW -lsupport -lrouGW -lix -lemos -L/usr/local/pgi/linux86/6.0/lib -L/usr/lib -lpgf90 -lpgf90_rpm1 -lpgf902 -lpgf90rtl -lpgftnrtl -lnspgc -lpgc -lm
LDADD += -L/usr/local/oracle/10.1.0/db_1/lib  -lclntsh -lrouMW -lsupport -lrouGW -lix -lemos 
AM_FCFLAGS = -I../base -I../vol7d

esempio_v7d_SOURCES = esempio_v7d.f90

esempio_v7doracle_SOURCES = esempio_v7doracle.f90
