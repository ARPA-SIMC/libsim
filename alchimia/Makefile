#LDLIBS=-lsim_vol7d
LDLIBS=

FFLAGS_DEBUG= -g -O0 -fbounds-check
#FFLAGS_DEBUG= -g -O0
FFLAGS= -I/usr/lib/gfortran/modules -I/usr/include -I/usr/include/dballe $(FFLAGS_DEBUG)
#FFLAGS=  $(FFLAGS_DEBUG)


#FC=/opt/gcc-4.6-20101016/bin/gfortran
#FC=/opt/gcc-4.6-20110129/bin/gfortran

FC=gfortran
#FC=pgfortran

PROGRAMS=main


OBJECTS=kinds.o missing_values.o optional_values.o alchimia.o pentolone.o forno.o 
#OBJECTS=alchimia.o pentolone.o  forno.o 


all  : $(PROGRAMS) 

$(PROGRAMS)  : $(OBJECTS)

pentolone.o forno.o : alchimia.o

clean :
	$(RM) $(PROGRAMS) $(addsuffix .o,$(PROGRAMS))  $(OBJECTS)


%.o: %.f90
	export LD_LIBRARY_PATH=/opt/gcc-4.6-20101016/lib/
	$(FC) -c $(FFLAGS) $<

%.o: %.F90
	export LD_LIBRARY_PATH=/opt/gcc-4.6-20101016/lib/
	$(FC) -c $(FFLAGS) $<

%: %.o
	export LD_LIBRARY_PATH=/opt/gcc-4.6-20101016/lib/
	$(FC)  -o $@ $< $(LDFLAGS) $(OBJECTS) $(LDLIBS)

