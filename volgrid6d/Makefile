# Makefile created by mkmf $Id: mkmf,v 14.0 2007/03/20 22:13:27 fms Exp $ 


FC = gfortran
LD = gfortran
FFLAGS=-I../log4fortran -I../base -I../vol7d -I/usr/include -g -fbounds-check 
LDFLAGS=  -L../base/.libs/ -L../vol7d/.libs/  -L../log4fortran/.libs/ -lsim_vol7d -lsim_base -llog4fortran -lgrib_api_f90 -lgrib_api -llog4c



.DEFAULT:
	-touch $@
all: demo demo2 demo3 demo4
demo.o: ./demo.f90 grid_class.o volgrid6d_var_class.o gridinfo_class.o volgrid6d_class.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./demo.f90
demo2.o: ./demo2.f90 grid_class.o volgrid6d_var_class.o gridinfo_class.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./demo2.f90
demo3.o: ./demo3.f90 grid_class.o volgrid6d_var_class.o gridinfo_class.o volgrid6d_class.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./demo3.f90
demo4.o: ./demo4.f90 grid_class.o gridinfo_class.o volgrid6d_class.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./demo4.f90
grid_class.o: ./grid_class.F90 regular_ll.o rotated_ll.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./grid_class.F90
regular_ll.o: ./regular_ll.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./regular_ll.f90
rotated_ll.o: ./rotated_ll.f90 regular_ll.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./rotated_ll.f90
volgrid6d_var_class.o: ./volgrid6d_var_class.F90 ../vol7d/vol7d_distinct.F90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./volgrid6d_var_class.F90
volgrid6d_class.o: ./volgrid6d_class.f90 
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./volgrid6d_class.f90
gridinfo_class.o: ./gridinfo_class.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./gridinfo_class.f90

SRC = demo.f90 demo2.f90 demo3.f90 demo4.f90 ./grid_class.F90 ./rotated_ll.f90 ./regular_ll.f90 ./volgrid6d_var_class.F90 ./volgrid6d_class.F90 gridinfo_class.f90
OBJ =  grid_class.o rotated_ll.o regular_ll.o volgrid6d_var_class.o volgrid6d_class.o gridinfo_class.o
clean: neat
	-rm -f .cppdefs $(OBJ) demo demo2 demo3 demo4
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
demo: demo.o $(OBJ) 
	$(LD) demo.o $(OBJ) -o demo  $(LDFLAGS)
demo2: demo2.o $(OBJ) 
	$(LD) demo2.o $(OBJ) -o demo2  $(LDFLAGS)
demo3: demo3.o $(OBJ) 
	$(LD) demo3.o $(OBJ) -o demo3  $(LDFLAGS)
demo4: demo4.o $(OBJ) 
	$(LD) demo4.o $(OBJ) -o demo4  $(LDFLAGS)
