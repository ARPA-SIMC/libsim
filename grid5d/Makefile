# Makefile created by mkmf $Id: mkmf,v 14.0 2007/03/20 22:13:27 fms Exp $ 


FC = gfortran
LD = gfortran
FFLAGS=-I/usr/include -g -fbounds-check 
LDFLAGS=-llog4fortran -lsim_base



.DEFAULT:
	-touch $@
all: demo
demo.o: ./demo.f90 grid.o grid6d_var_class.o gridinfo_class.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./demo.f90
grid.o: ./grid.f90 regular_ll.o rotated_ll.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./grid.f90
regular_ll.o: ./regular_ll.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./regular_ll.f90
rotated_ll.o: ./rotated_ll.f90 regular_ll.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./rotated_ll.f90
grid6d_var_class.o: ./grid6d_var_class.F90 ../vol7d/vol7d_distinct.F90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./grid6d_var_class.F90
gridinfo_class.o: ./gridinfo_class.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./gridinfo_class.f90

SRC = ./demo.f90 ./grid.f90 ./rotated_ll.f90 ./regular_ll.f90 ./grid6d_var_class.F90
OBJ = demo.o grid.o rotated_ll.o regular_ll.o grid6d_var_class.o
clean: neat
	-rm -f .cppdefs $(OBJ) demo
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
demo: $(OBJ) 
	$(LD) $(OBJ) -o demo  $(LDFLAGS)
