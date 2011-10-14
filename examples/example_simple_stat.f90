
use simple_stat

IMPLICIT NONE

real, DIMENSION(100000) :: rnum 
integer,dimension(:),allocatable :: seed
integer :: k

integer :: i
real :: perc_vals(21)=(/(5.*i,i=0,20)/)
REAL, DIMENSION(:),allocatable ::  ndi,limbins


! create replicatable random numbers
call random_seed(size=k)
allocate (seed(k))
seed=5
call random_seed(put=seed)
call random_number(rnum)

allocate (ndi(size(perc_vals)-1),limbins(size(perc_vals)))

print *,"with 100.000 rundom number"
call NormalizedDensityIndex (rnum, perc_vals, ndi, limbins) 
print *,"ndi:    ",ndi
print *,"limits: ",limbins

print *,"with 5.000 number to 0.7"
rnum(:5000) = 0.7
call NormalizedDensityIndex (rnum, perc_vals, ndi, limbins) 
print *,"ndi:    ",ndi
print *,"limits: ",limbins

print *,"with 10.000 number to 0.7"
rnum(:10000) = 0.7
call NormalizedDensityIndex (rnum, perc_vals, ndi, limbins) 
print *,"ndi:    ",ndi
print *,"limits: ",limbins

print *,"with 20.000 number to 0.7"
rnum(:20000) = 0.7
call NormalizedDensityIndex (rnum, perc_vals, ndi, limbins) 
print *,"ndi:    ",ndi
print *,"limits: ",limbins


END program
