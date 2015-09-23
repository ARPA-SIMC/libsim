use simple_stat
use array_utilities

IMPLICIT NONE

real, DIMENSION(1000000) :: rnum 
integer,dimension(:),allocatable :: seed
integer :: k

integer :: i
real :: perc_vals(11)=(/(10.*i,i=0,10)/)
REAL, DIMENSION(:),allocatable ::  ndi,limbins


! create replicatable random numbers
call random_seed(size=k)
allocate (seed(k))
seed=5
call random_seed(put=seed)
call random_number(rnum)

do i=1,size(rnum),size(rnum)/10
  print *, rnum(i)
end do

print *, "start sort ",size(rnum)
call sort (rnum)
print *,"end sort"

do i=1,size(rnum),size(rnum)/10
  print *, rnum(i)
end do

stop 1

allocate (ndi(size(perc_vals)-1),limbins(size(perc_vals)))

print *,"with 100.000 rundom number"
call NormalizedDensityIndex (rnum, perc_vals, ndi, limbins) 
print *,"ndi:    ",ndi
print *,"limits: ",limbins

print *,"with 5.000 number to 0.4; 10.000 number to 0.5; 5.000 number to 0.6"
rnum(:5000) = 0.4
rnum(5001:10000) = 0.6
rnum(10001:20000) = 0.5
call NormalizedDensityIndex (rnum, perc_vals, ndi, limbins) 
print *,"ndi:    ",ndi
print *,"limits: ",limbins


print *,"with 20.000 number to 0.4; 40.000 number to 0.5; 20.000 number to 0.6"
rnum(:20000) = 0.4
rnum(20001:40000) = 0.6
rnum(40001:80000) = 0.5
call NormalizedDensityIndex (rnum, perc_vals, ndi, limbins) 
print *,"ndi:    ",ndi
print *,"limits: ",limbins


END program
