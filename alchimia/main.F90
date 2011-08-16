program cucina

USE forno
USE pentolone

IMPLICIT NONE
integer :: i
integer, parameter :: ndat=100
type(fndsv) :: vfn,myvfn
character(len=10), allocatable:: mybin(:),mybout(:)
real,allocatable :: myin(:,:),myout(:,:)

call register_pentolone(vfn)
call register_forno(vfn)

mybin  = [character(len=10)::"acqua","olio","patate","sale","pollo","mais","gommosa"]
mybout = [character (len=10) :: "pole.pata.","lesso"]

print *,"Ho a disposizione:  ",mybin
print *,"Devo preparare:     ",mybout

if (.not. oracle(mybin,mybout,vfn,myvfn)) then
  print*, " non riesco a fare ",mybout
  stop 3
end if

call display(myvfn)
print *,"mi occorrono ",myvfn%nout," variabili in piu"


allocate(myin(ndat,size(mybin)))
myin=1.5
allocate(myout(ndat,myvfn%nout))
myout=rmiss
call make(myvfn,mybin,mybout,myin,myout)

end program cucina
