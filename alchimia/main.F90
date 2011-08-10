program cucina

USE forno
USE pentolone

IMPLICIT NONE

integer, parameter :: nfunc=pentolone_nfunc+forno_nfunc
type(fnds) :: vfn(nfunc),myvfn(nmaxfunc)
integer :: i

character(len=10), allocatable:: mybin(:),mybout(:)
real :: myin(6)=0.,myout(1)=rmiss

call init(vfn)
call register_pentolone(vfn)
call register_forno(vfn)
call init(myvfn)

!!$print *, "in"
!!$do i =1,size(vfn)
!!$   if (c_e(vfn(i)))  print *,vfn(i)
!!$end do

allocate(mybin(6),mybout(1))
mybin  = [character(len=10)::"acqua","olio","patate","sale","pollo","mais"]
mybout = [character (len=10) :: "pole.pata."]
!!$mybin  = (/"acqua     ","olio      ","patate    ","sale      ","pollo     ","mais      "/)
!!$mybout = (/"pole.pata."/)

print *,"Ho a disposizione:  ",mybin
print *,"Devo preparare:     ",mybout


if (oracle(mybin,mybout,vfn,myvfn)) then

  print *, "Ecco la ricetta:"
  do i =size(myvfn),1,-1
    if (c_e(myvfn(i)))  call display(myvfn(i))
  end do

  call make(myvfn,mybin,mybout,myin,myout)

else

  print*, " non riesco a fare ",mybout

end if

end program cucina
