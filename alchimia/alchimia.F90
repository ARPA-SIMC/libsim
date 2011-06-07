module alchimia
USE missing_values
!USE vol7d_utilities
USE optional_values
!USE err_handling
!USE char_utilities

IMPLICIT NONE

integer, parameter :: npar=10,nmaxfunc=100,nmaxb=100

abstract interface
  integer function elabora(bbin,bbout,iin,oout)
  import 
  CHARACTER(len=10),intent(in) :: bbin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bbout(npar) !<  table B  WMO
  real, intent(in) :: iin(npar)
  real, intent(out) :: oout(npar)
  end function elabora
end interface


type fnds
   CHARACTER(len=10) :: name
   CHARACTER(len=10) :: bin(npar) !< table B  WMO
   CHARACTER(len=10) :: bout(npar) !<  table B  WMO
   integer :: priority
   integer :: order
   procedure (elabora) ,nopass, pointer :: fn
end type fnds



interface c_e
   module procedure c_e_fn
end interface

interface OPERATOR (==)
   module procedure equal_fn
end interface

interface init
   module procedure fn_init, vfn_init
end interface

interface display
   module procedure fn_display
end interface


!!$#define ARRAYOF_ORIGTYPE TYPE(fnds)
!!$#define ARRAYOF_TYPE arrayof_fnds
!!$#define ARRAYOF_ORIGEQ 0
!!$#include "arrayof_pre.F90"
!!$! from arrayof
!!$PUBLIC insert, append, remove, packarray
!!$PUBLIC insert_unique, append_unique

contains


subroutine fn_init(fn,name,bin,bout,priority,order,func)
type(fnds),intent(inout) :: fn
CHARACTER(len=*),optional :: name
CHARACTER(len=*),optional :: bin(:) !< table B  WMO
CHARACTER(len=*),optional :: bout(:) !<  table B  WMO
integer,optional :: priority,order
procedure (elabora),optional :: func


call optio(name,fn%name)

fn%bin=cmiss
if (present(bin)) fn%bin(:size(bin))=bin

fn%bout=cmiss
if (present(bout)) fn%bout(:size(bout))=bout

call optio(priority,fn%priority)
call optio(order,fn%order)

if (present(func)) then
  fn%fn => func
else
  fn%fn =>  NULL()
end if


end subroutine fn_init

subroutine vfn_init(vfn)
type(fnds),intent(inout) :: vfn(:)
integer :: i

do i = 1, size(vfn)
   call fn_init(vfn(i))
end do

end subroutine vfn_init

subroutine fnregister(vfn,fn,order)

type(fnds),intent(inout) :: vfn(:)
type(fnds),intent(in) :: fn
integer,optional :: order

integer :: ind

ind=firsttrue(vfn == fn)

if (ind /= 0) return

ind=firsttrue(.not. c_e(vfn))

if (ind > 0) then
   vfn(ind)=fn
   if (present(order)) vfn(ind)%order = order
else
!!   call raise_error("non ho piu posto "//t2c(size(vfn)))
  print *,"errore"
  call exit(1)
end if

end subroutine fnregister


elemental logical function c_e_fn(fn)
type(fnds),intent(in) :: fn

c_e_fn= c_e(fn%name)

end function c_e_fn

elemental logical function equal_fn(this,that)
type(fnds),intent(in) :: this,that

equal_fn= this%name == that%name 

end function equal_fn


subroutine fn_display(fn)
type(fnds),intent(in) :: fn

print *,fn%name," : ",fn%bin(:count(c_e(fn%bin)))
print *,"ottieni  : ",fn%bout(:count(c_e(fn%bout)))
print *,""

end subroutine fn_display


recursive logical function oracle(mybin,mybout,vfn,mayvfn,recurse) result(stat)
type(fnds),intent(in) :: vfn(:)
character(len=*),intent(in) :: mybin(:),mybout(:)
type(fnds),intent(out) :: mayvfn(:)

type(fnds),save :: usefullfn(nmaxfunc),maybefn(nmaxfunc)

!!$type(arrayof_fnds) :: tmp
!!$tmp = arrayof_fnds_new()
!!$append(tmp,myfn(1))
!!$CALL packarray(tmp)
!!$print *,tmp%array

integer :: i,j,k,iin,iout
logical :: allfoundout, foundout, somefoundin, foundin
logical,optional :: recurse
integer,save :: order,num
character(len=10) :: newbin(nmaxb), newbout(nmaxb), tmpbin(nmaxb)


! init only on the main call
if (.not. optio_log(recurse)) then
   call init(maybefn)
   call init(usefullfn)
   order=0
end if

newbin=cmiss
newbin(:size(mybin))=mybin
newbout=cmiss
newbout(:size(mybin))=mybin

! order is level to put functions
order=order+1
somefoundin = .false.
num=count(c_e(maybefn))
tmpbin=cmiss

!search for functions starting from input
do i =1, count(c_e(vfn))
   foundin = .true.
   do J = 1, count(c_e(vfn(i)%bin(:)))
      if (.not. any(vfn(i)%bin(j) == newbin)) foundin = .false.
!!$      print *,"confronto: ",vfn(i)%bin(j)
!!$      print *,"con: ",mybin
   end do
   if (foundin) then
      call fnregister(maybefn,vfn(i),order)
      do k=1,size(vfn(i)%bout)
         tmpbin(firsttrue(.not. c_e(tmpbin)))=vfn(i)%bout(k)
         newbout(firsttrue(.not. c_e(newbout)))=vfn(i)%bout(k)
      end do
      somefoundin = .true.
   end if
end do

do i = 1, count(c_e(tmpbin))
   newbin(firsttrue(.not. c_e(newbin)))=tmpbin(i)
end do

! here bin and bout are bigger (newbin, newbout)
! by the output of applicable functions


!check if we can work anymore
stat = .false.
if (.not. somefoundin) return
if (num == count(c_e(maybefn))) return

!check if we have finish
allfoundout = .true.
do i=1, count(c_e(mybout))
   foundout = .false.
   do j =1, count(c_e(newbout))
      if (newbout(j) == mybout(i)) foundout = .true.
   end do
   if (.not. foundout) allfoundout = .false.
end do


! ok, all is done
if (allfoundout) then

!!$  print *, "intermediate"
!!$  do i =1,size(maybefn)
!!$    if (c_e(maybefn(i)))  print *,maybefn(i)
!!$  end do

   ! toglie i rami secchi
   newbout=cmiss
   newbout(:size(mybout))=mybout
   tmpbin=cmiss

   do i = count(c_e(maybefn)),1,-1
     if (maybefn(i)%order /= order) then
       !print *,"change order",maybefn(i)%order
       order=maybefn(i)%order
       iin=count(c_e(tmpbin))
       iout=count(c_e(newbout))
       newbout(iout+1:iout+iin)=tmpbin(:iin)
       tmpbin=cmiss
     end if

     !print *,"cerco:",newbout(:firsttrue(.not. c_e(newbout)))

     foundout = .false.
     do j=1, count(c_e(newbout))
       if (any(maybefn(i)%bout(:) == newbout(j))) foundout = .true.
     end do
     if (foundout) then
       call fnregister(mayvfn,maybefn(i),order)
       do k=1,count(c_e(maybefn(i)%bin))
         tmpbin(firsttrue(.not. c_e(tmpbin)))=maybefn(i)%bin(k)
       end do
     end if
   end do

   stat = .true.

else

   stat=oracle(newbin,mybout,vfn,mayvfn,.true.)

end if

end function oracle


subroutine make(mayvfn,mybin,mybout,myin,myout)
type(fnds),intent(inout) :: mayvfn(:)
character(len=*),intent(in) :: mybin(:),mybout(:)
real,intent(in) :: myin(:)
real,intent(out) :: myout(:)
integer :: status,i

do i=size(mayvfn),1,-1

  status= mayvfn(i)%fn(mayvfn(i)%bin,mayvfn(i)%bout,myin,myout)
  !if (status /= 0 ) call raise_error("error on function: "//trim(mayvfn(i)%name))
  if (status /= 0 ) print *,"error on function: "//trim(mayvfn(i)%name)

end do

end subroutine make

!!$#include "arrayof_post.F90"

end module alchimia


