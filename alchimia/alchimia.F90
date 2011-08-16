module alchimia
USE missing_values
!USE vol7d_utilities
USE optional_values
!USE err_handling
!USE char_utilities

IMPLICIT NONE

integer, parameter :: nmaxb=100

abstract interface
  subroutine elabora(bbin,bbout,iin,oout)
  import 
  CHARACTER(len=10),intent(in) :: bbin(:) !< table B  WMO
  CHARACTER(len=10),intent(in) :: bbout(:) !<  table B  WMO
  real, intent(in) :: iin(:,:)
  real, intent(out) :: oout(:,:)
  end subroutine elabora
end interface

type fnds
   CHARACTER(len=10) :: name=cmiss
   CHARACTER(len=10),allocatable :: bin(:) !< table B  WMO
   CHARACTER(len=10),allocatable :: bout(:) !<  table B  WMO
   integer :: priority
   integer :: order
   procedure (elabora) ,nopass, pointer :: fn
end type fnds

type fndsv
  integer :: nout = imiss
  type(fnds),allocatable :: fnds(:)
end type fndsv

interface c_e
   module procedure c_e_fn
end interface

interface OPERATOR (==)
   module procedure equal_fn
end interface

interface init
   module procedure fn_init
end interface

interface display
   module procedure fn_display,fnv_display
end interface

interface delete
   module procedure fnv_delete
end interface

interface make
   module procedure makev
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

if (present(bin)) then
  fn%bin=bin
else
  allocate(fn%bin(1))
  fn%bin=cmiss
end if

if (present(bout)) then
  fn%bout=bout
else
  allocate(fn%bout(1))
  fn%bout=cmiss
end if

call optio(priority,fn%priority)
call optio(order,fn%order)

if (present(func)) then
  fn%fn => func
else
  fn%fn =>  NULL()
end if

end subroutine fn_init



subroutine fnv_delete(fnv)
type(fndsv),intent(inout) :: fnv
type(fndsv) :: fn

fnv=fn

end subroutine fnv_delete


subroutine fnregister(vfn,fn,order)

type(fndsv),intent(inout) :: vfn
type(fnds),intent(in) :: fn
integer,optional :: order

integer :: nfn
type(fndsv) :: vfntmp

if (.not. allocated(vfn%fnds))then
  allocate(vfn%fnds(0))
  vfn%nout=0
end if

if (firsttrue(vfn%fnds == fn) /= 0) return
nfn=size(vfn%fnds)

allocate(vfntmp%fnds(nfn+1))

vfntmp%fnds(:nfn)=vfn%fnds

call move_alloc(from=vfntmp%fnds ,to=vfn%fnds)

vfn%fnds(nfn+1)=fn
if (present(order)) vfn%fnds(nfn+1)%order = order

vfn%nout=vfn%nout+size(fn%bout)

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

subroutine fnv_display(fnv)
type(fndsv),intent(in) :: fnv
integer :: i

print *, "Ecco la ricetta:"
do i = count(c_e(fnv%fnds)),1,-1
  call display(fnv%fnds(i))
end do
end subroutine fnv_display

recursive logical function oracle(mybin,mybout,vfn,mayvfn,recurse) result(stat)
type(fndsv),intent(in) :: vfn
character(len=*),intent(in) :: mybin(:),mybout(:)
type(fndsv),intent(out) :: mayvfn

type(fndsv),save :: usefullfn,maybefn

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
  print *,"cancello"
  call delete(maybefn)
  call delete(usefullfn)
  order=0
end if

print *,"oracle",order

newbin=cmiss
newbin(:size(mybin))=mybin
newbout=cmiss
newbout(:size(mybin))=mybin

! order is level to put functions
order=order+1
somefoundin = .false.
num=count(c_e(maybefn%fnds))
tmpbin=cmiss

!search for functions starting from input
do i =1, count(c_e(vfn%fnds))
   foundin = .true.
   do J = 1, count(c_e(vfn%fnds(i)%bin(:)))
      if (.not. any(vfn%fnds(i)%bin(j) == newbin)) foundin = .false.
!!$      print *,"confronto: ",vfn(i)%bin(j)
!!$      print *,"con: ",mybin
   end do
   if (foundin) then
     !print *,"registro  ",vfn%fnds(i)%name
     call fnregister(maybefn,vfn%fnds(i),order)
     do k=1,size(vfn%fnds(i)%bout)
       tmpbin(firsttrue(.not. c_e(tmpbin)))=vfn%fnds(i)%bout(k)
       newbout(firsttrue(.not. c_e(newbout)))=vfn%fnds(i)%bout(k)
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
if (num == count(c_e(maybefn%fnds))) return

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

   do i = count(c_e(maybefn%fnds)),1,-1
     if (maybefn%fnds(i)%order /= order) then
       !print *,"change order",maybefn(i)%order
       order=maybefn%fnds(i)%order
       iin=count(c_e(tmpbin))
       iout=count(c_e(newbout))
       newbout(iout+1:iout+iin)=tmpbin(:iin)
       tmpbin=cmiss
     end if

     !print *,"cerco:",newbout(:firsttrue(.not. c_e(newbout)))

     foundout = .false.
     do j=1, count(c_e(newbout))
       if (any(maybefn%fnds(i)%bout(:) == newbout(j))) foundout = .true.
     end do
     if (foundout) then
       !print *,"altroregistro  ",maybefn%fnds(i)%name
       call fnregister(mayvfn,maybefn%fnds(i),order)
       do k=1,count(c_e(maybefn%fnds(i)%bin))
         tmpbin(firsttrue(.not. c_e(tmpbin)))=maybefn%fnds(i)%bin(k)
       end do
     end if
   end do

   stat = .true.

else

   stat=oracle(newbin,mybout,vfn,mayvfn,.true.)

end if


! init only on the main call
if (.not. optio_log(recurse)) then
  call delete(maybefn)
  call delete(usefullfn)
  order=0
end if


end function oracle


subroutine makev(mayvfn,mybin,mybout,myin,myout)
type(fndsv),intent(inout) :: mayvfn
character(len=*),intent(in) :: mybin(:),mybout(:)
real,intent(in) :: myin(:,:)
real,intent(out) :: myout(:,:)
integer :: i

do i=size(mayvfn%fnds),1,-1
  if (c_e(mayvfn%fnds(i))) then
    call mayvfn%fnds(i)%fn(mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout,myin,myout)
    print *,"make",i,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout
  end if
end do

end subroutine makev





!!$#include "arrayof_post.F90"

end module alchimia


