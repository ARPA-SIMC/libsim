module alchimia
USE missing_values
use array_utilities
!USE volgrid6d_class
USE optional_values

IMPLICIT NONE

private
public fnds, fndsv

integer, parameter :: nmaxb=100

abstract interface
  subroutine elabora(mybin,mybout,bin,bout,in,out)
  import 
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)
  end subroutine elabora
end interface

type fnds
  CHARACTER(len=10) :: name=cmiss !< description of function
  CHARACTER(len=10),allocatable :: bin(:) !< standard table B descriptor for input parameters
  CHARACTER(len=10),allocatable :: bout(:) !< standard table B descriptor for output parameters
  integer :: priority !< relative priority for choise from functions with same output
  integer :: order !< order to execute functions
  procedure(elabora),nopass,pointer :: fn !< function with the abstract interface

  contains

  procedure :: c_e => c_e_fn
  generic :: operator(==) => equal_fn
  procedure :: init => init_fn
  procedure :: display => display_fn
end type fnds

type fndsv
  integer :: nout = imiss
  type(fnds),allocatable :: fnds(:)
end type fndsv


interface display
   module procedure fnv_display
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


subroutine init_fn(fn,name,bin,bout,priority,order,func)
CLASS(fnds),intent(inout) :: fn !< function object to create
CHARACTER(len=*),optional :: name !< description of function
CHARACTER(len=*),optional :: bin(:) !< standard table B descriptor for input parameters
CHARACTER(len=*),optional :: bout(:) !< standard table B descriptor for output parameters
integer,optional :: priority !< relative priority for choise from functions with same output
integer,optional :: order !< order to execute functions
procedure (elabora),optional :: func !< function with the abstract interface

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

end subroutine init_fn



subroutine fnv_delete(fnv)
type(fndsv),intent(inout) :: fnv
type(fndsv) :: fn

fnv=fn

end subroutine fnv_delete


subroutine fnregister(vfn,fn,order)

type(fndsv),intent(inout) :: vfn
type(fnds),intent(in),optional :: fn
integer,optional :: order

integer :: nfn
type(fndsv) :: vfntmp

if (.not. allocated(vfn%fnds))then
  allocate(vfn%fnds(0))
  vfn%nout=0
end if

if (present(fn))then

  if (firsttrue(vfn%fnds == fn) /= 0) return
  nfn=size(vfn%fnds)

  allocate(vfntmp%fnds(nfn+1))

  vfntmp%fnds(:nfn)=vfn%fnds
  
  call move_alloc(from=vfntmp%fnds ,to=vfn%fnds)
  
  vfn%fnds(nfn+1)=fn
  if (present(order)) vfn%fnds(nfn+1)%order = order
  
  vfn%nout=vfn%nout+size(fn%bout)

end if

end subroutine fnregister


elemental logical function c_e_fn(fn)
class(fnds),intent(in) :: fn

c_e_fn= c_e(fn%name)

end function c_e_fn

elemental logical function equal_fn(this,that)
class(fnds),intent(in) :: this,that

equal_fn= this%name == that%name 

end function equal_fn


subroutine display_fn(fn)
class(fnds),intent(in) :: fn

print *,fn%name," : ",fn%bin(:count(c_e(fn%bin)))
print *,"get      : ",fn%bout(:count(c_e(fn%bout)))
print *,""

end subroutine display_fn

subroutine fnv_display(fnv)
type(fndsv),intent(in) :: fnv
integer :: i

print *, "Here we have the solution:"
do i = count(fnv%fnds%c_e()),1,-1
  call fnv%fnds(i)%display()
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


! delte only on the main call
if (.not. optio_log(recurse)) then
  !print *,"cancello"
  call delete(maybefn)
  call delete(usefullfn)
  call fnregister(maybefn)
  call fnregister(usefullfn)
  order=0
end if

!print *,"oracle",order

newbin=cmiss
newbin(:size(mybin))=mybin
newbout=cmiss
newbout(:size(mybin))=mybin

! order is level to put functions
order=order+1
somefoundin = .false.
num=count(maybefn%fnds%c_e())
tmpbin=cmiss

!search for functions starting from input
do i =1, count(vfn%fnds%c_e())
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
if (num == count(maybefn%fnds%c_e())) return

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

   do i = count(maybefn%fnds%c_e()),1,-1
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

! delete on exit only on the main call
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
  if (mayvfn%fnds(i)%c_e()) then
    call mayvfn%fnds(i)%fn(mybin,mybout,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout,myin,myout)
    !print *,"make",i,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout
  end if
end do

end subroutine makev


!!$subroutine make_vg6d(mayvfn,mybin,mybout,vg6din,vg6dout)
!!$type(fndsv),intent(inout) :: mayvfn
!!$character(len=*),intent(in) :: mybin(:),mybout(:)
!!$type(volgrid6d),intent(in) :: vg6din
!!$type(volgrid6d),intent(out) :: vg6dout
!!$integer :: i,nx,ny,nlevel,ntime,ntimerange,nvar,nvarin,ilevel,itime,itimerange
!!$real,allocatable :: myin(:,:),myout(:,:)
!!$
!!$nx=size(vg6din%voldati,1)
!!$ny=size(vg6din%voldati,2)
!!$nlevel=size(vg6din%voldati,3)
!!$ntime=size(vg6din%voldati,4)
!!$ntimerange=size(vg6din%voldati,5)
!!$nvarin=size(mybin)
!!$nvar=size(mybout)
!!$
!!$allocate(myout(nx*ny,nvar))
!!$
!!$call init(vg6dout, vg6din%griddim, vg6din%time_definition, categoryappend="generated by alchimia make")
!!$call volgrid6d_alloc(vg6dout, vg6din%griddim%dim, ntime, nlevel, ntimerange, nvar)
!!$call volgrid6d_alloc_vol(vg6dout,inivol=.true.)
!!$
!!$vg6dout%time=vg6din%time
!!$vg6dout%timerange=vg6din%timerange
!!$vg6dout%level=vg6din%level
!!$
!!$do i=size(mayvfn%fnds),1,-1
!!$  if (mayvfn%fnds(i)%c_e()) then
!!$    do ilevel=1,nlevel
!!$      do itime=1,ntime
!!$        do itimerange=1,ntimerange
!!$          myin=reshape(vg6din%voldati(:,:,ilevel,itime,itimerange,:),(/nx*ny,nvarin/))
!!$          myout=rmiss
!!$          call mayvfn%fnds(i)%fn(mybin,mybout,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout,myin,myout)
!!$          vg6dout%voldati(:,:,ilevel,itime,itimerange,:)=reshape(myout,(/nx,ny,nvar/))
!!$        end do
!!$      end do
!!$    end do
!!$  end if
!!$end do
!!$
!!$end subroutine make_vg6d



!!$#include "arrayof_post.F90"

end module alchimia


