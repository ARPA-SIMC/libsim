! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "config.h"

!> \defgroup alchimia Libsim package, alchimia library.
!> This module defines objects and methods for generate derivate variables.
!!\ingroup alchimia
module alchimia

USE missing_values
use array_utilities
USE optional_values

IMPLICIT NONE

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
   CHARACTER(len=10) :: name=cmiss
   CHARACTER(len=10),allocatable :: bin(:) !< table B  WMO
   CHARACTER(len=10),allocatable :: bout(:) !<  table B  WMO
   integer :: priority
   integer :: order
   procedure (elabora) ,nopass, pointer :: fn
end type fnds

!> Vector of function to transform the input to alchimia module
type fndsv
  integer :: nout = imiss
  type(fnds),allocatable :: fnds(:)
end type fndsv

!>Check missing values for fnds 
interface c_e
   module procedure c_e_fn
end interface

interface OPERATOR (==)
   module procedure equal_fn
end interface

interface init
   module procedure fn_init
end interface

!> show on the screen the fnds and fndsv structure
interface display
   module procedure fn_display,fnv_display
end interface

!> Delete fndsv
interface delete
   module procedure fnv_delete
end interface

!> Do the real work to transform the input data to the output
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
private
public fnds,fndsv,make,init,c_e,display,delete,fnregister,oracle

contains

!> Inizialize a function object
subroutine fn_init(fn,name,bin,bout,priority,order,func)
type(fnds),intent(inout) :: fn       !< function object to create
CHARACTER(len=*),optional :: name    !< description of function
CHARACTER(len=*),optional :: bin(:)  !< standard table B descriptor for input parameters
CHARACTER(len=*),optional :: bout(:) !< standard table B descriptor for output parameters
integer,optional :: priority         !< relative priority for choise from functions with same output
integer,optional :: order            !< order to execute functions
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

end subroutine fn_init


!> Delete a vector of function object reinizializing it
subroutine fnv_delete(fnv)
type(fndsv),intent(inout) :: fnv
type(fndsv) :: fn

fnv=fn

end subroutine fnv_delete

!> Register a function object in the vector function object.
!! If called without argoments allocate vectors to (0)
!! if order is present force the order of added function
subroutine fnregister(vfn,fn,order)

type(fndsv),intent(inout) :: vfn     !< vector function object to ampliate
type(fnds),intent(in),optional :: fn !< function object to add
integer,optional :: order            !< order to execute the new added function object

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

!> Check missing function objects
elemental logical function c_e_fn(fn)
type(fnds),intent(in) :: fn !< function object to check

c_e_fn= c_e(fn%name)

end function c_e_fn

elemental logical function equal_fn(this,that)
type(fnds),intent(in) :: this,that

equal_fn= this%name == that%name 

end function equal_fn

!> Display function objects on screen
subroutine fn_display(fn)
type(fnds),intent(in) :: fn !< function object to display

print *,fn%name," : ",fn%bin(:count(c_e(fn%bin)))
print *,"get      : ",fn%bout(:count(c_e(fn%bout)))
print *,""

end subroutine fn_display

!> Display vector function objects on screen
subroutine fnv_display(fnv)
type(fndsv),intent(in) :: fnv !< vector function object to display
integer :: i

print *, "Here we have the solution:"
do i = count(c_e(fnv%fnds)),1,-1
  call display(fnv%fnds(i))
end do
end subroutine fnv_display

!> This function like a oracle say you how to abtain what you want.
!! Startin fron desciption of input and output and a vector of available functions
!! provide you the road to execute for make the output
recursive logical function oracle(mybin,mybout,vfn,mayvfn,recurse) result(stat)
type(fndsv),intent(in) :: vfn !< vector function object available
character(len=*),intent(in) :: mybin(:)  !< standard table B description of input
character(len=*),intent(in) :: mybout(:) !< standard table B description of output
type(fndsv),intent(out) :: mayvfn        !< vector function object that solve the problem

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

! delete on exit only on the main call
if (.not. optio_log(recurse)) then
  call delete(maybefn)
  call delete(usefullfn)
  order=0
end if

end function oracle

!> Execute the function to obtain what you have requested to oracle.
!! This is a sample only routine for the cousine test case.
subroutine makev(mayvfn,mybin,mybout,myin,myout)
type(fndsv),intent(inout) :: mayvfn       !< vector function object that solve the problem
character(len=*),intent(in) :: mybin(:)   !< standard table B description of input
character(len=*),intent(in) :: mybout(:)  !< standard table B description of output
real,intent(in) :: myin(:,:)  !< data input (ndata,nparameters)
real,intent(out) :: myout(:,:)!< data output (ndata,nparameters)
integer :: i

do i=size(mayvfn%fnds),1,-1
  if (c_e(mayvfn%fnds(i))) then
    call mayvfn%fnds(i)%fn(mybin,mybout,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout,myin,myout)
    !print *,"make",i,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout
  end if
end do

!!$#include "arrayof_post.F90"

end subroutine makev


end module alchimia

!>\example example_alchimia.f03
!!\brief Sample program to demostrate the alchimia module.
!!
!! This module use cuisine problems to demostrate the capacity of the module.

!>\example example_alchimiavg6d.f03
!!\brief  Sample program to demostrate the alchimia module with volgrid6d.

!>\example  example_alchimiav7d.f03
!! \brief Sample program to demostrate the alchimia module with vol7d.