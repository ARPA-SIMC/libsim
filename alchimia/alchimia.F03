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

!> \defgroup alchimia_class Libsim package, alchimia library.
!! Generate new variabile using functions libraries

!> This module defines objects and methods for generate derivate variables.
!!\ingroup alchimia_class
module alchimia

USE missing_values
use array_utilities
USE optional_values
USE log4fortran
USE char_utilities

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
   CHARACTER(len=50) :: name=cmiss
   CHARACTER(len=10),allocatable :: bin(:) !< table B  WMO
   CHARACTER(len=10),allocatable :: bout(:) !<  table B  WMO
   integer :: priority
   integer :: order
   procedure (elabora) ,nopass, pointer :: fn
end type fnds

!> Vector of function to transform the input to alchimia module
type fndsv
  integer :: nin = imiss
  integer :: nout = imiss
  type(fnds),allocatable :: fnds(:)
end type fndsv

!> shoplist are list of variables
type shoplist
  CHARACTER(len=10),allocatable :: bvar(:) !< table B  WMO
end type shoplist

!> Vector of shoplists that are list of variables
type shoplists
  type(shoplist),allocatable :: shoplist(:)
end type shoplists

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
   module procedure fn_display, fnv_display, vfnv_display, fnv_display_byorder, sl_display
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
public fnds,fndsv,make,init,c_e,display,delete,fnregister,oracle,register_copy
public shoppinglist, shoplists, compile_sl

contains

!> Register the copy function to be able to copy input variables to output if requested
subroutine register_copy(vfn,bin)

  type(fndsv),intent(inout) :: vfn !< vector of function descriptors into register 
  CHARACTER(len=10),intent(in) :: bin(:)  !< vector table B  WMO input  in user's data
  integer :: i

  do i=1, size(bin)
    call fnregister(vfn,alchimia_copy_def(bin(i)))
  end do

end subroutine register_copy

subroutine  alchimia_copy(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1)))=in(:,index_c(mybin,bin(1)))

end subroutine alchimia_copy

type(fnds) function alchimia_copy_def(bvar)
  CHARACTER(len=10),intent(in) :: bvar

  call init(alchimia_copy_def,"copy"//bvar,&
       [character(len=10) :: bvar],&
       [character(len=10) :: bvar],0,func=alchimia_copy)
end function alchimia_copy_def

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
elemental subroutine fnv_delete(fnv)
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
  vfn%nin=0
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
  
  vfn%nin=vfn%nin+size(fn%bin)
  vfn%nout=vfn%nout+size(fn%bout)

  CALL l4f_log(L4F_DEBUG, 'fnregister: adding function object '//trim(fn%name)//' ; nout '//t2c(vfn%nout))

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


!> Display shopping lists on screen
subroutine sl_display(sl)
type(shoplists),intent(in) :: sl !< shopping lists to display

integer :: i

do i = 1, size(sl%shoplist)
  print *,"shopping list : ",i
    print *,"varlist       : ",sl%shoplist(i)%bvar
  print *,""
end do

end subroutine sl_display


!> Display function objects on screen
subroutine fn_display(fn)
type(fnds),intent(in) :: fn !< function object to display
if (c_e(fn%order) .and. c_e(fn%priority)) then
  print *,"function : ",fn%name,"  order :",fn%order,"  priority :",fn%priority
else if (c_e(fn%order)) then
  print *,"function : ",fn%name,"  order :",fn%order
else if (c_e(fn%priority)) then
  print *,"function : ",fn%name,"  priority :",fn%priority
else
  print *,"function : ",fn%name
end if
print *,"input    : ",fn%bin (:count(c_e(fn%bin)))
print *,"output   : ",fn%bout(:count(c_e(fn%bout)))
print *,""

end subroutine fn_display

!> Display vector function objects on screen
subroutine fnv_display(fnv)
type(fndsv),intent(in) :: fnv !< vector function object to display
integer :: i

if (.not. allocated(fnv%fnds))return

print *,"-------------------------------------------------"
print *, "Here the function tree:"
do i = count(c_e(fnv%fnds)),1,-1
  call display(fnv%fnds(i))
end do
print *,"-------------------------------------------------"
end subroutine fnv_display



!> Display vector function objects on screen selected by order
subroutine fnv_display_byorder(fnv,order)
type(fndsv),intent(in) :: fnv !< vector function object to display
integer,intent(in) :: order !< level for recursice search of functions
 
integer :: i

print *,"-------------------------------------------------"
print *, "Here the function tree for order: ",order
do i = count(c_e(fnv%fnds)),1,-1
  if (fnv%fnds(i)%order == order ) then
    call display(fnv%fnds(i))
  end if
end do
print *,"-------------------------------------------------"
end subroutine fnv_display_byorder



!> Display vector function objects on screen
subroutine vfnv_display(vfnv)
type(fndsv),intent(in) :: vfnv(:) !< vector function object to display
integer :: i

print *,">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
do i = 1, size(vfnv)
  print*,">> Function tree number:",i
  call display(vfnv(i))
end do
print *,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
end subroutine vfnv_display



!> This function like a oracle say you how to abtain what you want.
!! Starting from desciption of input and output and a vector of available functions
!! provide to you the road to execute for make the output
recursive logical function oracle(mybin,mybout,vfn,myvfn,recurse) result(stat)
type(fndsv),intent(in) :: vfn !< vector function object available
character(len=*),intent(in) :: mybin(:)  !< standard table B description of input
character(len=*),intent(in) :: mybout(:) !< standard table B description of output
type(fndsv),intent(out) :: myvfn        !< vector function object that solve the problem
logical,optional :: recurse !< set to .true. when called in recurse

type(fndsv),save :: usefullfn,maybefn

!!$type(arrayof_fnds) :: tmp
!!$tmp = arrayof_fnds_new()
!!$append(tmp,myfn(1))
!!$CALL packarray(tmp)
!!$print *,tmp%array

integer :: i,j,k,iin,iout
logical :: allfoundout, foundout, somefoundin, foundin
integer,save :: order,num
character(len=10) :: newbin(nmaxb), newbout(nmaxb), tmpbin(nmaxb)


! delete only on the main call
if (.not. optio_log(recurse)) then
  CALL l4f_log(L4F_DEBUG, "oracle: delete and register")
  call delete(maybefn)
  call delete(usefullfn)
  call delete(myvfn)
  call fnregister(maybefn)
  call fnregister(usefullfn)
  call fnregister(myvfn)
  order=0
end if

CALL l4f_log(L4F_DEBUG, "oracle: order "//t2c(order))
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
!!$      print *,"compare: ",vfn(i)%bin(j)
!!$      print *,"with: ",mybin
   end do
   if (foundin) then
     CALL l4f_log(L4F_DEBUG, "oracle: register  "//trim(vfn%fnds(i)%name))
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

   ! remove dry branch
   newbout=cmiss
   newbout(:size(mybout))=mybout
   tmpbin=cmiss

   do i = count(c_e(maybefn%fnds)),1,-1
     if (maybefn%fnds(i)%order /= order) then
       CALL l4f_log(L4F_DEBUG, "oracle: change order "//t2c(maybefn%fnds(i)%order))
       order=maybefn%fnds(i)%order
       iin=count(c_e(tmpbin))
       iout=count(c_e(newbout))
       newbout(iout+1:iout+iin)=tmpbin(:iin)
       tmpbin=cmiss
     end if

     !print *,"search:",newbout(:firsttrue(.not. c_e(newbout)))

     foundout = .false.
     do j=1, count(c_e(newbout))
       if (any(maybefn%fnds(i)%bout(:) == newbout(j))) foundout = .true.
     end do
     if (foundout) then
       CALL l4f_log(L4F_DEBUG, "oracle: other register  "// trim(maybefn%fnds(i)%name))
       call fnregister(myvfn,maybefn%fnds(i),order)
       do k=1,count(c_e(maybefn%fnds(i)%bin))
         tmpbin(firsttrue(.not. c_e(tmpbin)))=maybefn%fnds(i)%bin(k)
       end do
     end if
   end do

   stat = .true.

else

   stat=oracle(newbin,mybout,vfn,myvfn,.true.)

end if

! delete on exit only on the main call
if (.not. optio_log(recurse)) then
  call delete(maybefn)
  call delete(usefullfn)
  order=0
end if

end function oracle


!> This function try to suggest you some road to obtain the variable you want.
!! Starting from desciption of output and a vector of available functions
!! provide to you some possible starting points.
recursive logical function shoppinglist(mybout,vfn,myvfn, copy, recurse) result(stat)
type(fndsv),intent(in) :: vfn !< vector function object available
character(len=*),intent(in) :: mybout(:) !< standard table B description of output
type(fndsv),intent(inout) :: myvfn        !< vector function object that solve the problem
logical,intent(in),optional :: copy !< if .true. the copy functions are localy added to vfn (you can have input variable copyed to output)
logical,intent(in),optional :: recurse !< set to .true. when called in recurse

type(fndsv) :: vfntmp
integer :: i,j,k
logical ::  somefoundout
integer,save :: order
character(len=10) :: newbout(nmaxb)

stat = .true.
newbout=cmiss
vfntmp=vfn

! delete only on the main call
if (.not. optio_log(recurse)) then
  CALL l4f_log(L4F_DEBUG, "shoppinglist: main call (delete and register)")

  call delete(myvfn)
  call fnregister(myvfn)
  order=0
  newbout(:size(mybout))=mybout

  if (optio_log(copy)) call register_copy(vfntmp,mybout)

else

  CALL l4f_log(L4F_DEBUG, "shoppinglist: sub call; order:"//t2c(order))

  !print*,pack(newbout,c_e(newbout))

  do i=1, count(c_e(myvfn%fnds(:)))
    !print*,"order:",myvfn%fnds(i)%order, order
    if (myvfn%fnds(i)%order == order) then
      do k=1,size(myvfn%fnds(i)%bin(:))
        newbout(firsttrue(.not. c_e(newbout)))=myvfn%fnds(i)%bin(k)
      end do
    end if
  end do

end if

!print*,pack(newbout,c_e(newbout))

! order is level to put functions
order=order+1
somefoundout = .false.

CALL l4f_log(L4F_DEBUG, "shoppinglist: order "//t2c(order))

!search for functions outputing my output
do i =1, count(c_e(vfntmp%fnds))
  !call display(vfntmp%fnds(i)) 
  do j = 1, count(c_e(vfntmp%fnds(i)%bout(:)))
    if (any(vfntmp%fnds(i)%bout(j) == newbout)) then
      CALL l4f_log(L4F_DEBUG, "shoppinglist: register  "//trim(vfntmp%fnds(i)%name))
      call fnregister(myvfn,vfntmp%fnds(i),order)
      somefoundout = .true.
    end if
  end do
end do

!check if we can work anymore
if (.not. somefoundout) return

stat=shoppinglist(mybout,vfntmp,myvfn,copy=optio_log(copy), recurse=.true.)

! delete on exit only on the main call
if (.not. optio_log(recurse)) then
  call delete(vfntmp)
  order=0
end if

end function shoppinglist


!> Execute the function to obtain what you have requested to oracle.
!! This is a sample only routine for the cousine test case.
subroutine makev(mayvfn,mybin,mybout,myin,myout)
type(fndsv),intent(inout) :: mayvfn       !< vector function object that solve the problem
character(len=*),intent(in) :: mybin(:)   !< standard table B description of input
character(len=*),intent(in) :: mybout(:)  !< standard table B description of output
real,intent(in) :: myin(:,:)  !< data input (ndata,nparameters)
real,intent(out) :: myout(:,:)!< data output (ndata,nparameters)
integer :: i,j
character(len=10) :: newbout(mayvfn%nout)


newbout=cmiss
do i=1, size(mayvfn%fnds)
  if (c_e(mayvfn%fnds(i))) then
    do j=1, size(mayvfn%fnds(i)%bout)
      if (c_e(mayvfn%fnds(i)%bout(j))) then
        if (index_c(newbout,mayvfn%fnds(i)%bout(j)) <= 0) then
          newbout(index_c(newbout,cmiss)) = mayvfn%fnds(i)%bout(j)
        end if
      end if
    end do
  end if
end do

do i=size(mayvfn%fnds),1,-1
  if (c_e(mayvfn%fnds(i))) then
    print *,"name:",mayvfn%fnds(i)%name,"order:",mayvfn%fnds(i)%order

    call mayvfn%fnds(i)%fn(mybin,newbout,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout,myin,myout)
    !print *,"make",i,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout
  end if
end do

!!$#include "arrayof_post.F90"

end subroutine makev




!> Produce a vector of list of variables usefull for produce your request.
function  compile_sl(myvfn)        

type(shoplists) :: compile_sl
type(fndsv),intent(in) :: myvfn       !< vector function object that solve the problem

integer :: i,j,k,nshoplist,nvar,nfunc,indfunc,indvar
CHARACTER(len=10),allocatable :: bvartmp(:)

indfunc=0
nshoplist=(maxval(myvfn%fnds(:)%order))
nshoplist=max(0,nshoplist)
allocate (compile_sl%shoplist(nshoplist))

nvar=1

do i=1,nshoplist
  nfunc=count(myvfn%fnds(:)%order==i)
  allocate(compile_sl%shoplist(i)%bvar(nvar-1))
  if (i > 1) then
    compile_sl%shoplist(i)%bvar = compile_sl%shoplist(i-1)%bvar
    do j = indfunc+1, indfunc+nfunc
      do k = 1, size(myvfn%fnds(j)%bout)
        indvar=index_c(compile_sl%shoplist(i)%bvar,myvfn%fnds(j)%bout(k))
        if (indvar > 0) compile_sl%shoplist(i)%bvar(indvar)=cmiss
      end do
    end do
  end if
  do j = indfunc+1, indfunc+nfunc
    do k = 1, size(myvfn%fnds(j)%bin)
      if (index_c(compile_sl%shoplist(i)%bvar,myvfn%fnds(j)%bin(k)) > 0 ) cycle
      allocate(bvartmp(nvar))
      bvartmp(:nvar-1)=compile_sl%shoplist(i)%bvar
      call move_alloc(from=bvartmp ,to=compile_sl%shoplist(i)%bvar)
      compile_sl%shoplist(i)%bvar(nvar)=myvfn%fnds(j)%bin(k)
      nvar=nvar+1
    end do
  end do
  indfunc=indfunc+nfunc
end do

do i=1,nshoplist
  compile_sl%shoplist(i)%bvar=pack(compile_sl%shoplist(i)%bvar,c_e(compile_sl%shoplist(i)%bvar))
end do

end function compile_sl

end module alchimia

!>\example example_alchimia.f03
!!\brief Sample program to demostrate the alchimia module.
!!
!! This module use cuisine problems to demostrate the capacity of the module.

!>\example example_alchimiavg6d.f03
!!\brief  Sample program to demostrate the alchimia module with volgrid6d.

!>\example  example_alchimiav7d.f03
!! \brief Sample program to demostrate the alchimia module with vol7d.
