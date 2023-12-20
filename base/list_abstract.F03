!> \brief  abstract class to use lists in fortran 2003.
!!
!! Linked data structure is a data structure which
!! consists of a set of data records (nodes) linked together and organized by references .
!!
!! A doubly-linked list is a linked data structure that consists of a set
!! of sequentially linked records called nodes. Each node contains two
!! fields, called links, that are references to the previous and to the
!! next node in the sequence of nodes. The beginning and ending nodes'
!! previous and next links, respectively, point to some kind of
!! terminator.
!!
!! The program example is the better starting point:
!!\include example_list.F03
!!\ingroup base
!!
module list_abstract

  use list_link
  use missing_values
  implicit none
  private
  public :: list

!> Abstract implementation of doubly-linked list
  type, abstract :: list
     private
     class(link),pointer :: firstLink => null() !< first link in list
     class(link),pointer :: lastLink => null()  !< last link in list
     class(link),pointer :: currLink => null()  !< list iterator
     integer             :: index=imiss         !< index to current
   contains
     procedure, non_overridable :: append       !< add class(*) to end of list
     procedure, non_overridable :: prepend      !< add class(*) to beginning of list
     procedure, non_overridable :: insert       !< add class(*) to position in list
     procedure, non_overridable :: rewind       !< reset list iterator to start
     procedure, non_overridable :: forward      !< reset list iterator to end
     procedure, non_overridable :: seek         !< set list iterator to index
     procedure, non_overridable :: next         !< increment list iterator
     procedure, non_overridable :: prev         !< increment list iterator
     procedure, non_overridable :: currentpoli  !< get value from currLink
     procedure, non_overridable :: currentindex !< get index of currLink
     procedure, non_overridable :: element      !< associated current element
     procedure, non_overridable :: delete       !< delete values from list
     procedure, non_overridable :: countelements!< count values in list
!     procedure                  :: current => currentpoli !< get index of currLink
     procedure                  :: display      !< print values in list
!     procedure                  :: write_formatted
!     generic                    :: write(formatted) => write_formatted
!     procedure(displayValues), deferred :: display !> prints values in list
  end type list

  abstract interface
!> Print the list
  subroutine displayValues(this)
    import list
    class(list) :: this
  end subroutine
  end interface

contains


!!$SUBROUTINE write_formatted &
!!$(dtv, unit, iotype, v_list, iostat, iomsg)
!!$  INTEGER, INTENT(IN) :: unit
!!$  ! the derived-type value/variable
!!$  class(List), INTENT(IN) :: dtv
!!$  ! the edit descriptor string
!!$  CHARACTER (LEN=*), INTENT(IN) :: iotype
!!$  INTEGER, INTENT(IN) :: v_list(:)
!!$  INTEGER, INTENT(OUT) :: iostat
!!$  CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
!!$  write (unit, *, IOSTAT=iostat, IOMSG=iomsg)  &
!!$       "class(List)"
!!$ END SUBROUTINE
!!$

!> Print the list
subroutine display(this)
class(List),intent(inout) :: this

call this%rewind()
do while(this%element())
!  print *,"index:",this%currentindex()," value:", this%currentpoli()
  print *,"index:",this%currentindex()," value: polimorphic value (not printable)"
  call this%next()
end do
end subroutine display


!> count values in list
integer function countelements(this)
class(list),intent(inout)  :: this

if (.not.c_e(this%currentindex())) call this%rewind()
countelements=this%currentindex()

do while(this%element())
   countelements=this%currentindex()
   call this%next()
end do

if (.not. c_e(countelements)) countelements =0

end function countelements


!> add class(*) to end of list
subroutine append(this, value)
class(list),intent(inout) :: this
class(*),intent(in) :: value
class(link), pointer :: newLink

newLink => link(value)
this%currLink  => newLink

if (.not. associated(this%firstLink)) then
  this%firstLink => newLink
  this%lastLink  => newLink
  this%index=1
else
  call newLink%setPrevLink(this%lastLink)
  call this%lastLink%setNextLink(newLink)

  this%lastLink => newLink
  this%index=this%index+1
end if

end subroutine append


!> add class(*) to beginning of list
subroutine prepend(this, value)
class(list),intent(inout) :: this
class(*) :: value
class(link), pointer :: newLink

newLink => link(value)
this%currLink  => newLink

if (.not. associated(this%firstLink)) then
  this%firstLink => newLink
  this%lastLink  => newLink
  this%index=1
else
  call newLink%setnextLink(this%firstLink)
  call this%firstLink%setPrevLink(newLink)

  this%firstLink => newLink
  this%index=1
end if
end subroutine prepend

!> add class(*) to position in list
logical function insert(this, value, index)
class(list),intent(inout) :: this
class(*) :: value
integer :: index ! removed optional because of inconsistent behavior
class(link), pointer :: newLink,nextlink

newLink => link(value)

!if (present(index)) then
  insert = this%seek(index)
  if (.not. insert) return
!else
!  insert=.true.
!end if

if (.not. this%element()) then
                                !insert the first one
  this%firstLink => newLink
  this%lastLink  => newLink
  this%index=1
else
                                !set prev and next in new link
  call newLink%setPrevLink(this%currlink)
  call newLink%setNextLink(this%currlink%nextlink())

                                !break the chain and insert
  nextlink=>this%currlink%nextlink()
  call this%currLink%setNextLink(newLink)
                                !verify if it's last
  if (associated(nextlink))then
   call nextLink%setprevLink(newLink)
  else
   this%lastLink => newLink
  end if
  this%index=this%index+1
end if

this%currLink  => newLink

end function insert

!> get index of currLink
integer function currentindex(this)
class(list),intent(in) :: this
currentindex=this%index
end function currentindex

!> reset list iterator to start
subroutine rewind(this)
class(list),intent(inout) :: this
this%currLink => this%firstLink
if (.not. associated(this%firstLink)) then
  this%index=imiss
else
   this%index=1
end if
end subroutine rewind

!> reset list iterator to end
subroutine forward(this)
class(list),intent(inout) :: this
this%currLink => this%lastLink
if (.not. associated(this%lastLink)) then
  ! index is unknow here
  this%index=imiss
end if

end subroutine forward

!> increment list iterator
subroutine next(this)
class(list),intent(inout) :: this

if (this%element()) then
  this%currLink => this%currLink%nextLink()
  if (this%element())then
     if(c_e(this%index))this%index=this%index+1
  else
     this%index=imiss
  end if
end if

end subroutine next

!> increment list iterator
subroutine prev(this)
class(list),intent(inout) :: this

if (this%element()) then
  this%currLink => this%currLink%prevLink()
  if (this%element())then
     if(c_e(this%index))this%index=this%index-1
  else
     this%index=imiss
  end if

end if

end subroutine prev


!> get value from currLink
function currentpoli(this)
class(list),intent(in) :: this
class(*), pointer :: Currentpoli
class(*), pointer :: l_p
l_p => this%currLink%getValue()
Currentpoli => l_p
end function Currentpoli


!>return .true. if current element is associated
logical function element(this)
class(list),intent(in) :: this

element = associated(this%currLink)
end function element

!> set list iterator to index
!! return .false. if failed
logical function seek(this, index)
class(list),intent(inout) :: this
integer :: index

if (index == this%index) then
  seek =.true.
  return
end if

if (index < (this%index) .or. .not. c_e(this%index)) then
  call this%rewind()
end if

do while (this%element())
  if (index == this%index) then
    seek =.true.
    return
  end if
  call this%next()
end do

seek = .false.
return

end function seek

!> delete values from list
!! return .true. if succes
logical function delete(this, index)
class(list),intent(inout) :: this
integer,optional :: index
class(link),pointer :: itemtodelete

if (.not. associated(this%firstLink)) then
  delete=.false.
  return
else
  if (present(index)) then
    delete=this%seek(index)
    if(.not. delete) return
    call deleteitem()
  else
    delete=.true.
    call this%rewind()
    do while (this%element())
       !save pointer to delete
       itemtodelete=>this%currlink
       call this%next()
       deallocate(itemtodelete)
    end do
    this%firstLink => null() ! first link in list
    this%lastLink => null()  ! last link in list
    this%currLink => null()  ! list iterator
    this%index=imiss         ! index to current
  end if
end if

contains

subroutine deleteitem()

class(link), pointer :: prevLink,nextLink

! detach myitem"
prevlink=>this%currlink%prevlink()
nextlink=>this%currlink%nextlink()

if (associated(prevLink)) then
   call prevLink%setNextLink(nextLink)
else
   this%firstLink => nextlink
end if

if (associated(nextLink)) then
   call nextLink%setPrevLink(prevLink)
else
   this%lastLink => prevlink
end if

deallocate(this%currlink)

! set current to prev
this%currLink => prevlink

if (associated(this%firstLink))then
   this%index=max(this%index-1,1)
else
    this%index=imiss         ! index to current
endif

end subroutine deleteitem
end function delete

end module list_abstract
