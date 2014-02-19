
module list_abstract
  use list_link
  use missing_values
  private
  public :: list
  type, abstract :: list
     private
     class(link),pointer :: firstLink => null() !> first link in list
     class(link),pointer :: lastLink => null()  !> last link in list
     class(link),pointer :: currLink => null()  !> list iterator
     integer             :: index=imiss         !> index to current
   contains
     procedure, non_overridable :: append       !> add class(*) to end of list
     procedure, non_overridable :: prepend      !> add class(*) to beginning of list
     procedure, non_overridable :: insert       !> add class(*) to position in list
     procedure, non_overridable :: rewind       !> reset list iterator to start
     procedure, non_overridable :: forward      !> reset list iterator to end
     procedure, non_overridable :: seek         !> set list iterator to index
     procedure, non_overridable :: next         !> increment list iterator
     procedure, non_overridable :: prev         !> increment list iterator
     procedure, non_overridable :: currentpoli  !> get value from currLink
     procedure, non_overridable :: element      !> present current element
     procedure, non_overridable :: delete       !> delete values from list
     procedure(displayValues), deferred :: display !> prints values in list
  end type list

  abstract interface
  subroutine displayValues(this)
    import list
    class(list) :: this
  end subroutine
  end interface

contains

subroutine append(this, value)
class(list) :: this
class(*) :: value
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



subroutine prepend(this, value)
class(list) :: this
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
  call this%firstLink%setNextLink(newLink)

  this%firstLink => newLink
  this%index=this%index+1
end if
end subroutine prepend


logical function insert(this, value, index)
class(list) :: this
class(*) :: value
integer,optional :: index
class(link), pointer :: newLink,nextlink

newLink => link(value)

if (present(index)) then
  insert = this%seek(index)
  if (.not. insert) return
else
  insert=.true.
end if

if (.not. associated(this%currLink)) then
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
  call nextLink%setprevLink(newLink)
                                !verify if it's first or last
  if (.not. this%element())then
    this%firstLink => newLink
    this%lastLink => newLink
  end if
  this%index=this%index+1
end if

this%currLink  => newLink

end function insert


subroutine rewind(this)
class(list) :: this
this%currLink => this%firstLink
if (.not. associated(this%firstLink)) then
  this%index=imiss
end if
this%index=1
end subroutine rewind

subroutine forward(this)
class(list) :: this
this%currLink => this%lastLink
if (.not. associated(this%lastLink)) then
  this%index=imiss
end if

! index is unknow here
this%index=imiss

end subroutine forward

subroutine next(this)
class(list) :: this

if (this%element()) then
  this%currLink => this%currLink%nextLink()
  this%index=this%index+1
end if

end subroutine next

subroutine prev(this)
class(list) :: this

if (this%element()) then
  this%currLink => this%currLink%prevLink()
  if (c_e(this%index))this%index=this%index-1
end if

end subroutine prev


function currentpoli(this)
class(list) :: this
class(*), pointer :: Currentpoli
Currentpoli => this%currLink%getValue()
end function Currentpoli


logical function element(this)
class(list) :: this

element = associated(this%currLink)
end function element

logical function seek(this, index)
class(list) :: this
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

logical function delete(this, index)
class(list) :: this
integer,optional :: index

if (.not. associated(this%firstLink)) then
  return
else
  if (present(index)) then
    delete=this%seek(index)
    if(.not. delete) return
    call deleteitem()
  else
    call this%rewind()
    do while (this%element())
      call deleteitem()
      call this%next()
    end do
  end if
end if

contains

subroutine deleteitem()

class(link), pointer :: prevLink,nextLink

                                ! detach myitem
prevlink=>this%currlink%prevlink()
nextlink=>this%currlink%nextlink()

if (associated(prevLink)) call prevLink%setNextLink(nextLink)
if (associated(nextLink)) call nextLink%setPrevLink(prevLink)

deallocate(this%currlink)
this%currLink => prevlink

!!!!!!!!!!!!!   to do    manage first and last value  !!!!!!!

!!$if (.not. associated (this%currlink%prevlink())) then
!!$  this%firstLink => this%currlink
!!$end if

!!$if (.not. associated (this%currlink%nextlink())) then
!!$  this%lastLink => this%currlink
!!$end if

this%index=this%index-1

end subroutine deleteitem
end function delete

end module list_abstract
