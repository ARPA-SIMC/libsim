!! abstract_list.f90

module list_abstract
  use list_link
  private
  public :: list
  type, abstract :: list
     private
     class(link),pointer :: firstLink => null() !> first link in list
     class(link),pointer :: lastLink => null()  !> last link in list
     class(link),pointer :: currLink => null()  !> list iterator
   contains
     procedure, non_overridable :: add     !> add class(*) to list
     procedure, non_overridable :: start   !> reset list iterator
     procedure, non_overridable :: next    !> increment list iterator
     procedure, non_overridable :: currentpoli !> get value from currLink
     procedure, non_overridable :: more    !> more values for iterator?
     procedure(displayValues), deferred :: display !> prints values in list
  end type list

  abstract interface
  subroutine displayValues(this)
    import list
    class(list) :: this
  end subroutine
  end interface

contains

subroutine add(this, value)
class(list) :: this
class(*) :: value
class(link), pointer :: newLink

if (.not. associated(this%firstLink)) then
  this%firstLink => link(value, this%firstLink)
  this%lastLink => this%firstLink
else
  newLink => link(value, this%lastLink%nextLink())
  call this%lastLink%setNextLink(newLink)
  this%lastLink => newLink
end if

end subroutine add

function currentpoli(this)
class(list) :: this
class(*), pointer :: Currentpoli
Currentpoli => this%currLink%getValue()
end function Currentpoli

subroutine next(this)
class(list) :: this
this%currLink => this%currLink%nextLink()
end subroutine next

function more(this)
class(list) :: this
logical more
more = associated(this%currLink)
end function more

subroutine start(this)
class(list) :: this
this%currLink => this%firstLink
end subroutine start

end module list_abstract
