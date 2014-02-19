
module list_link
  private
  public :: link
  type link
     private
     class(*), pointer :: value => null() !> value stored in link
     type(link), pointer :: next => null()!> next link in list
     type(link), pointer :: prev => null()!> next link in list
     contains
     procedure :: getValue    !> return value pointer
     procedure :: nextLink    !> return next pointer
     procedure :: prevLink    !> return next pointer
     procedure :: setNextLink !> set next pointer
     procedure :: setPrevLink !> set next pointer
  end type link

  interface link
   procedure constructor ! construct/initialize a link
  end interface

contains

function nextLink(this)
class(link) :: this
class(link), pointer :: nextLink
nextLink => this%next
end function nextLink

function prevLink(this)
class(link) :: this
class(link), pointer :: prevLink
prevLink => this%prev
end function prevLink
    
subroutine setNextLink(this,next)
class(link) :: this
type(link), pointer :: next
this%next => next
end subroutine setNextLink

subroutine setPrevLink(this,prev)
class(link) :: this
type(link), pointer :: prev
this%prev => prev
end subroutine setPrevLink

function getValue(this)
class(link) :: this
class(*), pointer :: getValue
getValue => this%value
end function getValue

function constructor(value)
type(link),pointer :: constructor
class(*) :: value
allocate(constructor)
constructor%prev => null()
constructor%next => null()
allocate(constructor%value, source=value)
end function constructor

end module list_link
