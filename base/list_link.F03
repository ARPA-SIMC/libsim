
module list_link
  private
  public :: link
  type link
     private
     class(*), pointer :: value => null() !> value stored in link
     type(link), pointer :: next => null()!> next link in list
     contains
     procedure :: getValue    !> return value pointer
     procedure :: nextLink    !> return next pointer
     procedure :: setNextLink !> set next pointer
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

  subroutine setNextLink(this,next)
  class(link) :: this
  type(link), pointer :: next
     this%next => next
  end subroutine setNextLink

  function getValue(this)
  class(link) :: this
  class(*), pointer :: getValue
  getValue => this%value
  end function getValue

  function constructor(value, next)
    type(link),pointer :: constructor
    class(*) :: value
    type(link), pointer :: next
    allocate(constructor)
    constructor%next => next
    allocate(constructor%value, source=value)
  end function constructor

end module list_link
