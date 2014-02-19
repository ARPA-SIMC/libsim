
module list_character
  use list_abstract
  private
  public :: characterList
  type, extends(list) :: characterList
   contains
!     procedure :: addCharacter                    ! add character in list
     procedure :: current => currentCharacter ! get character pointed by iterator
     procedure :: display => displayCharacter ! print the character list
!     generic :: add => addCharacter
  end type characterList

contains

subroutine displayCharacter(this)
class(characterList) :: this
class(*), pointer :: curr

call this%rewind()
do while(this%element())
  curr => this%Currentpoli()
  select type(curr)
  type is (character(len=*))
    print *, curr
  end select
  call this%next()
end do
end subroutine displayCharacter

!!$  subroutine addCharacter(this, value)
!!$    class(characterList) :: this
!!$    character value
!!$    class(*), allocatable :: v
!!$
!!$    allocate(v,source=value)
!!$    call this%addvalue(v)
!!$
!!$  end subroutine addCharacter


function currentCharacter(this)
character(len=:),pointer :: currentCharacter
class(characterList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (character(len=*))
  currentCharacter => v
end select
end function currentCharacter

end module list_character
