program example_list

use list_mix
use list_integer
use list_real
use list_character
use kinds

implicit none
type(integerlist) :: list_int
type(reallist) :: list_real
type(characterlist) :: list_char
type(mixlist) :: list_mix

class(*),pointer :: val
integer values(10)
integer i

type mytype
  integer :: i
  real    :: r
end type mytype
type(mytype) :: ir=mytype(3,6.)
                                ! real list
call list_real%add(3.14)
call list_real%add(6.28)
print *,"display real list"
call list_real%display()

!!$! do not work !!!
!!$! compiler bug ?
!!$                                ! char list
!!$call list_char%add("hello world")
!!$call list_char%add("bye bye baby")
!!$print *,"display char list"
!!$call list_char%display()


                                ! test integer list
do i=1, 10
  call list_int%add(i)
enddo
print *,"display integer list"
call list_int%display()
print *
call list_int%start()
i = 1
do while(list_int%more())
  values(i) = list_int%current()
  call list_int%next()
  i = i + 1
end do
print *,"display integer vector from list"
print *, values
  
                                ! test mix list
  do i=1, 10
  call list_mix%add(i)
enddo
call list_mix%add(1.23)
call list_mix%add(4d0)
call list_mix%add(ir)
                                ! this do not work !
                                ! compiler bug ???
                                !  call list_mix%add("test1")
                                !  call list_mix%add("test2")

print *,"display mix list"
call list_mix%display()

print *,"print mix list with cast"
call list_mix%start()
do while (list_mix%more())
  
  val => list_mix%current()
  select type (x => val)
  type is (integer)
    print *,x
  type is (real)
    print *,x
  type is (doubleprecision)
    print *,x
  type is (integer(kind=int_b))
    print *,x
  type is (character(*))
    print *,x
  type is (mytype)
    print *,x%i,x%r
  end select
  
  call list_mix%next()
  
end do

end program example_list
