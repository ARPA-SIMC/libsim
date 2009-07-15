program testlog

use log4fortran

integer :: category,ier
character(len=512):: a_name

!it's very easy
call very_easy_log()


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name)

!init di log4fortran
ier=l4f_init()

!change the default verbosity level (dummy routine only !)
l4f_priority=L4F_DEBUG

!imposta a_name
category=l4f_category_get(a_name)

call l4f_category_log(category,L4F_ERROR,"erroraccio in log4fortran")
call l4f_category_log(category,L4F_INFO,"info in log4fortran")

! aggiungo una comunicazione in stderr
write(0,*) "erroraccio in stderr"


call logexample()

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

!aggiungo una comunicazione in stdout
write(6,*) "l4f_fini",ier


contains



  subroutine very_easy_log()

  CALL l4f_log(L4F_INFO,"nothing is more easy")

  return

  end subroutine very_easy_log


  subroutine logexample()

  character(len=512):: a_name
  integer :: false_category=-1,local_category

  !questa chiamata prende dal launcher il nome univoco
  call l4f_launcher(a_name)

  local_category=l4f_category_get(trim(a_name)//".logexample")

  CALL l4f_category_log(local_category,L4F_DEBUG,"inizia logexample")

  call spassosa(false_category)

  !chiudo la category
  call l4f_category_delete(local_category)

  end subroutine logexample

  subroutine spassosa(category)

  character(len=512):: a_name
  integer :: local_category,category

  !questa chiamata prende dal launcher il nome univoco
  call l4f_launcher(a_name)
    
  if (l4f_category_exist(category)) then

    CALL l4f_category_log(category,L4F_DEBUG,"inizia lo spasso")
    CALL l4f_category_log(category,L4F_ERROR,"lo spasso non riesce bene")

  else
    
    local_category=l4f_category_get(trim(a_name)//".spassosa")

    CALL l4f_category_log(local_category,L4F_DEBUG,"inizia lo spasso")
    CALL l4f_category_log(local_category,L4F_ERROR,"lo spasso non riesce bene")

    !chiudo la category
    call l4f_category_delete(local_category)


  end if

  end subroutine spassosa
    
end program testlog
    
