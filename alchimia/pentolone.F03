module pentolone

USE alchimia
IMPLICIT NONE

contains

subroutine register_pentolone(vfn)

  type(fndsv),intent(inout) :: vfn

  call fnregister(vfn,bolli_def())
  call fnregister(vfn,mescola_def())
  call fnregister(vfn,friggi_def())
  call fnregister(vfn,gira_def())
  
end subroutine register_pentolone


subroutine  bolli(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out=1.

end subroutine bolli

type(fnds) function bolli_def()

!!$  call init(bolli_def,"bolli",&
!!$       (/"pollo     ","gommosa   "/),&
!!$       (/"brodo     "/),0,bolli)
!!$
  call init(bolli_def,"bolli",&
       [character(len=10) :: "pollo","gommosa"],&
       [character(len=10) :: "brodo","lesso"],0,func=bolli)
       ![character(len=10) :: "brodo"],0)

end function bolli_def


subroutine mescola(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out=2.

end subroutine mescola


type(fnds) function mescola_def()

  call init(mescola_def,"mescola",&
       [character(len=10) :: "mais","sale","acqua"],&
       [character(len=10) :: "polenta","sudore"],0,func=mescola)
!!$
!!$  call init(mescola_def,"mescola",&
!!$       (/"mais      ","sale      ","acqua     "/),&
!!$       (/"polenta   "/),0)

end function mescola_def


subroutine friggi(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out=3.

end subroutine friggi


type(fnds) function friggi_def()

  call init(friggi_def,"friggi",&
       [character(len=10) :: "olio","polenta"],&
       [character(len=10) :: "pole.frit."],0,func=friggi)
!!$
!!$  call init(friggi_def,"friggi",&
!!$       (/"olio      ","polenta   "/),&
!!$       (/"pole.frit."/),0)

end function friggi_def



subroutine gira(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out=4.

end subroutine gira


type(fnds) function gira_def()
  call init(gira_def,"gira",&
       [character(len=10) :: "mais","sale"],&
       [character(len=10) :: "broda"],0,func=gira)
!!$
!!$  call init(gira_def,"gira",&
!!$       (/"mais      ","sale      "/),&
!!$       (/"broda     "/),0)


end function gira_def


end module pentolone
