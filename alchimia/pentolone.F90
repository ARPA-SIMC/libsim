module pentolone

USE alchimia
IMPLICIT NONE

integer, parameter :: pentolone_nfunc=4

contains

subroutine register_pentolone(vfn)

  type(fnds),intent(inout) :: vfn(:)

  call fnregister(vfn,bolli_def())
  call fnregister(vfn,mescola_def())
  call fnregister(vfn,friggi_def())
  call fnregister(vfn,gira_def())
  
end subroutine register_pentolone


integer function  bolli(bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: bin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bout(npar) !<  table B  WMO
  real, intent(in) :: in(npar)
  real, intent(out) :: out(npar)

  out(1)=1.
  bolli=0

end function bolli

type(fnds) function bolli_def()

!!$  call init(bolli_def,"bolli",&
!!$       (/"pollo     ","gommosa   "/),&
!!$       (/"brodo     "/),0,bolli)
!!$
  call init(bolli_def,"bolli",&
       [character(len=10) :: "pollo","gommosa"],&
       [character(len=10) :: "brodo"],0,func=bolli)
       ![character(len=10) :: "brodo"],0)

end function bolli_def


integer function mescola(bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: bin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bout(npar) !<  table B  WMO
  real, intent(in) :: in(npar)
  real, intent(out) :: out(npar)

  out(1)=2.
  mescola=0

end function mescola


type(fnds) function mescola_def()

  call init(mescola_def,"mescola",&
       [character(len=10) :: "mais","sale","acqua"],&
       [character(len=10) :: "polenta"],0,func=mescola)
!!$
!!$  call init(mescola_def,"mescola",&
!!$       (/"mais      ","sale      ","acqua     "/),&
!!$       (/"polenta   "/),0)

end function mescola_def


integer function friggi(bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: bin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bout(npar) !<  table B  WMO
  real, intent(in) :: in(npar)
  real, intent(out) :: out(npar)

  out(1)=3.
  friggi=0

end function friggi


type(fnds) function friggi_def()

  call init(friggi_def,"friggi",&
       [character(len=10) :: "olio","polenta"],&
       [character(len=10) :: "pole.frit."],0,func=friggi)
!!$
!!$  call init(friggi_def,"friggi",&
!!$       (/"olio      ","polenta   "/),&
!!$       (/"pole.frit."/),0)

end function friggi_def



integer function gira(bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: bin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bout(npar) !<  table B  WMO
  real, intent(in) :: in(npar)
  real, intent(out) :: out(npar)

  out(1)=2.
  gira=0

end function gira


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
