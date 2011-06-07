module forno

USE alchimia

IMPLICIT NONE

integer, parameter :: forno_nfunc=4


contains

subroutine register_forno(vfn)

  type(fnds),intent(inout) :: vfn(:)

  call fnregister(vfn,inforna_def())
  call fnregister(vfn,rosola_def())
  call fnregister(vfn,ripassa_def())
  call fnregister(vfn,brucia_def())

end subroutine register_forno


integer function  inforna(bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: bin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bout(npar) !<  table B  WMO
  real, intent(in) :: in(npar)
  real, intent(out) :: out(npar)

  out(1)=4.

 end function inforna

type(fnds) function inforna_def()
  call init(inforna_def,"inforna",&
       [character(len=10) :: "farina","lievito"],&
       [character(len=10) :: "pane"],0,func=inforna)
end function inforna_def



subroutine rosola(bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: bin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bout(npar) !<  table B  WMO
  real, intent(in) :: in(npar)
  real, intent(out) :: out(npar)

  out(1)=5.

end subroutine rosola

type(fnds) function rosola_def()
  call init(rosola_def,"rosola",&
       [character(len=10) :: "patate","sale"],&
       [character(len=10) :: "patatine"],0)
end function rosola_def


subroutine ripassa(bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: bin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bout(npar) !<  table B  WMO
  real, intent(in) :: in(npar)
  real, intent(out) :: out(npar)

  out(1)=6.

end subroutine ripassa

type(fnds) function ripassa_def()
  call init(ripassa_def,"ripassa",&
       [character(len=10) :: "patatine","pole.frit."],&
       [character(len=10) :: "pole.pata."],0)
end function ripassa_def


subroutine brucia(bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: bin(npar) !< table B  WMO
  CHARACTER(len=10),intent(out) :: bout(npar) !<  table B  WMO
  real, intent(in) :: in(npar)
  real, intent(out) :: out(npar)

  out(1)=5.

end subroutine brucia

type(fnds) function brucia_def()
  call init(brucia_def,"brucia",&
       [character(len=10) :: "patate","sale"],&
       [character(len=10) :: "carboncini"],0)
end function brucia_def



end module forno
