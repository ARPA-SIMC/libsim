module termo

USE alchimia
USE termolib
USE array_utilities

IMPLICIT NONE

private
public register_termo

contains

subroutine register_termo(vfn)

  type(fndsv),intent(inout) :: vfn

  call fnregister(vfn,t_p2pt_def())
  call fnregister(vfn,dd_ff2u_v_def())
  call fnregister(vfn,u_v2dd_ff_def())
  call fnregister(vfn,r_t2td_def())
  call fnregister(vfn,ff_swd_lwb2pgt_def())

end subroutine register_termo

subroutine  t_p2pt(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1)))=o(in(:,index_c(mybin,bin(1))),in(:,index_c(mybin,bin(2)))/100.)

end subroutine t_p2pt

type(fnds) function t_p2pt_def()
  call init(t_p2pt_def,"t_p2pt",&
       [character(len=10) :: "B12101","B10004"],&
       [character(len=10) :: "B12192"],0,func=t_p2pt)
end function t_p2pt_def


subroutine  dd_ff2u_v(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  call uv(in (:,index_c(mybin ,bin (1))),in (:,index_c(mybin ,bin (2))), &
          out(:,index_c(mybout,bout(1))),out(:,index_c(mybout,bout(2))))

end subroutine dd_ff2u_v

type(fnds) function dd_ff2u_v_def()
  call init(dd_ff2u_v_def,"dd_ff2u_v",&
       [character(len=10) :: "B11001","B11002"],&
       [character(len=10) :: "B11003","B11004"],0,func=dd_ff2u_v)
end function dd_ff2u_v_def


subroutine  u_v2dd_ff(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  call uvwind(in (:,index_c(mybin ,bin (1))),in (:,index_c(mybin ,bin (2))), &
          out(:,index_c(mybout,bout(1))),out(:,index_c(mybout,bout(2))))

end subroutine u_v2dd_ff

type(fnds) function u_v2dd_ff_def()
  call init(u_v2dd_ff_def,"u_v2dd_ff",&
   [character(len=10) :: "B11003","B11004"],&
   [character(len=10) :: "B11002","B11001"],0,func=u_v2dd_ff)
end function u_v2dd_ff_def


!TRUG(UMID,T)

subroutine  r_t2td(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1)))= trug(in (:,index_c(mybin ,bin (1))),in (:,index_c(mybin ,bin (2))))
          
end subroutine r_t2td

type(fnds) function r_t2td_def()
  call init(r_t2td_def,"r_t2td",&
   [character(len=10) :: "B13003","B12101"],&
   [character(len=10) :: "B12103"],0,func=r_t2td)
end function r_t2td_def


subroutine  ff_swd_lwb2pgt(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
real, intent(in) :: in(:,:)
real, intent(out) :: out(:,:)

out(:,index_c(mybout,bout(1)))= pgt(in (:,index_c(mybin ,bin (1))),in (:,index_c(mybin ,bin (2))),in (:,index_c(mybin ,bin (3))))

end subroutine ff_swd_lwb2pgt

type(fnds) function ff_swd_lwb2pgt_def()
call init(ff_swd_lwb2pgt_def,"ff_swd_lwb2pgt",&
 [character(len=10) :: "B11002","B14018","B14017"],&
 [character(len=10) :: "B11193"],0,func=ff_swd_lwb2pgt)
end function ff_swd_lwb2pgt_def


! TODO

!!$OE(TD,TT,PT)
!!$
!!$OW(TD,TT,PT)
!!$
!!$OS(T,PT)
!!$
!!$TE(TD,TT,PT)
!!$
!!$
!!$TW( TD,TT,PT )
!!$
!!$TVIR(TD,TT,PT)
!!$
!!$USPEC(TD,PT)
!!$
!!$FR (T,TD )
!!$
!!$W(TD,PT) 
!!$
!!$QTORELHUM(Q,PT,T) 
!!$
!!$td_pq(p,q)
!!$
!!$RELHUMTOQ(RH,PT,T)
!!$
!!$ESAT(T)
!!$
!!$tesat(e)
!!$
!!$ALCL(TD,TT,PT)
!!$
!!$ALCLM(aw,ao,pt)  
!!$
!!$TMR(W,P)
!!$
!!$TDA(OO,P)
!!$
!!$TSA(OSAT,PT)
!!$

end module termo
