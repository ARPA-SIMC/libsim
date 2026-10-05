MODULE termo

USE alchimia
USE termolib
USE array_utilities
USE missing_values

IMPLICIT NONE

private
public register_termo

contains

subroutine register_termo(vfn)

  type(fndsv),intent(inout) :: vfn

  call fnregister(vfn,td_t_p2ept_def())
  call fnregister(vfn,t_p2pt_def())
  call fnregister(vfn,td_t_p2wbt_def())
  call fnregister(vfn,dd_ff2u_v_def())
  call fnregister(vfn,u_v2dd_ff_def())
  call fnregister(vfn,r_t2td_def())
  call fnregister(vfn,ff_swd_lwb2pgt_def())
  call fnregister(vfn,q_p_t2r_def())
  call fnregister(vfn,t_td2r_def())
  call fnregister(vfn,swb_alb2swd_def())
  call fnregister(vfn,swd_alb2swb_def())
  call fnregister(vfn,swdir_swdif2swd_def())
  call fnregister(vfn,swb_lwb2glb_def())
  call fnregister(vfn,t_p_w2omega_def())
  call fnregister(vfn,snow_ls_conv2tot_def())
  call fnregister(vfn,rain_snow_conv2tot_def())
  call fnregister(vfn,rain_snow_ls_conv2tot_def())
  call fnregister(vfn,snow_ls_conv_gr2tot_def())
  CALL fnregister(vfn,z_fi_def())
  call fnregister(vfn,fi_z_def())
  call fnregister(vfn,z_h_def())
  call fnregister(vfn,h_z_def())
  call fnregister(vfn,r_p_t2q_def())
  call fnregister(vfn,swgwmo2swgsim_def())
  call fnregister(vfn,swgsim2swgwmo_def())
  call fnregister(vfn,rh_t_td_p2vis_def())
  call fnregister(vfn,rho_qcw2vis_def())
  call fnregister(vfn,tv_p2rho_def())

end subroutine register_termo

subroutine  td_t_p2ept(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  where (c_e(in(:,index_c(mybin,bin(3)))))
    out(:,index_c(mybout,bout(1)))=ooee(&
     in(:,index_c(mybin,bin(1))),&
     in(:,index_c(mybin,bin(2))),&
     in(:,index_c(mybin,bin(3)))/100.)
  elsewhere
    out(:,index_c(mybout,bout(1)))=rmiss
  endwhere

end subroutine td_t_p2ept


subroutine t_p2pt(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  where (c_e(in(:,index_c(mybin,bin(2)))))
    out(:,index_c(mybout,bout(1)))=o(in(:,index_c(mybin,bin(1))),in(:,index_c(mybin,bin(2)))/100.)
  elsewhere
    out(:,index_c(mybout,bout(1)))=rmiss
  end where

end subroutine t_p2pt


type(fnds) function td_t_p2ept_def()
  call init(td_t_p2ept_def,"td_t_p2ept",&
       [character(len=10) :: "B12103","B12101","B10004"],&
       [character(len=10) :: "B12193"],0,func=td_t_p2ept)
end function td_t_p2ept_def


type(fnds) function t_p2pt_def()
  call init(t_p2pt_def,"t_p2pt",&
       [character(len=10) :: "B12101","B10004"],&
       [character(len=10) :: "B12192"],0,func=t_p2pt)
end function t_p2pt_def

SUBROUTINE td_t_p2wbt(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL,INTENT(in) :: in(:,:)
REAL,INTENT(out) :: out(:,:)

WHERE (c_e(in(:,index_c(mybin,bin(3)))))
  out(:,index_c(mybout,bout(1)))=tw(in(:,index_c(mybin,bin(1))),in(:,index_c(mybin,bin(2))),in(:,index_c(mybin,bin(3)))/100.)
ELSEWHERE
  out(:,index_c(mybout,bout(1)))=rmiss
END WHERE

END SUBROUTINE td_t_p2wbt


type(fnds) function td_t_p2wbt_def()
  call init(td_t_p2wbt_def,"td_t_p2wbt",&
       [character(len=10) :: "B12103","B12101","B10004"],&
       [character(len=10) :: "B12102"],0,func=td_t_p2wbt)
end function td_t_p2wbt_def


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

  call uvwind(in (:,index_c(mybin, bin(1))),in (:,index_c(mybin, bin(2))), &
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

  out(:,index_c(mybout,bout(1)))= trug(in(:,index_c(mybin, bin(1))), in(:,index_c(mybin, bin(2))))

end subroutine r_t2td

type(fnds) function r_t2td_def()
  call init(r_t2td_def,"r_t2td",&
   [character(len=10) :: "B13003","B12101"],&
   [character(len=10) :: "B12103"],0,func=r_t2td)
end function r_t2td_def


!RELHUMTOQ(RH,PT,T)

subroutine  r_p_t2q(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  where (c_e(in(:,index_c(mybin,bin(2)))))
    out(:,index_c(mybout,bout(1)))= RELHUMTOQ(&
     in (:,index_c(mybin ,bin (1))),&
     in (:,index_c(mybin ,bin (2)))/100.,&
     in (:,index_c(mybin ,bin (3))))
  elsewhere
    out(:,index_c(mybout,bout(1)))=rmiss
  endwhere

end subroutine r_p_t2q

type(fnds) function r_p_t2q_def()
  call init(r_p_t2q_def,"r_p_t2q",&
   [character(len=10) :: "B13003","B10004","B12101"],&
   [character(len=10) :: "B13001"],0,func=r_p_t2q)
end function r_p_t2q_def


subroutine  ff_swd_lwb2pgt(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
real, intent(in) :: in(:,:)
real, intent(out) :: out(:,:)

out(:,index_c(mybout,bout(1)))= pgt(in (:,index_c(mybin ,bin (1))),in (:,index_c(mybin ,bin (2))),in (:,index_c(mybin ,bin (3))))

!!$print *, "TEST  in -------->>>>",in (1,index_c(mybin ,bin (1))),&
!!$ in (1,index_c(mybin ,bin (2))),&
!!$ in (1,index_c(mybin ,bin (3)))
!!$print *, "TEST     -------->>>>",out(1,index_c(mybout,bout(1)))


end subroutine ff_swd_lwb2pgt

type(fnds) function ff_swd_lwb2pgt_def()
call init(ff_swd_lwb2pgt_def,"ff_swd_lwb2pgt",&
 [character(len=10) :: "B11002","B14018","B14196"],&
 [character(len=10) :: "B11193"],0,func=ff_swd_lwb2pgt)
end function ff_swd_lwb2pgt_def


subroutine  swd_alb2swb(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
real, intent(in) :: in(:,:)
real, intent(out) :: out(:,:)

out(:,index_c(mybout,bout(1)))= swbudget(in (:,index_c(mybin ,bin (1))),in (:,index_c(mybin ,bin (2))))

end subroutine swd_alb2swb

type(fnds) function swd_alb2swb_def()
call init(swd_alb2swb_def,"swd_alb2swb",&
 [character(len=10) :: "B14018","B14019"],&
 [character(len=10) :: "B14197"],0,func=swd_alb2swb)
end function swd_alb2swb_def


subroutine  swb_alb2swd(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
real, intent(in) :: in(:,:)
real, intent(out) :: out(:,:)

out(:,index_c(mybout,bout(1)))= swdown(in (:,index_c(mybin ,bin (1))),in (:,index_c(mybin ,bin (2))))

end subroutine swb_alb2swd

type(fnds) function swb_alb2swd_def()
call init(swb_alb2swd_def,"swb_alb2swd",&
 [character(len=10) :: "B14197","B14019"],&
 [character(len=10) :: "B14018"],0,func=swb_alb2swd)
end function swb_alb2swd_def


subroutine swdir_swdif2swd(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
real, intent(in) :: in(:,:)
real, intent(out) :: out(:,:)

out(:,index_c(mybout,bout(1)))= swdown_dirdif(in (:,index_c(mybin ,bin (1))),in (:,index_c(mybin ,bin (2))))

end subroutine swdir_swdif2swd

type(fnds) function swdir_swdif2swd_def()
call init(swdir_swdif2swd_def,"swdir_swdif2swd",&
 [character(len=10) :: "B14194","B14195"],&
 [character(len=10) :: "B14018"],0,func=swdir_swdif2swd)
end function swdir_swdif2swd_def


subroutine swb_lwb2glb(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
real, intent(in) :: in(:,:)
real, intent(out) :: out(:,:)

out(:,index_c(mybout,bout(1))) = glb_swblwb(in(:,index_c(mybin,bin(1))),in(:,index_c(mybin,bin(2))))

end subroutine swb_lwb2glb

type(fnds) function swb_lwb2glb_def()
call init(swb_lwb2glb_def,"swb_lwb2glb",&
 [character(len=10) :: "B14196","B14197"],&
 [character(len=10) :: "B14016"],0,func=swb_lwb2glb)
end function swb_lwb2glb_def


subroutine swgwmo2swgsim(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
real, intent(in) :: in(:,:)
real, intent(out) :: out(:,:)

out(:,index_c(mybout, bout(1))) = in(:,index_c(mybin, bin(1)))

end subroutine swgwmo2swgsim

type(fnds) function swgwmo2swgsim_def()
call init(swgwmo2swgsim_def,"swgwmo2swgsim",&
 [character(len=10) :: "B14021"],&
 [character(len=10) :: "B14198"],0,func=swgwmo2swgsim)
end function swgwmo2swgsim_def


subroutine swgsim2swgwmo(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
real, intent(in) :: in(:,:)
real, intent(out) :: out(:,:)

out(:,index_c(mybout, bout(1))) = in(:,index_c(mybin, bin(1)))

end subroutine swgsim2swgwmo

type(fnds) function swgsim2swgwmo_def()
call init(swgsim2swgwmo_def,"swgsim2swgwmo",&
 [character(len=10) :: "B14198"],&
 [character(len=10) :: "B14021"],0,func=swgsim2swgwmo)
end function swgsim2swgwmo_def


subroutine  q_p_t2r(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1)))= QTORELHUM(in (:,index_c(mybin, bin(1))),&
   in (:,index_c(mybin, bin(2))),in (:,index_c(mybin, bin(3))))

end subroutine q_p_t2r

type(fnds) function q_p_t2r_def()
  call init(q_p_t2r_def,"q_p_t2r",&
   [character(len=10) :: "B13001","B10004","B12101"],&
   [character(len=10) :: "B13003"],0,func=q_p_t2r)
end function q_p_t2r_def


subroutine  t_td2r(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1))) = fr(in (:,index_c(mybin ,bin (1))),&
   in (:,index_c(mybin ,bin (2))))

end subroutine t_td2r

type(fnds) function t_td2r_def()
  call init(t_td2r_def,"t_td2r",&
   [character(len=10) :: "B12101","B12103"],&
   [character(len=10) :: "B13003"],0,func=t_td2r)
end function t_td2r_def


subroutine t_p_w2omega(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1))) = omega_simple(in(:,index_c(mybin,bin(1))), &
   in(:,index_c(mybin,bin(2))), in(:,index_c(mybin,bin(3))))

end subroutine t_p_w2omega

type(fnds) function t_p_w2omega_def()
  call init(t_p_w2omega_def,"t_p_w2omega", &
   [character(len=10) :: "B12101","B10004","B11006"], &
   [character(len=10) :: "B11005"],0,func=t_p_w2omega)
end function t_p_w2omega_def


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
!!$td_pq(p,q)
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


subroutine snow_ls_conv2tot(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1))) = in(:,index_c(mybin,bin(1))) + &
   in(:,index_c(mybin,bin(2)))

end subroutine snow_ls_conv2tot

type(fnds) function snow_ls_conv2tot_def()
  call init(snow_ls_conv2tot_def,"snow_ls_conv2tot", &
   [character(len=10) :: "B13201","B13203"], &
   [character(len=10) :: "B13205"],0,func=snow_ls_conv2tot)
end function snow_ls_conv2tot_def


subroutine rain_snow_conv2tot(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1))) = in(:,index_c(mybin,bin(1))) + &
   in(:,index_c(mybin,bin(2)))

end subroutine rain_snow_conv2tot

type(fnds) function rain_snow_conv2tot_def()
  call init(rain_snow_conv2tot_def,"rain_snow_conv2tot", &
   [character(len=10) :: "B13202","B13203"], &
   [character(len=10) :: "B13204"],0,func=rain_snow_conv2tot)
end function rain_snow_conv2tot_def


subroutine rain_snow_ls_conv2tot(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1))) = in(:,index_c(mybin,bin(1))) + &
   in(:,index_c(mybin,bin(2))) + in(:,index_c(mybin,bin(3))) + &
   in(:,index_c(mybin,bin(4)))

end subroutine rain_snow_ls_conv2tot

type(fnds) function rain_snow_ls_conv2tot_def()
  call init(rain_snow_ls_conv2tot_def,"rain_snow_ls_conv2tot", &
   [character(len=10) :: "B13200","B13201","B13202","B13203"], &
   [character(len=10) :: "B13011"],0,func=rain_snow_ls_conv2tot)
end function rain_snow_ls_conv2tot_def


subroutine snow_ls_conv_gr2tot(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,index_c(mybout,bout(1))) = in(:,index_c(mybin,bin(1))) + &
   in(:,index_c(mybin,bin(2))) + in(:,index_c(mybin,bin(3)))

end subroutine snow_ls_conv_gr2tot

type(fnds) function snow_ls_conv_gr2tot_def()
  call init(snow_ls_conv_gr2tot_def,"snow_ls_conv_gr2tot", &
   [CHARACTER(len=10) :: "B13201","B13203","B13236"], &
   [character(len=10) :: "B13237"],0,func=snow_ls_conv_gr2tot)
end function snow_ls_conv_gr2tot_def


SUBROUTINE z_fi(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL, INTENT(in) :: in(:,:)
REAL, INTENT(out) :: out(:,:)

WHERE (c_e(in(:,index_c(mybin,bin(1)))))
  out(:,index_c(mybout,bout(1))) = in(:,index_c(mybin,bin(1)))/gearth
ELSEWHERE
  out(:,index_c(mybout,bout(1))) = rmiss
END WHERE

END SUBROUTINE z_fi

TYPE(fnds) FUNCTION z_fi_def()
CALL init(z_fi_def,"z_fi", &
 [CHARACTER(len=10) :: "B10008"], &
 [CHARACTER(len=10) :: "B10009"],0,func=z_fi)
END FUNCTION z_fi_def


SUBROUTINE fi_z(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL, INTENT(in) :: in(:,:)
REAL, INTENT(out) :: out(:,:)

WHERE (c_e(in(:,index_c(mybin,bin(1)))))
  out(:,index_c(mybout,bout(1))) = in(:,index_c(mybin,bin(1)))*gearth
ELSEWHERE
  out(:,index_c(mybout,bout(1))) = rmiss
END WHERE

END SUBROUTINE fi_z

TYPE(fnds) FUNCTION fi_z_def()
CALL init(fi_z_def,"fi_z", &
 [CHARACTER(len=10) :: "B10009"], &
 [CHARACTER(len=10) :: "B10008"],0,func=fi_z)
END FUNCTION fi_z_def


SUBROUTINE z_h(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL, INTENT(in) :: in(:,:)
REAL, INTENT(out) :: out(:,:)

out(:,index_c(mybout,bout(1))) = in(:,index_c(mybin,bin(1)))

END SUBROUTINE z_h

TYPE(fnds) FUNCTION z_h_def()
CALL init(z_h_def,"z_h", &
 [CHARACTER(len=10) :: "B10007"], &
 [CHARACTER(len=10) :: "B10009"],0,func=z_h)
END FUNCTION z_h_def


SUBROUTINE h_z(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL, INTENT(in) :: in(:,:)
REAL, INTENT(out) :: out(:,:)

out(:,index_c(mybout,bout(1))) = in(:,index_c(mybin,bin(1)))

END SUBROUTINE h_z

TYPE(fnds) FUNCTION h_z_def()
CALL init(h_z_def,"h_z", &
 [CHARACTER(len=10) :: "B10009"], &
 [CHARACTER(len=10) :: "B10007"],0,func=h_z)
END FUNCTION h_z_def


SUBROUTINE rh_t_td_p2vis(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL, INTENT(in) :: in(:,:)
REAL, INTENT(out) :: out(:,:)

out(:,index_c(mybout,bout(1))) = vis_boudala( &
 in(:,index_c(mybin,bin(1))), in(:,index_c(mybin,bin(2))), &
 in(:,index_c(mybin,bin(3))))

END SUBROUTINE rh_t_td_p2vis

TYPE(fnds) FUNCTION rh_t_td_p2vis_def()
CALL init(rh_t_td_p2vis_def,"rh_t_td_p2vis", &
 [CHARACTER(len=10) :: "B13003","B12101","B12103"], &
 [CHARACTER(len=10) :: "B20001"],0,func=rh_t_td_p2vis)
END FUNCTION rh_t_td_p2vis_def


SUBROUTINE rho_qcw2vis(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL, INTENT(in) :: in(:,:)
REAL, INTENT(out) :: out(:,:)

out(:,index_c(mybout,bout(1))) = vis_lwc( &
 in(:,index_c(mybin,bin(1))), in(:,index_c(mybin,bin(2))))

END SUBROUTINE rho_qcw2vis

TYPE(fnds) FUNCTION rho_qcw2vis_def()
CALL init(rho_qcw2vis_def,"rho_qcw2vis", &
 [CHARACTER(len=10) :: "B12194","B13192"], &
 [CHARACTER(len=10) :: "B20001"],0,func=rho_qcw2vis)
! [CHARACTER(len=10) :: "B12101","B12103","B10004","B13192"], &
END FUNCTION rho_qcw2vis_def


SUBROUTINE tv_p2rho(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL, INTENT(in) :: in(:,:)
REAL, INTENT(out) :: out(:,:)

out(:,index_c(mybout,bout(1))) = airden( &
 in(:,index_c(mybin,bin(1))), in(:,index_c(mybin,bin(2))))

END SUBROUTINE tv_p2rho

TYPE(fnds) FUNCTION tv_p2rho_def()
CALL init(tv_p2rho_def,"tv_p2rho", &
 [CHARACTER(len=10) :: "B12107","B10004"], &
 [CHARACTER(len=10) :: "B12194"],0,func=tv_p2rho)
END FUNCTION tv_p2rho_def

SUBROUTINE t_q2tv(mybin,mybout,bin,bout,in,out)
CHARACTER(len=10),INTENT(in) :: mybin(:)  !< vector table B  WMO input  in user's data
CHARACTER(len=10),INTENT(in) :: mybout(:) !< vector table B  WMO output in user's data
CHARACTER(len=10),INTENT(in) :: bin(:)    !< vector table B  WMO input used by function
CHARACTER(len=10),INTENT(in) :: bout(:)   !< vector table B  WMO output used by function
REAL, INTENT(in) :: in(:,:)
REAL, INTENT(out) :: out(:,:)

out(:,index_c(mybout,bout(1))) = airden( &
 in(:,index_c(mybin,bin(1))), in(:,index_c(mybin,bin(2))))

END SUBROUTINE t_q2tv

TYPE(fnds) FUNCTION t_q2tv_def()
CALL init(t_q2tv_def,"t_q2tv", &
 [CHARACTER(len=10) :: "B12101","B13001"], &
 [CHARACTER(len=10) :: "B12107"],0,func=t_q2tv)
END FUNCTION t_q2tv_def

END MODULE termo
