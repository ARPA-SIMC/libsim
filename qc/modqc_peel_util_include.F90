
!> Data validity check for confidence
elemental logical function vd/**/VOL7D_POLY_TYPES (flag)

/**/VOL7D_POLY_TYPE ,intent(in)  :: flag !< confidenza
      

vd/**/VOL7D_POLY_TYPES = .not. (realdat(flag,vol7d_var_new(qcattrvarsbtables(2),scalefactor=0)) <= qcpar%att .and. c_e(flag)&
 .and. c_e(qcpar%att))


return
end function vd/**/VOL7D_POLY_TYPES


!> Data gross error check
elemental logical function vdge/**/VOL7D_POLY_TYPES (flag)

/**/VOL7D_POLY_TYPE ,intent(in)  :: flag !< confidenza
      

vdge/**/VOL7D_POLY_TYPES = .not. (realdat(flag,vol7d_var_new(qcattrvarsbtables(2),scalefactor=0)) == qcpar%gross_error .and. c_e(flag) &
 .and. c_e(qcpar%gross_error))


return
end function vdge/**/VOL7D_POLY_TYPES


!> Data invalidated check
elemental logical function invalidated/**/VOL7D_POLY_TYPES(flag)

/**/VOL7D_POLY_TYPE ,intent(in)  :: flag !< attributo di invalidazione del dato
      
invalidated/**/VOL7D_POLY_TYPES= realdat(flag,vol7d_var_new(qcattrvarsbtables(1),scalefactor=0)) == qcpar%invalidated .and. c_e(flag) &
 .and. c_e(qcpar%invalidated)


return
end function invalidated/**/VOL7D_POLY_TYPES



!> Check data validity based on multiple confidences.
!! Compute final decision boolean flag
!! a controllo di qualità completo dovrebbe essere falso se solo una di queste condizioni si verifica:
!! a) invalidato
!! b) gross error check fallito
!! c) flag1 AND flag2 AND flag3 sotto soglia
ELEMENTAL LOGICAL FUNCTION qcsummaryflag/**/VOL7D_POLY_TYPES(flag0, flag1, flag2, flag3)
/**/VOL7D_POLY_TYPE ,intent(in),optional :: flag0
/**/VOL7D_POLY_TYPE ,intent(in),optional :: flag1
/**/VOL7D_POLY_TYPE ,intent(in),optional :: flag2
/**/VOL7D_POLY_TYPE ,intent(in),optional :: flag3


#ifdef VOL7D_POLY_ISC

qcsummaryflag/**/VOL7D_POLY_TYPES = .NOT.invalidated(optio_3/**/VOL7D_POLY_TYPES(flag0)) .AND. &
 vdge(optio_3/**/VOL7D_POLY_TYPES(flag1)) .AND. &
 vd(optio_3/**/VOL7D_POLY_TYPES(flag1)) .AND. &
 vd(optio_3/**/VOL7D_POLY_TYPES(flag2)) .AND. &
 vd(optio_3/**/VOL7D_POLY_TYPES(flag3))


contains

!> Return the optional value if present, otherwise return missing value.
!! N.B. elemental version of optio_c
elemental function optio_3c(var) result(char)

character (len=*),intent(in),optional  :: var !< variable to be checked
!here the string len needed is 3 but so is more easy
/**/VOL7D_POLY_TYPE :: char

if (present(var))then
  char=var
else
  char=cmiss
end if

return
end function optio_3c

#else

qcsummaryflag/**/VOL7D_POLY_TYPES = .NOT.invalidated(optio_/**/VOL7D_POLY_TYPES(flag0)) .AND. &
 vdge(optio_/**/VOL7D_POLY_TYPES(flag1)) .AND. &
 vd(optio_/**/VOL7D_POLY_TYPES(flag1)) .AND. &
 vd(optio_/**/VOL7D_POLY_TYPES(flag2)) .AND. &
 vd(optio_/**/VOL7D_POLY_TYPES(flag3))


#endif


END FUNCTION qcsummaryflag/**/VOL7D_POLY_TYPES

