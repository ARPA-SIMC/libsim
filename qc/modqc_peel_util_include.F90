
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
!! Quality control is complete if one of 3 conditions is verified:
!! a) invalidated data 
!! b) gross error check failed
!! c) tot variable less -1

!> Controllo di validita' del dato basato su test multipli.
!! Per il calcolo della validita' del dato (flag booleano B33007), si prendono in considerazione 3 test; il dato risulta invalidato (flag booleano posto a false) se e solo se uno dei test risulta soddisfatto:
!! a) il dato e' stato invalidato a mano (flag0=B33196=0)
!! b) il dato non ha passato il gross erro check (flag1=B33192=0)
!! c) la variabile tot risulta minore a -1
!! La variabile tot e' il risultato del confronto tra controllo climatologico (flag1, B33192), controllo temporale (flag2, B33193) e controllo spaziale (flag3, B33194).
!! Ad ognuno di tali controlli e' stato attribuito un punteggio a seconda che ciascuno dei valori relativi ai flag di qualita' risulti inferiore od uguale-maggiore di 10.
!! Nel dettaglio:
!! se B33192 < 10 tot=-1; se B33192>=10 tot=0
!! se B33193 < 10 tot=-1; se B33193>=10 tot=1
!! se B33194 < 10 tot=-1; se B33194>=10 tot=1
!! Ogni dato e' controllato nei 3 flag di qualita' presenti, e viene valutata la somma risultante di tot. Se tot risulta inferiore a -1, qcsummaryflag e' posto a false ed il dato e' invalitato (B33007=0). Se tot risulta maggiore od uguale a -1 qcsummaryflag e' true ed il dato e' valido.

ELEMENTAL LOGICAL FUNCTION qcsummaryflag/**/VOL7D_POLY_TYPES(flag0, flag1, flag2, flag3) 
/**/VOL7D_POLY_TYPE ,intent(in),optional :: flag0
/**/VOL7D_POLY_TYPE ,intent(in),optional :: flag1
/**/VOL7D_POLY_TYPE ,intent(in),optional :: flag2
/**/VOL7D_POLY_TYPE ,intent(in),optional :: flag3
integer :: tot


#ifdef VOL7D_POLY_ISC

if (invalidated(optio_3/**/VOL7D_POLY_TYPES(flag0))) then
  qcsummaryflag/**/VOL7D_POLY_TYPES = .false.
  return
endif

if ( .not. vdge(optio_3/**/VOL7D_POLY_TYPES(flag1))) then
  qcsummaryflag/**/VOL7D_POLY_TYPES = .false.
  return
endif

tot=0

if (.not. vd(optio_3/**/VOL7D_POLY_TYPES(flag1))) then 
  tot = tot -1
endif

if (.not. vd(optio_3/**/VOL7D_POLY_TYPES(flag2))) then 
  tot = tot -1
endif
if (vd(optio_3/**/VOL7D_POLY_TYPES(flag2)) .and. c_e(optio_3/**/VOL7D_POLY_TYPES(flag2))) then 
  tot = tot +1
endif

if (.not. vd(optio_3/**/VOL7D_POLY_TYPES(flag3))) then 
  tot = tot -1
endif
if (vd(optio_3/**/VOL7D_POLY_TYPES(flag3)) .and. c_e(optio_3/**/VOL7D_POLY_TYPES(flag3))) then 
  tot = tot +1
endif

qcsummaryflag/**/VOL7D_POLY_TYPES=(tot >= -1)

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


if (invalidated(optio_/**/VOL7D_POLY_TYPES(flag0))) then
  qcsummaryflag/**/VOL7D_POLY_TYPES = .false.
  return
endif

if ( .not. vdge(optio_/**/VOL7D_POLY_TYPES(flag1))) then
  qcsummaryflag/**/VOL7D_POLY_TYPES = .false.
  return
endif

tot=0

if (.not. vd(optio_/**/VOL7D_POLY_TYPES(flag1))) then 
  tot = tot -1
endif

if (.not. vd(optio_/**/VOL7D_POLY_TYPES(flag2))) then 
  tot = tot -1
endif
if (vd(optio_/**/VOL7D_POLY_TYPES(flag2)) .and. c_e(optio_/**/VOL7D_POLY_TYPES(flag2))) then 
  tot = tot +1
endif

if (.not. vd(optio_/**/VOL7D_POLY_TYPES(flag3))) then 
  tot = tot -1
endif
if (vd(optio_/**/VOL7D_POLY_TYPES(flag3)) .and. c_e(optio_/**/VOL7D_POLY_TYPES(flag3))) then 
  tot = tot +1
endif

qcsummaryflag/**/VOL7D_POLY_TYPES=(tot >= -1)

#endif


END FUNCTION qcsummaryflag/**/VOL7D_POLY_TYPES

