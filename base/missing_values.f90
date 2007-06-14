MODULE missing_values
USE kinds
IMPLICIT NONE

!omstart missing_values
!idx Costanti fortran per i valori mancanti
!Questo modulo definisce le seguenti costanti da usare come
!valori mancanti:
!
!rmiss  => reale di default
!smiss  => reale a singola precisione (kind=fp_s)
!dmiss  => reale a doppia precisione (kind=fp_d)
!imiss  => intero di default
!ibmiss => intero ad un byte (kind=int_b)
!ismiss => intero a 2 byte (kind=int_s)
!ilmiss => intero a 4 byte (kind=int_l)
!cmiss  => carattere
!
!Fa uso dei tipi definiti dal modulo kinds
!
!Definisce le seguenti procedure:
!
!LOGICAL FUNCTION c_e(var)
!
!Restitiusce .TRUE. se var e` valida e .FALSE. se e` mancante;
!e` richiamabile per tutti i tipi definiti sopra
!
!vedi kinds
!omend

REAL, PARAMETER :: rmiss = HUGE(1.0)
REAL(kind=fp_s), PARAMETER :: smiss = HUGE(1.0_fp_s)
REAL(kind=fp_d), PARAMETER :: dmiss = HUGE(1.0_fp_d)
INTEGER, PARAMETER :: imiss = HUGE(0)
INTEGER(kind=int_b), PARAMETER :: ibmiss = HUGE(0_int_b)
INTEGER(kind=int_s), PARAMETER :: ismiss = HUGE(0_int_s)
INTEGER(kind=int_l), PARAMETER :: ilmiss = HUGE(0_int_l)
CHARACTER(len=1), PARAMETER :: cmiss = char(0)

INTERFACE c_e
  MODULE PROCEDURE c_e_b, c_e_s, c_e_l, c_e_r, c_e_d, c_e_c
END INTERFACE

CONTAINS

LOGICAL FUNCTION c_e_b(var)
INTEGER(kind=int_b)  :: var

c_e_b=.TRUE.
IF (var == ibmiss)c_e_b= .FALSE.
RETURN
END FUNCTION c_e_b


LOGICAL FUNCTION c_e_s(var)
INTEGER (kind=int_s) ::  var

c_e_s=.TRUE.
IF (var == ismiss)c_e_s= .FALSE.
RETURN
END FUNCTION c_e_s


LOGICAL FUNCTION c_e_l(var)
INTEGER (kind=int_l) :: var

c_e_l=.TRUE.
IF (var == ilmiss)c_e_l= .FALSE.
RETURN
END FUNCTION c_e_l


LOGICAL FUNCTION c_e_r(var)
REAL :: var

c_e_r=.TRUE.
IF (var == rmiss)c_e_r= .FALSE.
RETURN
END FUNCTION c_e_r


LOGICAL FUNCTION c_e_d(var)
REAL (kind=fp_d) ::  var

c_e_d=.TRUE.
IF (var == dmiss)c_e_d= .FALSE.
RETURN
END FUNCTION c_e_d


LOGICAL FUNCTION c_e_c(var)
CHARACTER (len=*) var

c_e_c=.TRUE.
IF (var == cmiss)c_e_c=.FALSE.
RETURN

END FUNCTION c_e_c


END MODULE missing_values
