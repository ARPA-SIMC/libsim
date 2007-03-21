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
!vedi kinds
!omend

REAL, PARAMETER :: rmiss = HUGE(1.0)
REAL(kind=fp_s), PARAMETER :: smiss = HUGE(1.0_fp_s)
REAL(kind=fp_d), PARAMETER :: dmiss = HUGE(1.0_fp_d)
INTEGER, PARAMETER :: imiss = HUGE(0)
INTEGER(kind=int_b), PARAMETER :: ibmiss = HUGE(0_int_b)
INTEGER(kind=int_s), PARAMETER :: ismiss = HUGE(0_int_s)
INTEGER(kind=int_l), PARAMETER :: ilmiss = HUGE(0_int_l)
CHARACTER(len=1), PARAMETER :: cmiss = ' '

END MODULE missing_values
