!> \defgroup base Pacchetto libsim, libreria base.
!! La libreria base del pacchetto libsim contiene moduli e classi di uso
!! generale in applicazioni scientifiche scritte in Fortran 90. Per
!! compilare e linkare programmi che fanno uso di questa libreria si
!! dovranno inserire gli appositi comandi \c USE nelle unità di
!! programma coinvolte e usare, in fase di compilazione, l'opzione
!! \c -I/usr/include e, in fase di linking, l'opzione
!! \c -lsim_base, presupponendo che l'installazione sia stata
!! fatta a livello di sistema.

!> \brief Definizione di costanti utili per KIND
!!
!! Questo modulo definisce delle costanti da usare nelle dichiarazioni
!! di variabili e nella conversione di costanti per essere sicuri di
!! usare i tipi desiderati.
!! Esempio tipico di utilizzo:
!! \code
!! USE kinds
!! ...
!! INTEGER(kind=int_b) :: ab, bb
!! REAL(kind=fp_d) :: dd
!! 
!! ab = 13_int_b
!! dd = REAL(ab, kind=fp_d)
!! ...
!! \endcode
!! \ingroup base
MODULE kinds
IMPLICIT NONE

INTEGER, PARAMETER :: int_b    = SELECTED_INT_KIND(1) !< intero a 1 byte (byte)
INTEGER, PARAMETER :: int_s    = SELECTED_INT_KIND(4) !< intero a 2 byte (short)
INTEGER, PARAMETER :: int_l    = SELECTED_INT_KIND(8) !< intero a 4 byte (long)
INTEGER, PARAMETER, PRIVATE :: &
 int_ll_t = SELECTED_INT_KIND(16)
!> intero a 8 byte (long long, se supportato)
INTEGER, PARAMETER :: int_ll = &
 ( ( ( 1 + SIGN( 1, int_ll_t ) ) / 2 ) * int_ll_t ) + &
 ( ( ( 1 - SIGN( 1, int_ll_t ) ) / 2 ) * int_l    )

INTEGER, PARAMETER :: fp_s = SELECTED_REAL_KIND(6) !< reale a singola precisione (4 byte IEEE)
INTEGER, PARAMETER :: fp_d = SELECTED_REAL_KIND(15) !< reale a doppia precisione (8 byte IEEE)
INTEGER, PARAMETER, PRIVATE :: fp_q_t = SELECTED_REAL_KIND(20)
!> reale a quadrupla precisione (16 byte IEEE, se supportato)
INTEGER, PARAMETER :: fp_q = &
 ( ( ( 1 + SIGN( 1, fp_q_t ) ) / 2 ) * fp_q_t ) + &
 ( ( ( 1 - SIGN( 1, fp_q_t ) ) / 2 ) * fp_d )

! Da condizionare con #ifdef !
INTEGER, PARAMETER :: ptr_c = int_l !< intero della dimensione di un puntatore C

END MODULE kinds
