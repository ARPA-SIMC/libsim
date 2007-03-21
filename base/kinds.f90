MODULE kinds
IMPLICIT NONE

!omstart kinds
!Questo modulo definisce le seguenti costanti da usare
!nelle definizioni di variabili e nella conversione di costanti
!per essere sicuri di usare i tipi desiderati:
!
!fp_s   => reale a singola precisione (4 byte IEEE)
!fp_d   => reale a doppia precisione (8 byte IEEE)
!fp_d   => reale a quadrupla precisione (8 byte IEEE, se esiste)
!int_b  => intero a 1 byte (byte)
!int_s  => intero a 2 byte (short)
!int_l  => intero a 4 byte (long)
!int_ll => intero a 8 byte (long long, se esiste)
!ptr_c  => intero della dimensione di un puntatore C
!
!Esempio di utilizzo:
!USE kinds
!...
!INTEGER(kind=int_b) :: ab, bb
!ab = 13_int_b
!
!omend

PRIVATE

INTEGER, PARAMETER, PUBLIC :: &
 int_b    = SELECTED_INT_KIND(1), & ! Byte  integer
 int_s    = SELECTED_INT_KIND(4), & ! Short integer
 int_l    = SELECTED_INT_KIND(8)    ! Long  integer
INTEGER, PARAMETER :: &
 int_ll_t = SELECTED_INT_KIND(16)  ! LLong integer
INTEGER, PARAMETER, PUBLIC :: &
 int_ll   = ( ( ( 1 + SIGN( 1, int_ll_t ) ) / 2 ) * int_ll_t ) + &
 ( ( ( 1 - SIGN( 1, int_ll_t ) ) / 2 ) * int_l    )

INTEGER, PARAMETER, PUBLIC :: &
 fp_s = SELECTED_REAL_KIND(6), & ! Single precision
 fp_d = SELECTED_REAL_KIND(15)   ! Double precision
INTEGER, PARAMETER :: &
 fp_q_t = SELECTED_REAL_KIND(20) ! Quad precision
INTEGER, PARAMETER, PUBLIC  :: &
 fp_q   = ( ( ( 1 + SIGN( 1, fp_q_t ) ) / 2 ) * fp_q_t ) + &
 ( ( ( 1 - SIGN( 1, fp_q_t ) ) / 2 ) * fp_d )

! Da condizionare con #ifdef ?!
INTEGER, PARAMETER, PUBLIC :: &
 ptr_c = int_l ! C pointer

END MODULE kinds
