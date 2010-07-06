! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
#include "config.h"
!> \defgroup base Pacchetto libsim, libreria base.
!! La libreria base di libsim contiene moduli e classi di uso
!! generale in applicazioni scientifiche scritte in Fortran 90. Per
!! compilare e linkare programmi che fanno uso di questa libreria si
!! dovranno inserire gli appositi comandi \c USE nelle unità di
!! programma coinvolte e usare, in fase di compilazione, l'opzione
!! \c -I/usr/include e, in fase di linking, l'opzione
!! \c -lsim_base, presupponendo che l'installazione sia stata
!! fatta a livello di sistema.

!> \brief Definizione di costanti utili per dichiarare variabili di tipi
!! desiderati.
!! Questo modulo definisce delle costanti da usare nelle dichiarazioni
!! di variabili (tramite l'attributo \c KIND)
!! e nella conversione di costanti (tramite l'underscore \c _ ) per essere
!! sicuri di usare i tipi desiderati.
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

INTEGER, PARAMETER :: ptr_c = SIZEOF_PTR_C !< intero della dimensione di un puntatore C

END MODULE kinds
