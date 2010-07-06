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
!> \brief Definizione di costanti utili per le unità di I/O
!!
!! Questo modulo definisce delle costanti che associano
!! le unità di Input/Output standard UNIX alle unità di
!! Input/Output del Fortran; si utilizzano tipicamente nei comandi
!! READ, WRITE, INQUIRE. 
!! Esempio tipico di utilizzo:
!! \code
!! USE io_units
!!
!! WRITE(stout_unit,*)'Dimmi qualcosa di carino'
!! READ(stdin_unit,*)mesg
!! IF (mesg == 'scemo') THEN
!!   WRITE(stderr_unit,*)'Mascalzone!'
!!   STOP
!! ENDIF
!! ...
!! \endcode
!! \ingroup base
MODULE io_units
IMPLICIT NONE

! Da condizionare con #ifdef ?!
INTEGER, PARAMETER :: stderr_unit = 0 !< standard error
INTEGER, PARAMETER :: stdin_unit = 5 !< standard input
INTEGER, PARAMETER :: stdout_unit = 6 !< standard output

#ifdef F2003_FEATURES
CHARACTER(len=6), PARAMETER :: stream_if_possible='STREAM'
#else
CHARACTER(len=10), PARAMETER :: stream_if_possible='SEQUENTIAL'
#endif

END MODULE io_units
