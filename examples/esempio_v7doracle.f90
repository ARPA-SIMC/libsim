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
PROGRAM v7doracle
! Programma di esempio di estrazione dall'archivio Oracle del SIM
USE datetime_class
USE vol7d_class
USE vol7d_oraclesim_class

IMPLICIT NONE

TYPE(vol7d_oraclesim) :: db_v7d
TYPE(vol7d_network) :: network
TYPE(vol7d_timerange) :: timerange
TYPE(datetime) :: ti, tf
CHARACTER(len=12) :: c
INTEGER :: i, n
REAL, POINTER :: vol2d(:,:)

! Definisco le date iniziale e finale
CALL init(ti, year=2007, month=3, day=18, hour=12)
CALL init(tf, year=2007, month=3, day=21, hour=00)
! Definisco le reti da cui voglio estrarre
CALL init(network, 'FIDUPO')
! Definisco il timerange desiderato
CALL init(timerange, 1, 0, 1800)
! Chiamo il costruttore della classe vol7d_oraclesim per il mio oggetto
CALL init(db_v7d)
! Importo i dati, variabile 'B13011' della btable (precipitazione),
! rete 18 (FIDUPO)
CALL import(db_v7d, (/'B13011'/), (/network/), ti, tf, timerange=timerange)
! Creo una vista su un array bidimensionale che scorre le dimensioni
! dell'anagrafica e del tempo (vol7d_ana_d, vol7d_time_d)
CALL vol7d_get_voldatir(db_v7d%vol7d, (/vol7d_ana_d,vol7d_time_d/), vol2dp=vol2d)
! Calcolo la media e la stampo assieme all'istante
DO i = 1, SIZE(db_v7d%vol7d%time)
  CALL getval(db_v7d%vol7d%time(i), simpledate=c)
  n = COUNT (vol2d(:,i) /= rmiss)
  IF (n > 0) THEN
    PRINT*, c, ' prec. media:', SUM(vol2d(:,i), mask=(vol2d(:,i) /= rmiss))/n,n
  ENDIF
ENDDO

END PROGRAM v7doracle
