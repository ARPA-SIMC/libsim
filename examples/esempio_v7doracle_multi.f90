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
USE file_utilities
USE vol7d_class
USE vol7d_class_compute
USE vol7d_oraclesim_class

IMPLICIT NONE

TYPE(vol7d_oraclesim) :: db_v7d
TYPE(vol7d_network) :: dummy_network, network(8)
TYPE(datetime) :: ti, tf, tc
TYPE(vol7d) :: vol_cumh, vol_cumd
TYPE(timedelta) :: dt_cum
CHARACTER(len=12) :: c
INTEGER :: i, j, n
REAL, POINTER :: vol2d(:,:), vol2d_cum(:,:)
CHARACTER(len=512) :: filesim
TYPE(geo_coordvect),POINTER :: macroa(:)
INTEGER, ALLOCATABLE :: in_macroa(:)

! Definisco le date iniziale e finale
CALL init(ti, year=2007, month=3, day=18, hour=00)
CALL init(tf, year=2007, month=3, day=21, hour=00)
! Definisco una rete fittizia in cui forzare i dati
CALL init(dummy_network, 'generic')
! Definisco le reti da cui voglio estrarre
CALL init(network(1), 'LOCALI')
!CALL init(network(2),'AGRMET')
CALL init(network(2), 'AGRMET')
CALL init(network(3), 'URBANE')
!CALL init(network(5), '17')
CALL init(network(4), 'FIDUPO')
CALL init(network(5), 'ICIRFE')
CALL init(network(6), 'SIMNBO')
CALL init(network(7), 'SIMNPR')
CALL init(network(8), 'SPDSRA')
! Chiamo il costruttore della classe vol7d_oraclesim per il mio oggetto
CALL init(db_v7d)
! Importo i dati, variabile 'B13011' della btable (precipitazione),
! per un vettore di reti mettendo tutto in un'unica rete
! richiedo anche la variabile di anagrafica 'B07001' (altezza stazione)
CALL import(db_v7d, (/'B13011'/), (/network/), &
 ti, tf, anavar=(/'B07001'/), set_network=dummy_network)

PRINT*,SHAPE(db_v7d%vol7d%voldatir)
! Cumulo i dati su intervalli orari
CALL init(dt_cum, hour=1)
CALL vol7d_recompute_stat_proc(db_v7d%vol7d, vol_cumh, 1, dt_cum)
!CALL vol7d_cumulate(db_v7d%vol7d, vol_cumh, dt_cum)
! Mi faccio dare una "vista" bidimensionale dei miei dati
CALL vol7d_get_voldatir(vol_cumh, (/vol7d_ana_d,vol7d_time_d/), vol2dp=vol2d_cum)
! Stampo la media su tutte le stazioni ora per ora
DO i = 1, SIZE(vol_cumh%time)
  CALL getval(vol_cumh%time(i), simpledate=c)
  n = COUNT (vol2d_cum(:,i) /= rmiss)
  IF (n > 0) THEN
    PRINT'(2A,G12.5,1X,I5)',c,' prec. media (mm): ', &
     SUM(vol2d_cum(:,i), mask=(vol2d_cum(:,i) /= rmiss))/n, n
  ENDIF
ENDDO

! Cumulo i dati su intervalli giornalieri
CALL init(dt_cum, day=1)
CALL init(tc, year=2007, month=3, day=17, hour=00)
CALL vol7d_recompute_stat_proc(vol_cumh, vol_cumd, 1, dt_cum, tc)
!CALL vol7d_cumulate(vol_cumh, vol_cumd, dt_cum, tc)
! Mi faccio dare una "vista" bidimensionale dei miei dati
CALL vol7d_get_voldatir(vol_cumd, (/vol7d_ana_d,vol7d_time_d/), vol2dp=vol2d_cum)

! Importo un file con le macroaree Emilia Romagna
filesim=get_package_filepath('polipciv4.dat', filetype_data)
CALL import(macroa, shpfilesim=filesim)
ALLOCATE(in_macroa(SIZE(vol_cumd%ana)))
in_macroa = 0
! Determino l'appartenenza delle stazioni alle macroaree
DO i = 1, SIZE(vol_cumd%ana)
  DO j = 1, SIZE(macroa)
    IF (geo_coord_inside(vol_cumd%ana(i)%coord, macroa(j))) THEN
      in_macroa(i) = j
      EXIT
    ENDIF
  ENDDO
ENDDO

! Stampo la media su tutte le stazioni di ogni macroarea giorno per giorno
DO i = 1, SIZE(vol_cumd%time)
  CALL getval(vol_cumd%time(i), simpledate=c)
  DO j = 1, SIZE(macroa)
    n = COUNT(vol2d_cum(:,i) /= rmiss .AND. in_macroa(:) == j)
    IF (n > 0) THEN
      PRINT'(2A,I3,A,G12.5)',c,' macroarea: ',j,' prec media (mm): ', &
       SUM(vol2d_cum(:,i), mask=(vol2d_cum(:,i) /= rmiss &
       .AND. in_macroa(:) == j))/n
    ENDIF
  ENDDO
ENDDO

! Stampo la quota media delle stazioni di ogni macroarea
i=firsttrue(vol_cumd%anavar%r(:)%btable == 'B07001')
IF (i > 0) THEN
  DO j = 1, SIZE(macroa)
    n = COUNT(vol_cumd%volanar(:,i,1) /= rmiss .AND. in_macroa(:) == j)
    IF (n > 0) THEN
      PRINT'(A,I3,A,G12.5)','macroarea: ',j,' altezza media (m): ', &
     SUM(vol_cumd%volanar(:,i,1), mask=(vol_cumd%volanar(:,i,1) /= rmiss &
     .AND. in_macroa(:) == j))/n
    ENDIF
  ENDDO
ENDIF

END PROGRAM v7doracle
