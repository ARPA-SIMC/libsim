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
PROGRAM esempio_vol7d_compute
USE datetime_class
USE vol7d_class
USE vol7d_class_compute
IMPLICIT NONE

TYPE(vol7d) :: v7d1, v7d2, v7d3, v7dc, v7dl, v7dr
TYPE(datetime) :: enddate
DOUBLE PRECISION, POINTER :: vol1dd(:)
REAL, POINTER :: vol1dr(:), vol2dr(:,:)
INTEGER, POINTER :: vol1di(:)
INTEGER :: i, n1 = 50, n2 = 2

! prepara la strada a v7d
CALL init (v7d1)

! alloca i descrittori del volume, chiamabile piu' volte se con parametri diversi
CALL vol7d_alloc(v7d1, ndativarr=1, ntime=n1)

! alloca il volume dati v7d
CALL vol7d_alloc_vol(v7d1)

! ho detto che c'è una variabile dati reale (ndativarr=1) e la definisco
CALL init(v7d1%dativar%r(1), btable='B12101')

! anagrafica
CALL init(v7d1%ana(1), lon=11.5_fp_geo, lat=44.5_fp_geo)

! livello
CALL init(v7d1%level(1), level1=103, l1=2)

! scadenza
CALL init(v7d1%timerange(1), timerange=254, p1=0, p2=0)

! rete
CALL init(v7d1%network(1), name='generic')

DO i=1,n1
  ! ci scrivo le date di riferimento
  v7d1%time(i) = datetime_new(year=2007, month=3, day=1, hour=i-1)
  ! ci scrivo il dato reale
  v7d1%voldatir(1,i,1,1,1,1)=295.4+0.5*i
END DO

CALL vol7d_compute_stat_proc_agg(v7d1, v7d2, 0, &
 timedelta_new(hour=6), start=datetime_new(year=2007, month=3, day=1))
PRINT*,v7d1%voldatir
PRINT*,'Volume averaged unweighted'
IF (ASSOCIATED(v7d2%voldatir)) PRINT*,v7d2%voldatir
CALL delete(v7d2)

CALL vol7d_compute_stat_proc_agg(v7d1, v7d2, 0, &
 timedelta_new(hour=6), start=datetime_new(year=2007, month=3, day=1), &
 weighted=.TRUE.)
PRINT*,v7d1%voldatir
PRINT*,'Volume averaged weighted'
IF (ASSOCIATED(v7d2%voldatir)) PRINT*,v7d2%voldatir

CALL vol7d_compute_stat_proc_agg(v7d1, v7d2, 2, &
 timedelta_new(hour=6), start=datetime_new(year=2007, month=3, day=1), &
 weighted=.TRUE.)
PRINT*,v7d1%voldatir
PRINT*,'Volume maximum'
IF (ASSOCIATED(v7d2%voldatir)) PRINT*,v7d2%voldatir

CALL vol7d_compute_stat_proc_agg(v7d1, v7d2, 3, &
 timedelta_new(hour=6), start=datetime_new(year=2007, month=3, day=1), &
 weighted=.TRUE.)
PRINT*,v7d1%voldatir
PRINT*,'Volume minimum'
IF (ASSOCIATED(v7d2%voldatir)) PRINT*,v7d2%voldatir

END PROGRAM esempio_vol7d_compute
