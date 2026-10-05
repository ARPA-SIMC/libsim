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
! Programma di test per il module char_utilities
! migliorare a piacimento
PROGRAM termolib_test
USE missing_values
USE termolib
IMPLICIT NONE

INTEGER :: i, j, k
REAL :: t, tc, p, q, rh, rhc, td, tdc, twb, e
LOGICAL :: err

err = .FALSE.
PRINT*,'=== Testing termolib module ==='

PRINT*,'Checking rh'
DO k = 0, 10
  p = 10**(4.+REAL(k)/10.)
  DO i = 1, 75
    t = 200.+2.*REAL(i)
    DO j = 1, 100
      rh = REAL(j)
      q = relhumtoq(rh, p/100., t)
      rhc = qtorelhum(q, p, t)
      IF (c_e(rhc)) THEN
        IF (ABS(rh-rhc) > 0.01 .AND. q > 0) THEN
          PRINT*,'rh% err',ABS(rh-rhc),'at',p,t,rh,q
          err = .TRUE.
        ENDIF
      ELSE
!        PRINT*,'rh% miss',ABS(rh-rhc),'at',p,t,rh,q
      ENDIF
    ENDDO
  ENDDO
ENDDO

PRINT*,'Checking esat'
DO i = 1, 75
  t = 200.+2.*REAL(i)
  tc = tesat(esat(t))
  IF (c_e(rhc)) THEN
    IF (ABS(t-tc) > 0.0001) THEN
      PRINT*,'t err',ABS(t-tc),'at',t,esat(t)
      err = .TRUE.
    ENDIF
  ENDIF
ENDDO

PRINT*,'Checking td'
DO k = 0, 10
  p = 10**(4.+REAL(k)/10.)
  DO i = 1, 75
    td = 200.+2.*REAL(i)
    q = q_tdp(td, p)
    tdc = td_pq(p/100.,q)
    IF (c_e(tdc)) THEN
      IF (ABS(td-tdc) > 0.2 .AND. q > 0) THEN
        PRINT*,'td err',ABS(td-tdc),'at',p,td,q
        err = .TRUE.
      ENDIF
    ENDIF
  ENDDO
ENDDO

! big errors especially at t<0C
!PRINT*,'Checking td2'
!DO k = 0, 10
!  p = 10**(4.+REAL(k)/10.)
!  DO i = 1, 60
!    t = 200.+2.*REAL(i)
!    DO j = 1, 100
!      rh = j
!      td = trug(rh, t)
!      q = relhumtoq(rh, p/100., t)
!      tdc = td_pq(p/100.,q)
!      IF (c_e(td) .AND. c_e(tdc)) THEN
!        IF (ABS(td-tdc) > 0.2 .AND. q > 0) THEN
!          PRINT*,'td2 err',ABS(td-tdc),'at',p,t,rh,q,td,tdc
!          err = .TRUE.
!        ENDIF
!      ENDIF
!    ENDDO
!  ENDDO
!ENDDO

PRINT*,'Checking tw'
DO k = 0, 10
  p = 10**(4.+REAL(k)/10.)
  DO i = 1, 20
    t = 274.+2.*REAL(i)
    DO j = 10, 100, 10
      q = relhumtoq(REAL(j), p/100., t)
      td = td_pq(p/100., q)
      twb = tw(td, t, p/100.)
! what can we check here?
    ENDDO
  ENDDO
ENDDO

IF (err) CALL EXIT(1)

END PROGRAM termolib_test
