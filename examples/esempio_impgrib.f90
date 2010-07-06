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
PROGRAM impgrib_ex
USE impgrib_class
IMPLICIT NONE

INTEGER :: i, j, k, uw, ier, iargc
TYPE (impgrib) :: ig
CHARACTER (LEN=512) :: filename


CALL init(ig)
!!$ig%fatal=.TRUE.
ig%verbose=.TRUE.
CALL getarg(1, filename)
CALL readnaml(ig, filename, ier)
!!$CALL pbopen(uw, 'ppp.grb', 'w', ier)
!!$PRINT*,uw
!!$CALL pbopen(uw, 'out.grb', 'w', ier)
!!$IF (ier /= 0) STOP 2
!!$PRINT*,uw

DO i = 2,iargc()
  CALL getarg(i, filename)
!  CALL readtl(ig, filename, ier, const = i==2, mode = 3)
  CALL readtl(ig, filename, ier, mode = 3)
  PRINT'(A)','--------------'
  PRINT'(''|'',3I4,''|'')',ig%tt(:)
  PRINT'(A)','--------------'
  PRINT'(13L)',ig%found4d
!!$  CALL putgribdata(ig%rg, uw, ier)
  IF (ier /= 0) STOP 3
  DO j = 1, ig%nvar
    IF (ig%vt(j)%cumulate /= 0) THEN
      DO k = 1, SIZE(ig%vt(j)%gblevv) ! nlev
        PRINT*,MAXVAL(ig%rg%grid%field5d(:,:,k,j,i-1))
!         SUM(ig%rg%grid%field5d(:,:,k,j,1))/SIZE(ig%rg%grid%field5d(:,:,k,j,1))
      ENDDO
    ENDIF
  ENDDO
ENDDO
CALL readtl(ig, filename, ier, mode = 4)
DO i = 1, SIZE(ig%tt)
  DO j = 1, ig%nvar
    IF (ig%vt(j)%cumulate /= 0) THEN
      DO k = 1, SIZE(ig%vt(j)%gblevv) ! nlev
        PRINT*,MAXVAL(ig%rg%grid%field5d(:,:,k,j,i))
!         SUM(ig%rg%grid%field5d(:,:,k,j,1))/SIZE(ig%rg%grid%field5d(:,:,k,j,1))
      ENDDO
    ENDIF
  ENDDO
ENDDO
END PROGRAM impgrib_ex
  
