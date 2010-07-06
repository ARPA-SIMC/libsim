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
MODULE geo_grid_class

IMPLICIT NONE

REAL, PARAMETER :: gg_rmd=-1.E15, gg_rmdc=gg_rmd/10.
INTEGER, PARAMETER :: &
 gg_firstlon = 1, &
 gg_lastlon = 2,  &
 gg_firstlat = 3, &
 gg_lastlat = 4,  &
 gg_loninc = 5,   &
 gg_latinc = 6,   &
 gg_lonrot = 7,   &
 gg_latrot = 8,   &
 gg_rot = 9,      &
 gg_lonstr = 10,  &
 gg_latstr = 11,  &
 gg_str = 12

INTEGER, PARAMETER :: &
 gg_ija = 1,      &
 gg_drt = 2,      &
 gg_nx = 3,       &
 gg_ny = 4,       &
 gg_npar = 5

TYPE geo_grid
  INTEGER :: ija, drt, nx, ny, npar
  REAL :: firstlon, lastlon, firstlat, lastlat, loninc, latinc, &
   lonrot, latrot, rot, lonstr, latstr, str
  INTEGER :: nlev, nvar, ntim, curlev, curvar, curtim
  REAL, POINTER :: vcp(:)
  REAL, POINTER :: field2d(:,:), field5d(:,:,:,:,:)
END TYPE geo_grid

INTERFACE init
  MODULE PROCEDURE gg_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE gg_delete
END INTERFACE

INTERFACE alloc
  MODULE PROCEDURE gg_alloc
END INTERFACE

INTERFACE setsize
  MODULE PROCEDURE gg_setsize
END INTERFACE

INTERFACE setelement
  MODULE PROCEDURE gg_setelement
END INTERFACE

INTERFACE getival
  MODULE PROCEDURE gg_getival
END INTERFACE

INTERFACE getrval
  MODULE PROCEDURE gg_getrval
END INTERFACE

INTERFACE setval
  MODULE PROCEDURE gg_setival, gg_setrval
END INTERFACE

INTERFACE getfield5d
  MODULE PROCEDURE gg_getfield5d
END INTERFACE

INTERFACE getfield2d
  MODULE PROCEDURE gg_getfield2d
END INTERFACE

INTERFACE filter5p
  MODULE PROCEDURE gg_filter5p
END INTERFACE

INTERFACE rescale
  MODULE PROCEDURE gg_rescale
END INTERFACE

INTERFACE cut
  MODULE PROCEDURE gg_cut
END INTERFACE

INTERFACE regrid
  MODULE PROCEDURE gg_regrid
END INTERFACE

CONTAINS

SUBROUTINE gg_init(this)
TYPE(geo_grid) :: this

this%ija = 0
this%drt = -1
this%nx = 0
this%ny = 0
this%npar = 0
this%nlev = 1
this%nvar = 1
this%ntim = 1
this%curlev = 1
this%curvar = 1
this%curtim = 1
NULLIFY(this%vcp, this%field2d, this%field5d)

END SUBROUTINE gg_init


SUBROUTINE gg_delete(this)
TYPE (geo_grid) :: this

IF (ASSOCIATED(this%field5d)) DEALLOCATE(this%field5d)
NULLIFY(this%field2d)
IF (ASSOCIATED(this%vcp)) DEALLOCATE(this%vcp)

END SUBROUTINE gg_delete


SUBROUTINE gg_alloc(this, ier)
TYPE (geo_grid) :: this
INTEGER, INTENT(OUT) :: ier

ier = 0
IF (ASSOCIATED(this%field5d)) RETURN
IF (this%nx <= 0 .OR. this%ny <= 0) THEN
  PRINT*,'Error, too soon to allocate'
  ier = 1
  RETURN
ENDIF
ALLOCATE(this%field5d(this%nx, this%ny, this%nlev, this%nvar, this%ntim), STAT=ier)
IF (ier /= 0) THEN
  WRITE(*,'(A,I8,A)')'Error, cannot allocate ', &
   this%nx*this%ny*this%nlev*this%nvar*this%ntim,' words'
!  IF (this%fatal) STOP 1
  RETURN
ENDIF
CALL setelement(this) ! Assign field2d

END SUBROUTINE gg_alloc


SUBROUTINE gg_setsize(this, nlev, nvar, ntim, ier)
TYPE (geo_grid) :: this
INTEGER, INTENT(IN), OPTIONAL :: nlev, nvar, ntim
INTEGER, INTENT(OUT), OPTIONAL :: ier

INTEGER :: ler, sh(5)
REAL, POINTER :: tmpptr(:,:,:,:,:)

ler = 0
IF (PRESENT(nlev)) THEN
  IF (nlev > 0) THEN
    this%nlev = nlev
  ELSE
    ler = 1
  ENDIF
ENDIF
IF (PRESENT(nvar)) THEN
  IF (nvar > 0) THEN
    this%nvar = nvar
  ELSE
    ler = 1
  ENDIF
ENDIF
IF (PRESENT(ntim)) THEN
  IF (ntim > 0) THEN
    this%ntim = ntim
  ELSE
    ler = 1
  ENDIF
ENDIF
IF (ASSOCIATED(this%field5d)) THEN
  tmpptr => this%field5d
  CALL gg_alloc(this, ler)
  IF (ler == 0) THEN
    this%field5d = gg_rmd
    sh = SHAPE(tmpptr)
    this%field5d(:,:,1:sh(3),1:sh(4),1:sh(5)) = tmpptr(:,:,:,:,:)
  ENDIF
  DEALLOCATE(tmpptr)
ENDIF
IF (PRESENT(ier)) ier = ler

END SUBROUTINE gg_setsize


SUBROUTINE gg_setelement(this, nlev, nvar, ntim, ier)
TYPE (geo_grid) :: this
INTEGER, INTENT(IN), OPTIONAL :: nlev, nvar, ntim
INTEGER, INTENT(OUT), OPTIONAL :: ier

INTEGER :: ler

IF (PRESENT(nlev)) THEN
  IF (nlev > 0 .AND. nlev <= this%nlev) THEN
    this%curlev = nlev
  ELSE
    ler = 1
  ENDIF
ENDIF
IF (PRESENT(nvar)) THEN
  IF (nvar > 0 .AND. nvar <= this%nvar) THEN
    this%curvar = nvar
  ELSE
    ler = 1
  ENDIF
ENDIF
IF (PRESENT(ntim)) THEN
  IF (ntim > 0 .AND. ntim <= this%ntim) THEN
    this%curtim = ntim
  ELSE
    ler = 1
  ENDIF
ENDIF
IF (ASSOCIATED(this%field5d)) &
 this%field2d => this%field5d(:,:,this%curlev,this%curvar,this%curtim)
IF (PRESENT(ier)) ier = ler

END SUBROUTINE gg_setelement


INTEGER FUNCTION gg_getival(this, what) RESULT(gg_getval)
TYPE(geo_grid), INTENT(IN) :: this
INTEGER, INTENT(IN) :: what

SELECT CASE(what)
CASE(gg_ija)
  gg_getval = this%ija
CASE(gg_drt)
  gg_getval = this%drt
CASE(gg_nx)
  gg_getval = this%nx
CASE(gg_ny)
  gg_getval = this%ny
CASE(gg_npar)
  gg_getval = this%npar
END SELECT

END FUNCTION gg_getival


REAL FUNCTION gg_getrval(this, what) RESULT(gg_getval)
TYPE(geo_grid), INTENT(IN) :: this
INTEGER, INTENT(IN) :: what

SELECT CASE(what)
CASE(gg_firstlon)
  gg_getval = this%firstlon
CASE(gg_lastlon)
  gg_getval = this%lastlon
CASE(gg_firstlat)
  gg_getval = this%firstlat
CASE(gg_lastlat)
  gg_getval = this%lastlat
CASE(gg_loninc)
  gg_getval = this%loninc
CASE(gg_latinc)
  gg_getval = this%latinc
CASE(gg_lonrot)
  gg_getval = this%lonrot
CASE(gg_latrot)
  gg_getval = this%latrot
CASE(gg_rot)
  gg_getval = this%rot
END SELECT

END FUNCTION gg_getrval


SUBROUTINE gg_setival(this, what, val)
TYPE(geo_grid), INTENT(OUT) :: this
INTEGER, INTENT(IN) :: what
INTEGER :: val

SELECT CASE(what)
CASE(gg_ija)
  this%ija = val
CASE(gg_drt)
  this%drt = val
CASE(gg_nx)
  this%nx = val
CASE(gg_ny)
  this%ny = val
CASE(gg_npar)
  this%npar = val
END SELECT

END SUBROUTINE gg_setival


SUBROUTINE gg_setrval(this, what, val)
TYPE(geo_grid), INTENT(OUT) :: this
INTEGER, INTENT(IN) :: what
REAL :: val

SELECT CASE(what)
CASE(gg_firstlon)
  this%firstlon = val
CASE(gg_lastlon)
  this%lastlon = val
CASE(gg_firstlat)
  this%firstlat = val
CASE(gg_lastlat)
  this%lastlat = val
CASE(gg_loninc)
  this%loninc = val
CASE(gg_latinc)
  this%latinc = val
CASE(gg_lonrot)
  this%lonrot = val
CASE(gg_latrot)
  this%latrot = val
CASE(gg_rot)
  this%rot = val
END SELECT

END SUBROUTINE gg_setrval


FUNCTION gg_getfield5d(this) RESULT(field)
TYPE(geo_grid), INTENT(IN) :: this

REAL, POINTER :: field(:,:,:,:,:)

field => this%field5d
END FUNCTION gg_getfield5d


FUNCTION gg_getfield2d(this) RESULT(field)
TYPE(geo_grid), INTENT(IN) :: this

REAL, POINTER :: field(:,:)

field => this%field2d
END FUNCTION gg_getfield2d


! Transformation methods acting only on one field at a time
! and not on grid description

SUBROUTINE gg_filter5p(this, times)
TYPE(geo_grid), INTENT(INOUT) :: this
INTEGER, INTENT(in), OPTIONAL :: times

INTEGER :: i, j, im1, ip1, jm1, jp1, l, ltimes, sh(2)
REAL, ALLOCATABLE :: tmpbuff(:,:)

IF (PRESENT(times)) THEN
  ltimes = times
ELSE
  ltimes = 1
ENDIF
IF (ASSOCIATED(this%field2d) .AND. ltimes > 0) THEN
  sh = SHAPE(this%field2d)
  ALLOCATE(tmpbuff(sh(1), sh(2)))

  DO l = 1, ltimes
    DO j = 1, sh(2)
      jm1=MAX(j-1,1)
      jp1=MIN(j+1,sh(2))
      DO i = 1, sh(1)
        im1=MAX(i-1,1)
        ip1=MIN(i+1,sh(1))
        tmpbuff(i,j) = 0.2*(this%field2d(i,j) + &
         this%field2d(im1,j) + &
         this%field2d(ip1,j) + &
         this%field2d(i,jm1) + &
         this%field2d(i,jp1))
      ENDDO
    ENDDO
  ENDDO
  this%field2d = tmpbuff

  DEALLOCATE(tmpbuff)
ENDIF

END SUBROUTINE gg_filter5p


SUBROUTINE gg_rescale(this, convm, convs)
TYPE(geo_grid), INTENT(INOUT) :: this
REAL, INTENT(in) :: convm, convs

IF (convm /= 1. .OR. convs /= 0.) THEN ! Convert
  WHERE(this%field2d(:,:) > gg_rmdc)
    this%field2d(:,:) = convs + &
     this%field2d(:,:)*convm
  END WHERE
ENDIF

END SUBROUTINE gg_rescale


! Transformation methods acting on the whole grid
! including grid description => involve reallocating field5d

SUBROUTINE gg_cut(this, i1, i2, j1, j2, ier)
TYPE(geo_grid), INTENT(INOUT) :: this
INTEGER, INTENT(IN) :: i1, i2 , j1, j2
INTEGER, INTENT(OUT) :: ier

INTEGER :: sh(5)
REAL, POINTER :: tmp5d(:,:,:,:,:)

IF (i1 < 0 .OR. i2 < 0 .OR. j1 < 0 .OR. j2 < 0) THEN
  PRINT'(A)','Error, invalid cut parameters'
  ier = 1
  RETURN
ENDIF
IF (i1+i2 > this%nx .OR. j1+j2 > this%ny) THEN
  PRINT'(A)','Error, cut bigger than actual size of area'
  ier = 1
  RETURN
ENDIF

sh = SHAPE(this%field5d)
IF (sh(1) /= this%nx .OR. sh(2) /= this%ny) THEN
  PRINT'(A)','Error, internal grid error'
  ier = 1
  RETURN
ENDIF
ALLOCATE(tmp5d(sh(1)-(i1+i2), sh(2)-(j1+j2), sh(3), sh(4), sh(5)))
tmp5d(:,:,:,:,:) = this%field5d(i1+1:sh(1)-i2, j1+1:sh(2)-j2, :, :, :)

DEALLOCATE(this%field5d)
this%field5d => tmp5d
! Adjust the grid description
this%nx = this%nx-(i1+i2)
this%ny = this%ny-(j1+j2)
this%firstlon = this%firstlon+i1*this%loninc
this%lastlon = this%lastlon-i2*this%loninc
this%firstlat = this%firstlat+j1*this%latinc
this%lastlat = this%lastlat-j2*this%latinc
CALL setelement(this) ! Assign field2d

END SUBROUTINE gg_cut


SUBROUTINE gg_regrid(this, ngx, ngy, ier)
TYPE(geo_grid), INTENT(INOUT) :: this
INTEGER, INTENT(IN) :: ngx, ngy
INTEGER, INTENT(OUT) :: ier

INTEGER :: i, j, ie, je, ii, jj, navg, l3, l4, l5
INTEGER :: sh(5)
REAL, POINTER :: tmp5d(:,:,:,:,:)

! Sanity checks
IF (ngx <= 0 .OR. ngy <= 0) THEN
  PRINT'(A,2I4,A)','Error, regridding values ', ngx, ngy, ' not valid'
  ier = 1
  RETURN
ENDIF
IF (ngx > this%nx .OR. ngy > this%ny) THEN
  PRINT'(A,2I4,A)','Error, regridding values ', ngx, ngy, ' too big'
  ier = 1
  RETURN
ENDIF
IF (ngx == 1 .AND. ngy == 1) RETURN ! Nothing to do

sh = SHAPE(this%field5d)
IF (sh(1) /= this%nx .OR. sh(2) /= this%ny) THEN
  PRINT'(A)','Error, internal grid error'
  ier = 1
  RETURN
ENDIF
ALLOCATE(tmp5d(sh(1)/ngx, sh(2)/ngy, sh(3), sh(4), sh(5)))

DO l5 = 1, sh(5)
  DO l4 = 1, sh(4)
    DO l3 = 1, sh(3)
      ii = 0
      jj = 0
      DO j = 1, this%ny-ngy+1, ngy
        je = j+ngy-1
        jj = jj+1
        DO i = 1, this%nx-ngx+1, ngx
          ie = i+ngx-1
          ii = ii+1
          navg = COUNT(this%field5d(i:ie,j:je,l3,l4,l5) > gg_rmdc)
          IF (navg > 0) THEN
            tmp5d(ii,jj,l3,l4,l5) = SUM(this%field5d(i:ie,j:je,l3,l4,l5), &
             MASK=(this%field5d(i:ie,j:je,l3,l4,l5) > gg_rmdc))/navg
          ELSE
            tmp5d(ii,jj,l3,l4,l5) = gg_rmd
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

DEALLOCATE(this%field5d)
this%field5d => tmp5d
! Adjust the grid description
this%firstlon = this%firstlon+(ngx-1)*0.5*this%loninc
this%firstlat = this%firstlat+(ngy-1)*0.5*this%latinc
this%nx = this%nx/ngx
this%ny = this%ny/ngy
this%loninc = this%loninc*ngx
this%latinc = this%latinc*ngy
this%lastlon = this%firstlon+(this%nx-1)*this%loninc
this%lastlat = this%firstlat+(this%ny-1)*this%latinc
CALL setelement(this) ! Assign field2d

END SUBROUTINE gg_regrid


END MODULE geo_grid_class
