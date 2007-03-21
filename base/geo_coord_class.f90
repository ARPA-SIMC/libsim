MODULE geo_coord_class
USE kinds
USE missing_values
USE phys_const
IMPLICIT NONE

INTEGER, PARAMETER :: geoprec=fp_s, utmprec=fp_d

TYPE geo_coord
  PRIVATE
  REAL(kind=geoprec) :: lon, lat
  REAL(kind=utmprec) :: utme, utmn
  LOGICAL :: geoce, utmce
  INTEGER :: fuso
  CHARACTER(LEN=20) :: elliss
END TYPE geo_coord

TYPE(geo_coord),PARAMETER :: geo_coord_miss= &
 geo_coord(rmiss,rmiss,dmiss,dmiss,.FALSE.,.FALSE.,imiss,cmiss)

PRIVATE geo_dist_latlon

INTERFACE init
  MODULE PROCEDURE geo_coord_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE geo_coord_delete
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE geo_coord_eq, geo_coord_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE geo_coord_ne, geo_coord_nesv
END INTERFACE

INTERFACE getlat
  MODULE PROCEDURE geo_coord_getlat
END INTERFACE

INTERFACE getlon
  MODULE PROCEDURE geo_coord_getlon
END INTERFACE

CONTAINS

SUBROUTINE geo_coord_init(this, lon, lat, utme, utmn, fuso, elliss)
TYPE(geo_coord) :: this
REAL(kind=geoprec), INTENT(IN), OPTIONAL :: lon, lat
REAL(kind=utmprec), INTENT(IN), OPTIONAL  :: utme, utmn
INTEGER, INTENT(IN), OPTIONAL  :: fuso
CHARACTER(LEN=20), INTENT(IN), OPTIONAL  :: elliss

IF (PRESENT(lon) .AND. PRESENT(lat)) THEN
  this%lon = lon
  this%lat = lat
  this%utme = rmiss
  this%utmn = rmiss
  this%fuso = imiss
  this%elliss = cmiss
  this%geoce = .TRUE.
  this%utmce = .FALSE.
ELSE IF (PRESENT(utme) .AND. PRESENT(utmn) .AND. &
 PRESENT(fuso) .AND. PRESENT(elliss)) THEN
  this%lon = rmiss
  this%lat = rmiss
  this%utme = utme
  this%utmn = utmn
  this%fuso = fuso
  this%elliss = elliss
  this%geoce = .FALSE.
  this%utmce = .TRUE.
ELSE
  this%geoce = .FALSE.
  this%utmce = .FALSE.
ENDIF

END SUBROUTINE geo_coord_init


SUBROUTINE geo_coord_delete(this)
TYPE(geo_coord) :: this

this%lon = rmiss
this%lat = rmiss
this%utme = rmiss
this%utmn = rmiss
this%fuso = imiss
this%elliss = cmiss
this%geoce = .FALSE.
this%utmce = .FALSE.


END SUBROUTINE geo_coord_delete

elemental FUNCTION geo_coord_eq(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%geoce .AND. that%geoce) THEN
  IF (this%lon == that%lon .AND. this%lat == that%lat) THEN
    res = .TRUE.
  ELSE
    res = .FALSE.
  ENDIF
ELSE IF (this%utmce .AND. that%utmce) THEN
  IF (this%utme == that%utme .AND. this%utmn == that%utmn &
   .AND. this%fuso == that%fuso .AND. this%elliss == that%elliss) THEN
    res = .TRUE.
  ELSE
    res = .FALSE.
  ENDIF
ELSE
! conversione...
ENDIF

END FUNCTION geo_coord_eq


FUNCTION geo_coord_eqsv(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION geo_coord_eqsv


elemental FUNCTION geo_coord_ne(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION geo_coord_ne


FUNCTION geo_coord_nesv(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION geo_coord_nesv


FUNCTION geo_coord_getlat(this) RESULT(lat)
TYPE(geo_coord) :: this
REAL(kind=geoprec) :: lat

lat = this%lat

END FUNCTION geo_coord_getlat


FUNCTION geo_coord_getlon(this) RESULT(lon)
TYPE(geo_coord) :: this
REAL(kind=geoprec) :: lon

lon = this%lon

END FUNCTION geo_coord_getlon


FUNCTION geo_coord_dist(this, that) RESULT(dist)
TYPE(geo_coord), INTENT (IN) :: this, that
REAL :: dist

IF (this%geoce .AND. that%geoce) THEN
  dist = geo_dist_latlon(lat1=this%lat, lon1=this%lon, lat2=that%lat, lon2=that%lon)
ELSE IF (this%utmce .AND. that%utmce) THEN
  !dist = geo_dist_utm(..)
ELSE
  ! conversione...
END IF

END FUNCTION geo_coord_dist


FUNCTION geo_coord_dentro(this, poly) RESULT(dentro)
TYPE(geo_coord), INTENT (IN) :: this, poly(:)
LOGICAL :: dentro

END FUNCTION geo_coord_dentro


FUNCTION geo_dist_latlon(lat1, lon1, lat2, lon2) RESULT(dist)
REAL (kind=geoprec), INTENT(IN) :: lat1, lat2, lon1, lon2
REAL :: dist
REAL,PARAMETER :: rpi = 3.1415927
REAL :: x,y

!dubbi matematici:
!conversione in radianti: mezzi più furbi?
!pigreco: costante già presente?

y = (lat1-lat2)*degrad*rearth
x = (lon1-lon2)*degrad*rearth*COS(((lat1+lat2)/2.)*degrad)
dist=SQRT(x**2+y**2)

END FUNCTION geo_dist_latlon


END MODULE geo_coord_class
