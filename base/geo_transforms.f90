!> This module defines transformation/projection functions for
!! georeferenced data on the Sphere.
!!
!! Every transformation should be in the form of \c SUBROUTINE
!! proj_&lt;tranform&gt;(lon,lat,x,y...) for forward transformation
!! (lon/lat to x/y) and unproj_&lt;tranform&gt;(x,y,lon,lat...) for
!! the inverse one. Additional transformation parameters should be
!! passed as additional arguments to the subroutine call, if possible
!! being the same for forward and backward transformation. The
!! transformation subroutines should be \c ELEMENTAL in order to work
!! with scalars or arrays of any rank and size.
!!
!!\ingroup base
MODULE geo_transforms
USE doubleprecision_phys_const
IMPLICIT NONE
PRIVATE
PUBLIC proj_regular_ll, unproj_regular_ll, proj_rotated_ll, unproj_rotated_ll, &
 proj_stretched_ll, unproj_stretched_ll, proj_lambert, unproj_lambert, &
 proj_polar_stereographic, unproj_polar_stereographic

CONTAINS

ELEMENTAL SUBROUTINE proj_regular_ll(lon,lat,x,y)
DOUBLE PRECISION, INTENT(in)  :: lon,lat
DOUBLE PRECISION, INTENT(out) :: x,y

x = lon
y = lat

END SUBROUTINE proj_regular_ll

ELEMENTAL SUBROUTINE unproj_regular_ll(x,y,lon,lat)
DOUBLE PRECISION, INTENT(in) :: x,y
DOUBLE PRECISION, INTENT(out) :: lon,lat

lon = x
lat = y

END SUBROUTINE unproj_regular_ll


ELEMENTAL SUBROUTINE proj_rotated_ll(lon,lat,x,y, &
 longitude_south_pole, latitude_south_pole, angle_rotation)
DOUBLE PRECISION, INTENT(in) :: lon,lat
DOUBLE PRECISION, INTENT(out) :: x,y
DOUBLE PRECISION, INTENT(in) :: longitude_south_pole, latitude_south_pole, &
 angle_rotation

DOUBLE PRECISION :: cy0,sy0,rx,srx,crx,sy,cy,lpolosud


lpolosud=acos(-sin(degrad*latitude_south_pole))

rx = degrad*(lon - longitude_south_pole)
srx = sin(rx)
crx = cos(rx)

sy0 = sin(lpolosud)
cy0 = cos(lpolosud)

sy = sin(degrad*lat)
cy = cos(degrad*lat)

x = raddeg*atan2(cy*srx, cy0*cy*crx+sy0*sy)       
y = raddeg*asin(cy0*sy - sy0*cy*crx)

END SUBROUTINE proj_rotated_ll

ELEMENTAL SUBROUTINE unproj_rotated_ll(x,y,lon,lat,&
 longitude_south_pole, latitude_south_pole, angle_rotation)
DOUBLE PRECISION, INTENT(in) :: x,y
DOUBLE PRECISION, INTENT(out) :: lon,lat
DOUBLE PRECISION, INTENT(in) :: longitude_south_pole, latitude_south_pole, &
 angle_rotation

DOUBLE PRECISION :: cy0,sy0,lpolosud

lpolosud=acos(-sin(degrad*latitude_south_pole))

cy0 = cos(lpolosud)
sy0 = sin(lpolosud)

lat = raddeg*asin(sy0*cos(degrad*y)*cos(degrad*x)+cy0*sin(degrad*y))
lon = longitude_south_pole + &
 raddeg*asin(sin(degrad*x)*cos(degrad*y)/cos(degrad*lat))

END SUBROUTINE unproj_rotated_ll

! come usare il polo? ruotare e antiruotare?
ELEMENTAL SUBROUTINE proj_stretched_ll(lon,lat,x,y, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor)
DOUBLE PRECISION, INTENT(in) :: lon,lat
DOUBLE PRECISION, INTENT(out) :: x,y
DOUBLE PRECISION, INTENT(in) :: longitude_stretch_pole, latitude_stretch_pole, &
 stretch_factor

DOUBLE PRECISION :: csq

csq = stretch_factor**2
x = lon
y = raddeg*ASIN((1.0D0 - csq + (1.0D0 + csq)*SIN(degrad*lat)) / &
 (1.0D0 + csq + (1.0D0 - csq)*SIN(degrad*lat)))

END SUBROUTINE proj_stretched_ll

ELEMENTAL SUBROUTINE unproj_stretched_ll(x,y,lon,lat,&
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor)
DOUBLE PRECISION, INTENT(in) :: x,y
DOUBLE PRECISION, INTENT(out) :: lon,lat
DOUBLE PRECISION, INTENT(in) :: longitude_stretch_pole, latitude_stretch_pole, &
 stretch_factor

DOUBLE PRECISION :: csq

csq = stretch_factor**2
lon = x
! TODO verificare la formula inversa
lat = raddeg*ASIN((csq - 1.0D0 + (csq + 1.0D0)*SIN(degrad*y)) / &
 (csq + 1.0D0 + (csq - 1.0D0)*SIN(degrad*y)))

END SUBROUTINE unproj_stretched_ll


! Formulas and notation from:
! http://mathworld.wolfram.com/LambertConformalConicProjection.html
! http://en.wikipedia.org/wiki/Lambert_conformal_conic_projection
! http://fr.wikipedia.org/wiki/Projection_conique_conforme_de_Lambert
! with the following guess:
! projection is always polar, so reference latitude=+-90 according to
! projectionCenterFlag; reference longitude is LoV.
! how coordinates of south pole should be treated? Metview ignores them.
ELEMENTAL SUBROUTINE proj_lambert(lon,lat,x,y, &
 latin1, latin2, lov, lad, projection_center_flag)
DOUBLE PRECISION, INTENT(in) :: lon,lat
DOUBLE PRECISION, INTENT(out) :: x,y
DOUBLE PRECISION, INTENT(in) :: latin1, latin2, lov, lad
INTEGER, INTENT(in) :: projection_center_flag

DOUBLE PRECISION  :: n, f, ro0, ro, cs1, cs2, cs3, pollat, angle, cot
DOUBLE PRECISION, PARAMETER :: epsy = 1.0D-100

IF (IAND(projection_center_flag, 128) == 0) THEN
  pollat = 90.D0*degrad
ELSE
  pollat = -90.D0*degrad
ENDIF
cs1 = COS(degrad*latin1)
cs2 = TAN(pi*.25D0 + degrad*latin1*.5D0)

IF (latin1 == latin2) THEN
  n = SIN(degrad*latin1) ! verify that n->sin(latin1) when latin2->latin1
ELSE
  n = LOG(cs1/COS(degrad*latin2)) / &
   LOG(TAN(pi*.25D0 + degrad*latin2*.5D0) / cs2)
ENDIF
f = cs1*cs2**n/n*rearth ! check that rearth is correct here (only if lad==latin1?)
angle = pi*.25D0 + pollat*.5D0
cot = COS(angle)/SIN(angle)
IF (cot > epsy) THEN
  ro0 = f*cot**n
ELSE
  ro0 = 0.0D0
ENDIF

angle = pi*.25D0 + degrad*lat*.5D0
cot = COS(angle)/SIN(angle)
IF (cot > epsy) THEN
  ro = f*cot**n
ELSE
  ro = 0.0D0
ENDIF

cs3 = degrad*n*(lon - lov)

x = ro*SIN(cs3)
y = ro0 - ro*COS(cs3)

END SUBROUTINE proj_lambert

ELEMENTAL SUBROUTINE unproj_lambert(x,y,lon,lat, &
 latin1, latin2, lov, lad, projection_center_flag)
DOUBLE PRECISION, INTENT(in) :: x,y
DOUBLE PRECISION, INTENT(out) :: lon,lat
DOUBLE PRECISION, INTENT(in) :: latin1, latin2, lov, lad
INTEGER, INTENT(in) :: projection_center_flag

DOUBLE PRECISION :: n, f, ro0, ro, theta, cs1, cs2, pollat, angle, cot
DOUBLE PRECISION, PARAMETER :: epsy = 1.0D-100

! check, pollat is actually used as the latitude at which
! y=0, may be not correct and is not enough for Southern Hemisphere
IF (IAND(projection_center_flag, 128) == 0) THEN
  pollat = 90.D0*degrad
ELSE
  pollat = -90.D0*degrad
ENDIF
cs1 = COS(degrad*latin1)
cs2 = TAN(pi*.25D0 + degrad*latin1*.5D0)

IF (latin1 == latin2) THEN
  n = SIN(degrad*latin1) ! verify limit
ELSE
  n = LOG(cs1/COS(degrad*latin2)) / &
   LOG(TAN(pi*.25D0 + degrad*latin2*.5D0) / cs2)
ENDIF
f = cs1*cs2**n/n*rearth ! check that rearth is correct here (only if lad==latin1?)
angle = pi*.25D0 + pollat*.5D0
cot = COS(angle)/SIN(angle)
IF (cot > epsy) THEN
  ro0 = f*cot**n
ELSE
  ro0 = 0.0D0
ENDIF

ro = SIGN(SQRT(x*x + (ro0-y)*(ro0-y)), n) ! check SIGN
theta = raddeg*ATAN2(x, ro0-y)

lon = lov + theta/n
lat = raddeg*(2.D0*ATAN((f/ro)**(1.D0/n)) - pi*.5D0)

END SUBROUTINE unproj_lambert


!http://mathworld.wolfram.com/StereographicProjection.html
ELEMENTAL SUBROUTINE proj_polar_stereographic(lon,lat,x,y, &
 lov, lad, projection_center_flag)
DOUBLE PRECISION, INTENT(in) :: lon,lat
DOUBLE PRECISION, INTENT(out) :: x,y
DOUBLE PRECISION, INTENT(in) :: lov, lad
INTEGER, INTENT(in) :: projection_center_flag

DOUBLE PRECISION  :: k, pollat

IF (IAND(projection_center_flag, 128) == 0) THEN
  pollat = 90.D0*degrad
ELSE
  pollat = -90.D0*degrad
ENDIF

k = 2.0D0*rearth/(1.0D0 + SIN(pollat)*SIN(degrad*lat) + &
 COS(pollat)*COS(degrad*lat)*COS(degrad*(lon - lov)))
x = k*COS(degrad*lat)*SIN(degrad*(lon - lov))
y = k*(COS(pollat)*SIN(degrad*lat) - &
 SIN(pollat)*COS(degrad*lat)*COS(degrad*(lon - lov)))

END SUBROUTINE proj_polar_stereographic

ELEMENTAL SUBROUTINE unproj_polar_stereographic(x,y,lon,lat, &
 lov, lad, projection_center_flag)
DOUBLE PRECISION, INTENT(in) :: x,y
DOUBLE PRECISION, INTENT(out) :: lon,lat
DOUBLE PRECISION, INTENT(in) :: lov, lad
INTEGER, INTENT(in) :: projection_center_flag

DOUBLE PRECISION  :: ro, c, pollat

IF (IAND(projection_center_flag, 128) == 0) THEN
  pollat = 90.D0*degrad
ELSE
  pollat = -90.D0*degrad
ENDIF

ro = SQRT(x**2 + y**2)
c = 2.0D0*ATAN(ro/(2.0D0*rearth))
lat = raddeg*ASIN(COS(c)*SIN(pollat)+y*SIN(c)*COS(pollat)/ro)
lon = lov + raddeg*ATAN2(x*SIN(c), &
 (ro*COS(pollat)*COS(c)-y*SIN(pollat)*SIN(c)))

END SUBROUTINE unproj_polar_stereographic

END MODULE geo_transforms
