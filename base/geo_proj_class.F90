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
MODULE geo_proj_class
USE doubleprecision_phys_const
USE missing_values
USE char_utilities
USE optional_values
IMPLICIT NONE

TYPE geo_proj_polar
  DOUBLE PRECISION :: latin1, latin2 ! latitudes at which the projection plane intesects the sphere
  DOUBLE PRECISION :: lad ! latitudine at which dx and dy (in m) are specified
  DOUBLE PRECISION :: lon1, lat1 ! stored in order not to forget them, from grib
  INTEGER :: projection_center_flag ! 0 = northern hemisphere, 128 = southern hemisphere
END TYPE geo_proj_polar

TYPE geo_proj_rotated
  DOUBLE PRECISION :: longitude_south_pole, latitude_south_pole, angle_rotation
END TYPE geo_proj_rotated

TYPE geo_proj_stretched
  DOUBLE PRECISION :: latitude_stretch_pole, longitude_stretch_pole, stretch_factor
END TYPE geo_proj_stretched

TYPE geo_proj_ellips
  PRIVATE
  DOUBLE PRECISION :: rf, a ! inverse of flattening and semi-major axis
  DOUBLE PRECISION :: f, e2, e1, ep2, e11, e12, e13, e14, e4, e6, ef0, ef1, ef2, ef3, k0 ! computed parameters 
END TYPE geo_proj_ellips

TYPE geo_proj
  CHARACTER(len=80) :: proj_type=cmiss ! the projection type
  DOUBLE PRECISION :: xoff, yoff ! offsets in x and y wrt origin, aka false easting and northing resp.
  DOUBLE PRECISION :: lov ! line of view (or central meridian, reference longitude, orientation of the grid)
  TYPE(geo_proj_rotated) :: rotated
  TYPE(geo_proj_stretched) :: stretched
  TYPE(geo_proj_polar) :: polar
!TYPE (geo_proj_equatorial) :: equatorial ! For projections like Mercator?
  TYPE(geo_proj_ellips) :: ellips
END TYPE geo_proj


!> Destructors of the corresponding objects.
INTERFACE delete
  MODULE PROCEDURE geo_proj_delete
END INTERFACE

!> Copy an object, creating a fully new instance.
INTERFACE copy
  MODULE PROCEDURE geo_proj_copy
END INTERFACE

!> Method for returning the contents of the object.
INTERFACE get_val
  MODULE PROCEDURE geo_proj_get_val
END INTERFACE

!> Method for setting the contents of the object.
INTERFACE set_val
  MODULE PROCEDURE geo_proj_set_val
END INTERFACE

!> Method for testing the existence of the object.
INTERFACE c_e
  MODULE PROCEDURE geo_proj_c_e
END INTERFACE

!> Write the object on a formatted or unformatted file.
INTERFACE write_unit
  MODULE PROCEDURE geo_proj_write_unit
END INTERFACE

!> Read the object from a formatted or unformatted file.
INTERFACE read_unit
  MODULE PROCEDURE geo_proj_read_unit
END INTERFACE

!> Print a brief description on stdout.
INTERFACE display
  MODULE PROCEDURE geo_proj_display
END INTERFACE

!> Compute forward coordinate transformation from geographical system to
!! projected system.
INTERFACE proj
  MODULE PROCEDURE geo_proj_proj
END INTERFACE

!> Compute backward coordinate transformation from projected system to
!! geographical system.
INTERFACE unproj
  MODULE PROCEDURE geo_proj_unproj
END INTERFACE

!> Logical equality operators for objects of the classes \a geo_proj.
!! They are all defined as \c ELEMENTAL thus work also on arrays of
!! any shape.
INTERFACE OPERATOR (==)
  MODULE PROCEDURE geo_proj_eq
END INTERFACE

!> Logical inequality operators for objects of the classes \a geo_proj.
!! They are all defined as \c ELEMENTAL thus work also on arrays of
!! any shape.
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE geo_proj_ne
END INTERFACE


INTEGER,PARAMETER,PUBLIC :: &
 geo_proj_unit_degree = 0, & !< coordinate unit is degrees (longitude periodic over 360 degrees)
 geo_proj_unit_meter = 1     !< coordinate unit is meters (non-periodic)


INTEGER,PARAMETER :: nellips = 41 !< number of predefine ellipsoids

! queste costanti vanno usate per specificare l'ellissoide da usare per
! interpretare i dati UTM, gli ellissoidi sono tratti dal pacchetto
! software "proj" (vedi l'uscita a video del comando \c proj \c -le ).
INTEGER,PARAMETER,PUBLIC :: &
ellips_merit =    1,  & !< constants for predefined ellipsoids MERIT 1983
ellips_sgs85 =    2,  & !< Soviet Geodetic System 85        
ellips_grs80 =    3,  & !< GRS 1980(IUGG, 1980)             
ellips_iau76 =    4,  & !< IAU 1976                         
ellips_airy =     5,  & !< Airy 1830                        
ellips_apl4_9 =   6,  & !< Appl. Physics. 1965              
ellips_nwl9d =    7,  & !< Naval Weapons Lab., 1965         
ellips_mod_airy = 8,  & !< Modified Airy                    
ellips_andrae =   9,  & !< Andrae 1876 (Den., Iclnd.)       
ellips_aust_sa =  10, & !< Australian Natl & S. Amer. 1969  
ellips_grs67 =    11, & !< GRS 67(IUGG 1967)                
ellips_bessel =   12, & !< Bessel 1841                      
ellips_bess_nam = 13, & !< Bessel 1841 (Namibia)            
ellips_clrk66 =   14, & !< Clarke 1866                      
ellips_clrk80 =   15, & !< Clarke 1880 mod.                 
ellips_cpm =      16, & !< Comm. des Poids et Mesures 1799  
ellips_delmbr =   17, & !< Delambre 1810 (Belgium)          
ellips_engelis =  18, & !< Engelis 1985                     
ellips_evrst30 =  19, & !< Everest 1830                     
ellips_evrst48 =  20, & !< Everest 1948                     
ellips_evrst56 =  21, & !< Everest 1956                     
ellips_evrst69 =  22, & !< Everest 1969                     
ellips_evrstss =  23, & !< Everest (Sabah & Sarawak)        
ellips_fschr60 =  24, & !< Fischer (Mercury Datum) 1960     
ellips_fschr60m = 25, & !< Modified Fischer 1960            
ellips_fschr68 =  26, & !< Fischer 1968                     
ellips_helmert =  27, & !< Helmert 1906                     
ellips_hough =    28, & !< Hough                            
ellips_intl =     29, & !< International 1909 (Hayford)     
ellips_krass =    30, & !< Krassovsky, 1942                 
ellips_kaula =    31, & !< Kaula 1961                       
ellips_lerch =    32, & !< Lerch 1979                       
ellips_mprts =    33, & !< Maupertius 1738                  
ellips_new_intl = 34, & !< New International 1967           
ellips_plessis =  35, & !< Plessis 1817 (France)            
ellips_seasia =   36, & !< Southeast Asia                   
ellips_walbeck =  37, & !< Walbeck                          
ellips_wgs60 =    38, & !< WGS 60                           
ellips_wgs66 =    39, & !< WGS 66                           
ellips_wgs72 =    40, & !< WGS 72                           
ellips_wgs84 =    41    !< WGS 84                           

DOUBLE PRECISION, PARAMETER, PRIVATE :: &
 rf(nellips)=(/ & ! inverse of flattening for each ellipsoid
 298.257D0, &
 298.257D0, &
 298.257222101D0, &
 298.257D0, &
 299.325D0, &
 298.25D0, &
 298.25D0, &
 299.328D0, &
 300.0D0, &
 298.25D0, &
 298.2471674270D0, &
 299.1528128D0, &
 299.1528128D0, &
 294.98D0, &
 293.4663D0, &
 334.29D0, &
 311.5D0, &
 298.2566D0, &
 300.8017D0, &
 300.8017D0, &
 300.8017D0, &
 300.8017D0, &
 300.8017D0, &
 298.3D0, &
 298.3D0, &
 298.3D0, &
 298.3D0, &
 297.D0, &
 297.D0, &
 298.3D0, &
 298.24D0, &
 298.257D0, &
 191.D0, &
 298.247D0, &
 308.641D0, &
 298.302D0, &
 302.782D0, &
 298.3D0, &
 298.25D0, &
 298.26D0, &
 298.257223563D0 /)
DOUBLE PRECISION, PARAMETER, PRIVATE :: &
 a(nellips)=(/ & ! semi-major axis for each ellipsoid
 6378137.0D0, &
 6378136.0D0, &
 6378137.0D0, &
 6378140.0D0, &
 6377563.396D0, &
 6378137.0D0, &
 6378145.0D0, &
 6377340.189D0, &
 6377104.43D0, &
 6378160.0D0, &
 6378160.0D0, &
 6377397.155D0, &
 6377483.865D0, &
 6378206.4D0, &
 6378249.145D0, &
 6375738.7D0, &
 6376428.D0, &
 6378136.05D0, &
 6377276.345D0, &
 6377304.063D0, &
 6377301.243D0, &
 6377295.664D0, &
 6377298.556D0, &
 6378166.D0, &
 6378155.D0, &
 6378150.D0, &
 6378200.D0, &
 6378270.0D0, &
 6378388.0D0, &
 6378245.0D0, &
 6378163.D0, &
 6378139.D0, &
 6397300.D0, &
 6378157.5D0, &
 6376523.D0, &
 6378155.0D0, &
 6376896.0D0, &
 6378165.0D0, &
 6378145.0D0, &
 6378135.0D0, &
 6378137.0D0 /)

DOUBLE PRECISION,PARAMETER,PRIVATE :: k0=0.9996D0 ! scale factor at central meridian (check whether this is correct and constant)

PRIVATE
PUBLIC geo_proj, geo_proj_rotated, geo_proj_stretched, geo_proj_polar, &
 geo_proj_ellips, &
 geo_proj_new, delete, copy, get_val, set_val, c_e, &
 write_unit, read_unit, display, proj, unproj, OPERATOR(==), OPERATOR(/=)


CONTAINS

!> Constructor for a \a geo_proj object. All the arguments are
!! optional and the keyword form should always be used. The arguments
!! \a ellips_smaj_axis and \a ellips_flatt (specifying explicitly the
!! ellipsoid) are alternative to the argument \a ellips_type
!! specifying a predefined ellipsoid. If no ellipsoid information is
!! provided, a spherical Earth is assumed.
FUNCTION geo_proj_new(proj_type, lov, zone, xoff, yoff, &
 longitude_south_pole, latitude_south_pole, angle_rotation, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor, &
 latin1, latin2, lad, projection_center_flag, &
 ellips_smaj_axis, ellips_flatt, ellips_type) RESULT(this)
CHARACTER(len=*),INTENT(in),OPTIONAL :: proj_type !< type of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lov !< line of view, also known as reference longitude or orientation of the grid (polar projections)
INTEGER,INTENT(in),OPTIONAL :: zone !< Earth zone (mainly for UTM), sets lov to the correct zone central meridian
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xoff !< offset on x axis (false easting)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: yoff !< offset on y axis (false northing)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_south_pole !< longitude of the southern pole of projection 
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_south_pole !< latitude of the southern pole of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: angle_rotation !< angle of rotation of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_stretch_pole !< longitude of the pole of stretching
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_stretch_pole !< latitude of the pole of stretching
DOUBLE PRECISION,INTENT(in),OPTIONAL :: stretch_factor !< stretching factor
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latin1 !< first standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latin2 !< second standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lad !< latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,INTENT(in),OPTIONAL :: projection_center_flag !< flag indicating which pole is represented
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_smaj_axis !< Earth semi-major axis
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_flatt !< Earth flattening
INTEGER,INTENT(in),OPTIONAL :: ellips_type !< number in the interval [1,nellips] indicating a predefined ellipsoid, alternative to the previous arguments

TYPE(geo_proj) :: this

this%proj_type = optio_c(proj_type, LEN(this%proj_type))

! line of view / central meridian, use set_val
this%lov = optio_d(lov)
CALL set_val(this, zone=zone, lov=lov)

! offset / false *ing
this%xoff = optio_d(xoff)
IF (.NOT.c_e(this%xoff)) this%xoff = 0.0D0
this%yoff = optio_d(yoff)
IF (.NOT.c_e(this%yoff)) this%yoff = 0.0D0

this%rotated%longitude_south_pole = optio_d(longitude_south_pole)
this%rotated%latitude_south_pole = optio_d(latitude_south_pole)
this%rotated%angle_rotation = optio_d(angle_rotation)
this%stretched%longitude_stretch_pole = optio_d(longitude_stretch_pole)
this%stretched%latitude_stretch_pole = optio_d(latitude_stretch_pole)
this%stretched%stretch_factor = optio_d(stretch_factor)
this%polar%latin1 = optio_d(latin1)
this%polar%latin2 = optio_d(latin2)
this%polar%lad = optio_d(lad)
this%polar%projection_center_flag = optio_l(projection_center_flag)

! ellipsoid, start from sphere, then use set_val
CALL ellips_compute(this%ellips)
CALL set_val(this, ellips_smaj_axis=ellips_smaj_axis, ellips_flatt=ellips_flatt, &
 ellips_type=ellips_type)

END FUNCTION geo_proj_new


! compute constants related to the desired ellipsoid as a function of
! semi-major axis and inverse of flattening
SUBROUTINE ellips_compute(this, a, f)
TYPE(geo_proj_ellips),INTENT(inout) :: this
DOUBLE PRECISION,INTENT(in),OPTIONAL :: a, f

IF (PRESENT(a) .AND. PRESENT(f)) THEN
  this%f = f
  this%a = a
ELSE IF (PRESENT(a)) THEN ! parameters for a spherical Earth with given radius
  this%f = 0.0D0
  this%a = a
ELSE ! parameters for a standard spherical Earth
  this%f = 0.0D0
  this%a = rearth
ENDIF

this%e2 = 2.0D0*this%f - this%f*this%f ! Eccentricity
this%e1 = this%f/(2.0D0 - this%f)
this%ep2 = this%e2/(1.0D0 - this%e2)
this%e11 = 3.0D0*this%e1/2.0D0 - 27.0D0*this%e1*this%e1*this%e1/32.0D0
this%e12 = 21.0D0*this%e1*this%e1/16.0D0 &
 - 55.0D0*this%e1*this%e1*this%e1*this%e1/32.0D0
this%e13 = 151.0D0*this%e1*this%e1*this%e1/96.0D0
this%e14 = 1097.0D0*this%e1*this%e1*this%e1*this%e1/512.0D0
this%e4 = this%e2*this%e2
this%e6 = this%e2*this%e4
this%ef0 = this%a*(1.0D0 - 0.25D0*this%e2&
 *(1.0D0+this%e2/16.0D0*(3.0D0 + 1.25D0*this%e2)))
this%ef1 = this%a*(0.375D0*this%e2 &
 *(1.0D0 + 0.25D0*this%e2*(1.0D0 + 0.46875D0*this%e2)))
this%ef2 = this%a*(0.05859375D0*this%e2*this%e2*(1.0D0 + 0.75D0*this%e2))
this%ef3 = this%a*this%e2*this%e2*this%e2*35.0D0/3072.0D0

END SUBROUTINE ellips_compute


!> Destroy a \a geo_proj object.
SUBROUTINE geo_proj_delete(this)
TYPE(geo_proj),INTENT(inout) :: this

this%proj_type = cmiss
this%lov = dmiss
this%xoff = dmiss
this%yoff = dmiss
this%rotated%longitude_south_pole = dmiss
this%rotated%latitude_south_pole = dmiss
this%rotated%angle_rotation = dmiss
this%stretched%longitude_stretch_pole = dmiss
this%stretched%latitude_stretch_pole = dmiss
this%stretched%stretch_factor = dmiss
this%polar%latin1 = dmiss
this%polar%latin2 = dmiss
this%polar%lad = dmiss
this%polar%projection_center_flag = imiss

END SUBROUTINE geo_proj_delete


!> Create an independent copy of a \a geo_proj object.
SUBROUTINE geo_proj_copy(this, that)
TYPE(geo_proj),INTENT(in) :: this
TYPE(geo_proj),INTENT(out) :: that

that = this

END SUBROUTINE geo_proj_copy


SUBROUTINE geo_proj_get_val(this, &
 proj_type, lov, zone, xoff, yoff, &
 longitude_south_pole, latitude_south_pole, angle_rotation, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor, &
 latin1, latin2, lad, projection_center_flag, &
 ellips_smaj_axis, ellips_flatt, ellips_type, unit)
TYPE(geo_proj),INTENT(in) :: this !< Object to be queried
CHARACTER(len=*),OPTIONAL :: proj_type !< Type of projection
DOUBLE PRECISION,OPTIONAL :: lov !< Line of view, also known as reference longitude or orientation of the grid (polar projections)
INTEGER,OPTIONAL :: zone !< Earth zone (mainly for UTM), sets lov to the correct zone central meridian
DOUBLE PRECISION,OPTIONAL :: xoff !< Offset on x axis (false easting)
DOUBLE PRECISION,OPTIONAL :: yoff !< Offset on y axis (false northing)
DOUBLE PRECISION,OPTIONAL :: longitude_south_pole !< Longitude of the southern pole of projection 
DOUBLE PRECISION,OPTIONAL :: latitude_south_pole !< Latitude of the southern pole of projection
DOUBLE PRECISION,OPTIONAL :: angle_rotation !< Angle of rotation of projection
DOUBLE PRECISION,OPTIONAL :: longitude_stretch_pole !< Longitude of the pole of stretching
DOUBLE PRECISION,OPTIONAL :: latitude_stretch_pole !< Latitude of the pole of stretching
DOUBLE PRECISION,OPTIONAL :: stretch_factor !< Stretching factor
DOUBLE PRECISION,OPTIONAL :: latin1 !< First standard latitude from main pole (Lambert)
DOUBLE PRECISION,OPTIONAL :: latin2 !< Second standard latitude from main pole (Lambert)
DOUBLE PRECISION,OPTIONAL :: lad !< Latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,OPTIONAL :: projection_center_flag !< Flag indicating which pole is represented
DOUBLE PRECISION,OPTIONAL :: ellips_smaj_axis !< Earth semi-major axis
DOUBLE PRECISION,OPTIONAL :: ellips_flatt !< Earth flattening
INTEGER,OPTIONAL :: ellips_type !< number in the interval [1,nellips] indicating a predefined ellipsoid, alternative to the previous arguments
INTEGER,OPTIONAL :: unit !< unit of measure of the projected coordinate, one of the constants \a geo_proj_unit_*

INTEGER :: i

IF (PRESENT(proj_type)) proj_type = this%proj_type

IF (PRESENT(lov) .AND. PRESENT(zone)) THEN
  zone = NINT((this%lov + 183.0D0)/6.0D0)
  lov = this%lov - zone*6.0D0 - 183.0D0
  IF (ABS(lov) < 1.0D-6) lov = 0.0D-6
ELSE IF (PRESENT(lov)) THEN
  lov = this%lov
ELSE IF (PRESENT(zone)) THEN
  zone = NINT((this%lov + 183.0D0)/6.0D0)
ENDIF

IF (PRESENT(xoff)) xoff = this%xoff
IF (PRESENT(yoff)) yoff = this%yoff
IF (PRESENT(longitude_south_pole)) longitude_south_pole = this%rotated%longitude_south_pole
IF (PRESENT(latitude_south_pole)) latitude_south_pole = this%rotated%latitude_south_pole
IF (PRESENT(angle_rotation)) angle_rotation = this%rotated%angle_rotation
IF (PRESENT(longitude_stretch_pole)) longitude_stretch_pole = this%stretched%longitude_stretch_pole
IF (PRESENT(latitude_stretch_pole)) latitude_stretch_pole = this%stretched%latitude_stretch_pole
IF (PRESENT(stretch_factor)) stretch_factor = this%stretched%stretch_factor
IF (PRESENT(latin1)) latin1 = this%polar%latin1
IF (PRESENT(latin2)) latin2 = this%polar%latin2
IF (PRESENT(lad)) lad = this%polar%lad
IF (PRESENT(projection_center_flag)) projection_center_flag = this%polar%projection_center_flag
! ellipsoid
IF (PRESENT(ellips_smaj_axis)) ellips_smaj_axis = this%ellips%a
IF (PRESENT(ellips_flatt)) ellips_flatt = this%ellips%f
IF (PRESENT(ellips_type)) THEN
  ellips_type = imiss
  DO i = 1, nellips
    IF (this%ellips%f == 1.0D0/rf(i) .AND. this%ellips%a == a(i)) THEN
      ellips_type = i
      EXIT
    ENDIF
  ENDDO
ENDIF

IF (PRESENT(unit)) THEN
  SELECT CASE(this%proj_type)
  CASE("regular_ll", "rotated_ll")
    unit = geo_proj_unit_degree
  CASE("lambert", "polar_stereographic", "UTM")
    unit = geo_proj_unit_meter
  CASE default
    unit = imiss
  END SELECT
ENDIF


END SUBROUTINE geo_proj_get_val


SUBROUTINE geo_proj_set_val(this, &
 proj_type, lov, zone, xoff, yoff, &
 longitude_south_pole, latitude_south_pole, angle_rotation, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor, &
 latin1, latin2, lad, projection_center_flag, &
 ellips_smaj_axis, ellips_flatt, ellips_type)
TYPE(geo_proj),INTENT(inout) :: this !< Object to be updated
CHARACTER(len=*),OPTIONAL :: proj_type !< Type of projection
DOUBLE PRECISION,OPTIONAL :: lov !< Line of view, also known as reference longitude or orientation of the grid (polar projections)
INTEGER,INTENT(in),OPTIONAL :: zone !< Earth zone (mainly for UTM), sets lov to the correct zone central meridian
DOUBLE PRECISION,OPTIONAL :: xoff !< Offset on x axis (false easting)
DOUBLE PRECISION,OPTIONAL :: yoff !< Offset on y axis (false northing)
DOUBLE PRECISION,OPTIONAL :: longitude_south_pole !< Longitude of the southern pole of projection 
DOUBLE PRECISION,OPTIONAL :: latitude_south_pole !< Latitude of the southern pole of projection
DOUBLE PRECISION,OPTIONAL :: angle_rotation !< Angle of rotation of projection
DOUBLE PRECISION,OPTIONAL :: longitude_stretch_pole !< Longitude of the pole of stretching
DOUBLE PRECISION,OPTIONAL :: latitude_stretch_pole !< Latitude of the pole of stretching
DOUBLE PRECISION,OPTIONAL :: stretch_factor !< Stretching factor
DOUBLE PRECISION,OPTIONAL :: latin1 !< First standard latitude from main pole (Lambert)
DOUBLE PRECISION,OPTIONAL :: latin2 !< Second standard latitude from main pole (Lambert)
DOUBLE PRECISION,OPTIONAL :: lad !< Latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,OPTIONAL :: projection_center_flag !< Flag indicating which pole is represented
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_smaj_axis !< Earth semi-major axis
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_flatt !< Earth flattening
INTEGER,INTENT(in),OPTIONAL :: ellips_type !< number in the interval [1,nellips] indicating a predefined ellipsoid, alternative to the previous arguments

INTEGER :: lzone
DOUBLE PRECISION :: llov


! line of view / central meridian
llov = optio_d(lov)
lzone = optio_i(zone)
IF (c_e(llov) .AND. c_e(lzone)) THEN
  this%lov = llov + zone*6.0D0 - 183.0D0
ELSE IF (c_e(llov)) THEN
  this%lov = llov
ELSE IF (c_e(lzone)) THEN
  this%lov = lzone*6.0D0 - 183.0D0
ENDIF

! ellipsoid
IF (PRESENT(ellips_smaj_axis)) THEN
! explicit ellipsoid parameters provided (sphere if flatt is not present or 0)
  CALL ellips_compute(this%ellips, ellips_smaj_axis, ellips_flatt)
ELSE IF (PRESENT(ellips_type)) THEN
  IF (ellips_type > 0 .AND. ellips_type <= nellips) THEN
! an hard coded ellipsoid has been requested
    CALL ellips_compute(this%ellips, a(ellips_type), 1.0D0/rf(ellips_type))
  ELSE ! fallback to default sphere
    CALL ellips_compute(this%ellips)
  ENDIF
ENDIF

! todo all the rest

END SUBROUTINE geo_proj_set_val


FUNCTION geo_proj_c_e(this) RESULT(c_e)
TYPE(geo_proj),INTENT(in) :: this
LOGICAL :: c_e

c_e = this%proj_type /= cmiss

END FUNCTION geo_proj_c_e


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE geo_proj_read_unit(this, unit)
TYPE(geo_proj),INTENT(out) :: this !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this
ELSE
  READ(unit)this
ENDIF

END SUBROUTINE geo_proj_read_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE geo_proj_write_unit(this, unit)
TYPE(geo_proj),INTENT(in) :: this !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this
ELSE
  WRITE(unit)this
ENDIF

END SUBROUTINE geo_proj_write_unit


SUBROUTINE geo_proj_display(this)
TYPE(geo_proj),INTENT(in) :: this

PRINT*,"<<<<<<<<<<<<<<< ",t2c(this%proj_type,"undefined projection"), &
 " >>>>>>>>>>>>>>>>"

IF (c_e(this%xoff) .AND. this%xoff /= 0.0D0) THEN
  PRINT*,"False easting",this%xoff
ENDIF
IF (c_e(this%yoff) .AND. this%yoff /= 0.0D0) THEN
  PRINT*,"False northing",this%yoff
ENDIF

IF (c_e(this%lov)) THEN
  PRINT*,"Central Meridian",this%lov
ENDIF

IF (this%proj_type == 'rotated_ll' .OR. this%proj_type == 'stretched_rotated_ll') THEN
  PRINT*,"Rotated projection:"
  PRINT*,"lon of south pole",this%rotated%longitude_south_pole
  PRINT*,"lat of south pole",this%rotated%latitude_south_pole
  PRINT*,"angle of rotation",this%rotated%angle_rotation
ENDIF

IF (this%proj_type == 'stretched_ll' .OR. this%proj_type == 'stretched_rotated_ll') THEN
  PRINT*,"Stretched projection:"
  PRINT*,"lon of stretch pole",this%stretched%longitude_stretch_pole
  PRINT*,"lat of stretch pole",this%stretched%latitude_stretch_pole
  PRINT*,"stretching factor",this%stretched%stretch_factor
ENDIF

IF (this%proj_type == 'lambert' .OR. this%proj_type == 'polar_stereographic') THEN
  PRINT*,"Polar projection:"
  IF (c_e(this%polar%latin1) .OR. c_e(this%polar%latin2)) THEN
    PRINT*,"lat of intersections",this%polar%latin1,this%polar%latin2
  ENDIF
  IF (c_e(this%polar%lad)) THEN
    PRINT*,"isometric latitude",this%polar%lad
  ENDIF
  IF (IAND(this%polar%projection_center_flag, 128) == 0) THEN
    PRINT*,"North Pole"
  ELSE
    PRINT*,"South Pole"
  ENDIF
ENDIF

IF (this%proj_type == 'mercator') THEN
  IF (c_e(this%polar%lad)) THEN
    PRINT*,"isometric latitude",this%polar%lad
  ENDIF
ENDIF

IF (this%ellips%f == 0.0D0) THEN
  PRINT*,"Spherical Earth:"
  PRINT*,"Radius (m)",this%ellips%a
ELSE
  PRINT*,"Ellipsoid:"
  PRINT*,"Flattening",this%ellips%f
  PRINT*,"Reverse of flattening",1.0D0/this%ellips%f
  PRINT*,"Semi-major axis (m)",this%ellips%a
ENDIF


END SUBROUTINE geo_proj_display


!> Computes and returns coordinates in the projected system given the
!! geographical coordinates.
ELEMENTAL SUBROUTINE geo_proj_proj(this, lon, lat, x, y)
TYPE(geo_proj),INTENT(in) :: this !< object to project
!> geographical coordinates
DOUBLE PRECISION, INTENT(in) :: lon, lat
!> projected coordinates
DOUBLE PRECISION, INTENT(out) :: x, y

SELECT CASE(this%proj_type)

CASE("regular_ll")
  CALL proj_regular_ll(lon, lat, x, y)

CASE("rotated_ll")
  CALL proj_rotated_ll(lon, lat, x, y, this%rotated%longitude_south_pole, &
   this%rotated%latitude_south_pole, this%rotated%angle_rotation)
  
CASE("lambert")
  CALL proj_lambert(lon, lat, x, y, this%polar%latin1, &
   this%polar%latin2, this%lov, this%polar%lad, &
   this%polar%projection_center_flag)

CASE("polar_stereographic")
  CALL proj_polar_stereographic(lon, lat, x, y, this%lov, &
   this%polar%lad, this%polar%projection_center_flag)
  
CASE("mercator")
  CALL proj_mercator(lon, lat, x, y, this%lov, this%polar%lad)
  
CASE("UTM")
  CALL proj_utm(lon, lat, x, y, this%lov, this%xoff, this%yoff, this%ellips)

CASE default
  x = dmiss
  y = dmiss

END SELECT

END SUBROUTINE geo_proj_proj


!> Computes and returns geographical coordinates given the coordinates
!! in the projected system.
ELEMENTAL SUBROUTINE geo_proj_unproj(this, x, y, lon, lat)
TYPE(geo_proj),INTENT(in) :: this !< object to unproject
!> projected coordinates
DOUBLE PRECISION, INTENT(in) :: x, y
!> geographical coordinates
DOUBLE PRECISION, INTENT(out) :: lon, lat

SELECT CASE(this%proj_type)

CASE("regular_ll")
  CALL unproj_regular_ll(x, y, lon, lat)

CASE("rotated_ll")
  CALL unproj_rotated_ll(x, y, lon, lat, this%rotated%longitude_south_pole, &
   this%rotated%latitude_south_pole, this%rotated%angle_rotation)
  
CASE("lambert")
  CALL unproj_lambert(x, y, lon, lat, this%polar%latin1, &
   this%polar%latin2, this%lov, this%polar%lad, &
   this%polar%projection_center_flag)

CASE("polar_stereographic")
  CALL unproj_polar_stereographic(x, y, lon, lat, this%lov, &
   this%polar%lad, this%polar%projection_center_flag)
  
CASE("mercator")
  CALL unproj_mercator(x, y, lon, lat, this%lov, this%polar%lad)

CASE("UTM")
  CALL unproj_utm(x, y, lon, lat, this%lov, this%xoff, this%yoff, this%ellips)

CASE default
  lon = dmiss
  lat = dmiss

END SELECT

END SUBROUTINE geo_proj_unproj


ELEMENTAL FUNCTION geo_proj_eq(this, that) RESULT(eq)
TYPE(geo_proj),INTENT(in) :: this, that
LOGICAL :: eq

eq = this%proj_type == that%proj_type .AND. this%xoff == that%xoff .AND. &
 this%yoff == that%yoff .AND. geo_lon_eq(this%lov, that%lov) .AND. &
 geo_lon_eq(this%rotated%longitude_south_pole, that%rotated%longitude_south_pole) .AND. &
 this%rotated%latitude_south_pole == that%rotated%latitude_south_pole .AND. &
 this%rotated%angle_rotation == that%rotated%angle_rotation .AND. &
 this%stretched%latitude_stretch_pole == that%stretched%latitude_stretch_pole .AND. &
 geo_lon_eq(this%stretched%longitude_stretch_pole, that%stretched%longitude_stretch_pole) .AND. &
 this%stretched%stretch_factor == that%stretched%stretch_factor .AND. &
 this%polar%latin1 == that%polar%latin1 .AND. & ! polar%lon1, polar%lat1
 this%polar%latin2 == that%polar%latin2 .AND. & ! intentionally not checked
 this%polar%lad == that%polar%lad .AND. &
 this%polar%projection_center_flag == that%polar%projection_center_flag .AND. &
 this%ellips%f == that%ellips%f .AND. this%ellips%a == that%ellips%a

END FUNCTION geo_proj_eq


ELEMENTAL FUNCTION geo_proj_ne(this, that) RESULT(ne)
TYPE(geo_proj),INTENT(in) :: this, that
LOGICAL :: ne

ne = .NOT. (this == that)

END FUNCTION geo_proj_ne


!> Compare two longitudes and return .TRUE. if they
!! represent the same longitude modulo 360.
ELEMENTAL FUNCTION geo_lon_eq(l1, l2) RESULT(eq)
DOUBLE PRECISION,INTENT(in) :: l1 !< first longitude
DOUBLE PRECISION,INTENT(in) :: l2 !< second longitude

LOGICAL :: eq

!eq = (l1 == l2) .OR. (ABS(l2-l1) == 360.0D0)
eq = MODULO(l2-l1, 360.0D0) == 0.0D0

END FUNCTION geo_lon_eq

! =====================
! == transformations ==
! =====================

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

DOUBLE PRECISION :: cy0,sy0,rx,srx,crx,sy,cy,l_south_pole

! old hibu formula
!l_south_pole = ACOS(-SIN(degrad*latitude_south_pole))
l_south_pole = (latitude_south_pole+90.)*degrad

rx = degrad*(lon - longitude_south_pole)
srx = SIN(rx)
crx = COS(rx)

sy0 = SIN(l_south_pole)
cy0 = COS(l_south_pole)

sy = SIN(degrad*lat)
cy = COS(degrad*lat)

x = raddeg*ATAN2(cy*srx, cy0*cy*crx+sy0*sy)       
y = raddeg*ASIN(cy0*sy - sy0*cy*crx)
x = x + angle_rotation ! check

END SUBROUTINE proj_rotated_ll

ELEMENTAL SUBROUTINE unproj_rotated_ll(x,y,lon,lat,&
 longitude_south_pole, latitude_south_pole, angle_rotation)
DOUBLE PRECISION, INTENT(in) :: x,y
DOUBLE PRECISION, INTENT(out) :: lon,lat
DOUBLE PRECISION, INTENT(in) :: longitude_south_pole, latitude_south_pole, &
 angle_rotation

DOUBLE PRECISION :: cy0, sy0, l_south_pole, xr

xr = (x - angle_rotation)*degrad ! check
! old hibu formula
!l_south_pole = ACOS(-SIN(degrad*latitude_south_pole))
l_south_pole = (latitude_south_pole+90.)*degrad

cy0 = COS(l_south_pole)
sy0 = SIN(l_south_pole)

lat = raddeg*ASIN(sy0*COS(degrad*y)*COS(xr)+cy0*SIN(degrad*y))
lon = longitude_south_pole + &
 raddeg*ASIN(SIN(xr)*COS(degrad*y)/COS(degrad*lat))

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


!http://mathworld.wolfram.com/StereographicProjection.html
ELEMENTAL SUBROUTINE proj_mercator(lon, lat, x, y, lov, lad)
DOUBLE PRECISION,INTENT(in)  :: lon,lat
DOUBLE PRECISION,INTENT(out) :: x,y
DOUBLE PRECISION,INTENT(in) :: lov
DOUBLE PRECISION,INTENT(in)  :: lad

DOUBLE PRECISION :: scx, scy

scy = COS(degrad*lad)*rearth
scx = scy*degrad
x = (lon-lov)*scx
y = LOG(TAN(0.25D0*pi + 0.5D0*lat*degrad))*scy

END SUBROUTINE proj_mercator

ELEMENTAL SUBROUTINE unproj_mercator(x, y, lon, lat, lov, lad)
DOUBLE PRECISION,INTENT(in)  :: x,y
DOUBLE PRECISION,INTENT(out) :: lon,lat
DOUBLE PRECISION,INTENT(in) :: lov
DOUBLE PRECISION,INTENT(in)  :: lad

DOUBLE PRECISION :: scx, scy

scy = COS(degrad*lad)*rearth
scx = scy*degrad
lon = x/scx + lov
lat = 2.0D0*ATAN(EXP(y/scy))*raddeg-90.0D0

END SUBROUTINE unproj_mercator


ELEMENTAL SUBROUTINE proj_utm(lon, lat, x, y, lov, false_e, false_n, ellips)
DOUBLE PRECISION,INTENT(in)  :: lon,lat
DOUBLE PRECISION,INTENT(out) :: x,y
DOUBLE PRECISION,INTENT(in) :: lov
DOUBLE PRECISION,INTENT(in)  :: false_e, false_n
TYPE(geo_proj_ellips),INTENT(in) :: ellips

DOUBLE PRECISION :: deltalon, p
DOUBLE PRECISION :: n, t, t2, c, m, a1, a2, a3, a4, a5, a6, sinp, cosp, tanp

! --- Compute delta longitude in radians
deltalon = degrad*(lon - lov)

! --- Convert phi (latitude) to radians
p = degrad*lat
sinp = SIN(p)
cosp = COS(p)
tanp = TAN(p)

n = ellips%a/SQRT(1.0D0 - ellips%e2*sinp*sinp)
t = tanp*tanp
c = ellips%ep2*cosp*cosp
a1 = deltalon*cosp
!!$m = 111132.0894_fp_utm*lat - 16216.94_fp_utm*SIN(2.0*p) + 17.21_fp_utm*SIN(4.0*p) &
!!$ - 0.02_fp_utm*SIN(6.0*p)
! Modificato rispetto alla routine originale, dipende dall'ellissoide
m = ellips%ef0*p - ellips%ef1*SIN(2.0D0*p) + ellips%ef2*SIN(4.0D0*p) - &
 ellips%ef3*SIN(6.0D0*p)

a2 = a1**2
a3 = a2*a1
a4 = a2**2
a5 = a4*a1
a6 = a4*a2
t2 = t**2

! --- Compute UTM x and y (m)
x = k0*n*(a1 + (1.0D0 - t + c)*a3/6.0D0 &
 + (5.0D0 - 18.0D0*t + t2 + 72.0D0*c - 58.0D0*ellips%ep2) &
 *a5/120.0D0) + false_e
y = k0*(m + n*tanp &
 *(a2/2.0D0 + (5.0D0 - t + 9.0D0*c + &
 4.0D0*c*c)*a4/24.0D0 + (61.0D0 - 58.0D0*t + t2 + &
 600.0D0*c - 330.0D0*ellips%ep2)*a6/720.0D0))
y = y + false_n

END SUBROUTINE proj_utm

ELEMENTAL SUBROUTINE unproj_utm(x, y, lon, lat, lov, false_e, false_n, ellips)
DOUBLE PRECISION,INTENT(IN) :: x, y
DOUBLE PRECISION,INTENT(OUT) :: lon, lat
DOUBLE PRECISION,INTENT(in) :: lov
DOUBLE PRECISION,INTENT(in) :: false_e, false_n
TYPE(geo_proj_ellips),INTENT(in) :: ellips

DOUBLE PRECISION :: xm, ym, m, u, p1, c1, c2, t1, t2, n1, &
 sinp1, cosp1, tanp1, sin2p1, r0, r1, d, d2, d3, d4, d5, d6, p, l

! --- Correct for false easting, southern hemisphere
xm = x - false_e
!IF (zone < 0) THEN
ym = y - false_n
!ELSE
!ym = utmn
!ENDIF

m = ym/k0
u = m/(ellips%a*(1.0D0-ellips%e2/4.0D0 - &
 3.0D0*ellips%e4/64.0D0 - 5.0D0*ellips%e6/256.0D0))
p1 = u + ellips%e11*SIN(2.0D0*u) + ellips%e12*SIN(4.0D0*u) + &
 ellips%e13*SIN(6.0D0*u) + ellips%e14*SIN(8.0D0*u)
sinp1 = SIN(p1)
cosp1 = COS(p1)
tanp1 = TAN(p1)
c1 = ellips%ep2*cosp1**2
c2 = c1**2
t1 = tanp1**2
t2 = t1**2
sin2p1 = sinp1**2
r0 = 1.0D0-ellips%e2*sin2p1
n1 = ellips%a/SQRT(r0)
!r1 = ellips%a*(1.0D0-ellips%e2)/SQRT(r0**3)
r1 = r0/(1.0D0-ellips%e2)

d = xm/(n1*k0)
d2=d**2
d3=d*d2
d4=d*d3
d5=d*d4
d6=d*d5

! other variant from mstortini:
!p = p1 - (1.0D0*tanp1/r0) * (d2/2.0D0 &
! original version with r1 = ellips%a*(1.0D0-ellips%e2)/SQRT(r0**3)
!p = p1 - (n1*tanp1/r1) * (d2/2.0D0 &
! optimized version with different definition of r1
p = p1 - (r1*tanp1) * (d2/2.0D0 &
 - (5.0D0 + 3.0D0*t1 + 10.0D0*c1 - 4.0D0*c2 &
 - 9.0D0*ellips%ep2)*d4/24.0D0 &
 + (61.0D0 + 90.0D0*t1 + 298.0D0*c1 + 45.0D0*t2 &
 - 252D0*ellips%ep2 - 3.0D0*c2)*d6/720.0D0)
lat = raddeg*p
l = (d - (1.0D0 + 2.0D0*t1 + c1)*d3/6.0D0 &
 + (5.0D0 - 2.0D0*c1 + 28.0D0*t1 - 3.0D0*c2 &
 + 8.0D0*ellips%ep2 + 24.0D0*t2)*d5/120.0D0)/cosp1
lon = raddeg*l + lov

END SUBROUTINE unproj_utm

END MODULE geo_proj_class
