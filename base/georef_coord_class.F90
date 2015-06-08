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
!> This module defines objects describing georeferenced sparse points
!! possibly with topology and projection information.  This module
!! defines two classes, \a georef_coord, which represents a single
!! georeferenced point on the Earth, and \a georef_coord_array which
!! defines a set of points with a topological relation.
!!
!! Both classes have \a PRIVATE members, so that they cannot be
!! manipulated directly, but only through the proper methods.
!!
!! It is also possible to dafine a dynamically extendible array of \a
!! georef_coord_array objects, of type \a arrayof_georef_coord_array,
!! suitable for importing/exporting data from/to a shapefile.
!!
!! \ingroup base
MODULE georef_coord_class
USE err_handling
USE missing_values
USE optional_values
USE geo_proj_class
#ifdef HAVE_SHAPELIB
USE shapelib
#endif
IMPLICIT NONE

!> Derive type defining a single georeferenced point, either in
!! geodetic or in projected coordinates. The object has no information
!! about the georeferenced coordinate system associated, it must be
!! kept separately by the user.
TYPE georef_coord
  PRIVATE
  DOUBLE PRECISION :: x=dmiss, y=dmiss
END TYPE georef_coord

!> Missing value for georef_coord
TYPE(georef_coord),PARAMETER :: georef_coord_miss=georef_coord(dmiss,dmiss)

!> Derived type defining a one-dimensional array of georeferenced points
!! with an associated topology (isolated point, arc, polygon, group of
!! points), possibly broken into parts and with an associated
!! georeferenced coordinate system.
TYPE georef_coord_array
  PRIVATE
  INTEGER,ALLOCATABLE :: parts(:)
  TYPE(georef_coord),ALLOCATABLE :: coord(:)
  INTEGER :: topo=imiss
  TYPE(geo_proj) :: proj
  TYPE(georef_coord) :: bbox(2)=(/georef_coord_miss, georef_coord_miss/)
  LOGICAL :: bbox_updated=.FALSE.
END TYPE georef_coord_array

INTEGER,PARAMETER :: georef_coord_array_point = 1 !< Topology for georef_coord_array (from shapelib): isolated point
INTEGER,PARAMETER :: georef_coord_array_arc = 3 !< Topology for georef_coord_array (from shapelib): arc (multiple arcs unsupported)
INTEGER,PARAMETER :: georef_coord_array_polygon = 5 !< Topology for georef_coord_array (from shapelib): polygon (necessarily closed, multiple polygons unsupported)
INTEGER,PARAMETER :: georef_coord_array_multipoint = 8 !< Topology for georef_coord_array (from shapelib): group of points


!> Detructors for the two classes.
!! They clean up all the information associated with the corresponding
!! objects.
INTERFACE delete
  MODULE PROCEDURE georef_coord_delete, georef_coord_array_delete
END INTERFACE

!> Check missing value.
INTERFACE c_e
  MODULE PROCEDURE georef_coord_c_e, georef_coord_array_c_e
END INTERFACE

!> Methods for returning the value of object members.
INTERFACE getval
  MODULE PROCEDURE georef_coord_getval, georef_coord_proj_getval, georef_coord_array_getval
END INTERFACE

INTERFACE compute_bbox
  MODULE PROCEDURE georef_coord_array_compute_bbox
END INTERFACE

!> Logical equality operator.
INTERFACE OPERATOR (==)
  MODULE PROCEDURE georef_coord_eq
END INTERFACE

!> Logical inequality operator.
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE georef_coord_ne
END INTERFACE

!> Logical greater-equal operator. Returns true if the first point
!! lies to the northeast of the second
INTERFACE OPERATOR (>=)
  MODULE PROCEDURE georef_coord_ge
END INTERFACE

!> Logical less-equal operator. Returns true if the first point
!! lies to the southwest of the second
INTERFACE OPERATOR (<=)
  MODULE PROCEDURE georef_coord_le
END INTERFACE

#ifdef HAVE_SHAPELIB
!> Import an array of \a georef_coord_array objects from a file
!! in ESRI/Shapefile format.
INTERFACE import
  MODULE PROCEDURE arrayof_georef_coord_array_import
END INTERFACE

!> Export an array of \a georef_coord_array objects to a file
!! in ESRI/Shapefile format.
INTERFACE export
  MODULE PROCEDURE arrayof_georef_coord_array_export
END INTERFACE
#endif

!> Read a single \a georef_coord object or an array of \a georef_coord objects
!! from a Fortran \c FORMATTED or \c UNFORMATTED file.
INTERFACE read_unit
  MODULE PROCEDURE georef_coord_read_unit, georef_coord_vect_read_unit
END INTERFACE

!> Write a single \a georef_coord object or an array of \a georef_coord objects
!! to a Fortran \c FORMATTED or \c UNFORMATTED file.
INTERFACE write_unit
  MODULE PROCEDURE georef_coord_write_unit, georef_coord_vect_write_unit
END INTERFACE

!> Determine whether a point lies inside a polygon or a rectangle.
INTERFACE inside
  MODULE PROCEDURE georef_coord_inside, georef_coord_inside_rectang
END INTERFACE

#define ARRAYOF_ORIGTYPE TYPE(georef_coord_array)
#define ARRAYOF_TYPE arrayof_georef_coord_array
!define ARRAYOF_ORIGEQ 0
#define ARRAYOF_ORIGDESTRUCTOR(x) CALL delete(x)
#include "arrayof_pre.F90"
! from arrayof
PUBLIC insert, append, remove, packarray

PRIVATE
PUBLIC georef_coord, georef_coord_miss, &
 georef_coord_array, georef_coord_array_point, georef_coord_array_arc, &
 georef_coord_array_polygon, georef_coord_array_multipoint, &
 delete, c_e, getval, compute_bbox, OPERATOR(==), OPERATOR(/=), OPERATOR(>=), OPERATOR(<=), &
#ifdef HAVE_SHAPELIB
 import, export, &
#endif
 read_unit, write_unit, inside, &
 georef_coord_new, georef_coord_array_new

CONTAINS

#include "arrayof_post.F90"

! ===================
! ==   georef_coord   ==
! ===================
!> Construct a \a georef_coord object with the optional parameters provided.
!! If coordinates are not provided the object obtained is empty
!! (missing, see c_e function).
FUNCTION georef_coord_new(x, y) RESULT(this)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: x !< x coordinate
DOUBLE PRECISION,INTENT(in),OPTIONAL :: y !< y coordinate
TYPE(georef_coord) :: this

CALL optio(x, this%x)
CALL optio(y, this%y)

END FUNCTION georef_coord_new


SUBROUTINE georef_coord_delete(this)
TYPE(georef_coord),INTENT(inout) :: this

this%x = dmiss
this%y = dmiss

END SUBROUTINE georef_coord_delete


ELEMENTAL FUNCTION georef_coord_c_e(this) RESULT (res)
TYPE(georef_coord),INTENT(in) :: this
LOGICAL :: res

res = .NOT. this == georef_coord_miss 

END FUNCTION georef_coord_c_e


!> Query a \a georef_coord object.
!! This is the correct way to retrieve the contents of a \a
!! georef_coord object, since its members are declared as \a
!! PRIVATE. It is declared as \a ELEMENTAL, thus it works also on
!! arrays of any shape and, in that case, the result will hae the same
!! shape as \a this.
ELEMENTAL SUBROUTINE georef_coord_getval(this, x, y)
TYPE(georef_coord),INTENT(in) :: this !< object to query
DOUBLE PRECISION,INTENT(out),OPTIONAL :: x !< x-coordinate
DOUBLE PRECISION,INTENT(out),OPTIONAL :: y !< y-coordinate

IF (PRESENT(x)) x = this%x
IF (PRESENT(y)) y = this%y

END SUBROUTINE georef_coord_getval


!> Query a \a georef_coord object associating a geographical projection to it.
!! This method allow to interpret the \a x,y coordinates of a \a
!! georef_coord object as projected on a specified geographical
!! projection system and retrieve the geodetic longitude and/or
!! latitude associated to them. When \a x or \y are requested it works
!! as the basic get_val method. It is declared as \a ELEMENTAL, thus
!! it works also on arrays of any shape and, in that case, the result
!! will hae the same shape as \a this.
ELEMENTAL SUBROUTINE georef_coord_proj_getval(this, proj, x, y, lon, lat)
TYPE(georef_coord),INTENT(in) :: this !< object to query
TYPE(geo_proj),INTENT(in) :: proj !< geographical projection associated to coordinates of \a this
DOUBLE PRECISION,INTENT(out),OPTIONAL :: x !< x-coordinate
DOUBLE PRECISION,INTENT(out),OPTIONAL :: y !< y-coordinate
DOUBLE PRECISION,INTENT(out),OPTIONAL :: lon !< geodetic longitude
DOUBLE PRECISION,INTENT(out),OPTIONAL :: lat !< geodetic latitude

DOUBLE PRECISION :: llon, llat

IF (PRESENT(x)) x = this%x
IF (PRESENT(y)) y = this%y
IF (PRESENT(lon) .OR. present(lat)) THEN
  CALL unproj(proj, this%x, this%y, llon, llat)
  IF (PRESENT(lon)) lon = llon
  IF (PRESENT(lat)) lat = llat
ENDIF

END SUBROUTINE georef_coord_proj_getval


! document and improve
ELEMENTAL FUNCTION  getlat(this)
TYPE(georef_coord),INTENT(in) :: this ! oggetto di cui restituire latitudine
DOUBLE PRECISION :: getlat ! latitudine geografica

getlat = this%y ! change!!!

END FUNCTION getlat

! document and improve
ELEMENTAL FUNCTION  getlon(this)
TYPE(georef_coord),INTENT(in) :: this ! oggetto di cui restituire latitudine
DOUBLE PRECISION :: getlon ! longitudine geografica

getlon = this%x ! change!!!

END FUNCTION getlon


ELEMENTAL FUNCTION georef_coord_eq(this, that) RESULT(res)
TYPE(georef_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%x == that%x .AND. this%y == that%y)

END FUNCTION georef_coord_eq


ELEMENTAL FUNCTION georef_coord_ge(this, that) RESULT(res)
TYPE(georef_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%x >= that%x .AND. this%y >= that%y)

END FUNCTION georef_coord_ge


ELEMENTAL FUNCTION georef_coord_le(this, that) RESULT(res)
TYPE(georef_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%x <= that%x .AND. this%y <= that%y)

END FUNCTION georef_coord_le


ELEMENTAL FUNCTION georef_coord_ne(this, that) RESULT(res)
TYPE(georef_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION georef_coord_ne


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! e, nel caso \a this sia un vettore, la lunghezza del record e quella
!! del vettore devono essere accordate. Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE georef_coord_read_unit(this, unit)
TYPE(georef_coord),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CALL georef_coord_vect_read_unit((/this/), unit)

END SUBROUTINE georef_coord_read_unit


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! e, nel caso \a this sia un vettore, la lunghezza del record e quella
!! del vettore devono essere accordate. Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE georef_coord_vect_read_unit(this, unit)
TYPE(georef_coord) :: this(:) !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form
INTEGER :: i

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  read(unit,*) (this(i)%x,this(i)%y, i=1,SIZE(this))
!TODO bug gfortran compiler !
!missing values are unredeable when formatted
ELSE
  READ(unit) (this(i)%x,this(i)%y, i=1,SIZE(this))
ENDIF

END SUBROUTINE georef_coord_vect_read_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE georef_coord_write_unit(this, unit)
TYPE(georef_coord),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CALL georef_coord_vect_write_unit((/this/), unit)

END SUBROUTINE georef_coord_write_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE georef_coord_vect_write_unit(this, unit)
TYPE(georef_coord),INTENT(in) :: this(:) !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form
INTEGER :: i

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*) (this(i)%x,this(i)%y, i=1,SIZE(this))
!TODO bug gfortran compiler !
!missing values are unredeable when formatted
ELSE
  WRITE(unit) (this(i)%x,this(i)%y, i=1,SIZE(this))
ENDIF

END SUBROUTINE georef_coord_vect_write_unit


!> Restituisce la distanza in m tra 2 oggetti georef_coord.
!! La distanza è calcolata approssimativamente ed è valida per piccoli angoli.
!FUNCTION georef_coord_dist(this, that) RESULT(dist)
!USE doubleprecision_phys_const
!TYPE(georef_coord), INTENT (IN) :: this !< primo punto
!TYPE(georef_coord), INTENT (IN) :: that !< secondo punto
!DOUBLE PRECISION :: dist !< distanza in metri
!
!DOUBLE PRECISION :: x,y
!! Distanza approssimata, valida per piccoli angoli
!
!x = (this%x-that%x)*COS(((this%y+this%y)/2.)*degrad)
!y = (this%y-that%y)
!dist = SQRT(x**2 + y**2)*degrad*rearth
!
!END FUNCTION georef_coord_dist


!> Determines whether the point \a this lies inside a specified rectangle.
!! The rectangle is oriented parallely to the coordinate system, its
!! lower-left and upper-right vertices are specified by the two
!! arguments. The function returns \c .TRUE. also in the case it lies
!! exactly on the border of the rectangle.
FUNCTION georef_coord_inside_rectang(this, coordmin, coordmax) RESULT(res)
TYPE(georef_coord),INTENT(IN) :: this !< point to test
TYPE(georef_coord),INTENT(IN) :: coordmin !< lower-left vertex
TYPE(georef_coord),INTENT(IN) :: coordmax !< upper-right vertex
LOGICAL :: res

res = (this >= coordmin .AND. this <= coordmax)

END FUNCTION georef_coord_inside_rectang


! ========================
! == georef_coord_array ==
! ========================
!> Construct a \a georef_coord_array object with the optional parameters provided.
!! If coordinates are not provided the object obtained is empty
!! (missing, see c_e function), if coordinate arrays are of different
!! lengths the \a georef_coord_array is initialised to the shortest
!! length provided.
FUNCTION georef_coord_array_new(x, y, topo, proj) RESULT(this)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: x(:) !< x coordinate array
DOUBLE PRECISION,INTENT(in),OPTIONAL :: y(:) !< y coordinate array
INTEGER,INTENT(in),OPTIONAL :: topo !< topology of the object, one of the \a georef_coord_array_* constants
TYPE(geo_proj),INTENT(in),OPTIONAL :: proj !< geographical projection associated to the coordinates
TYPE(georef_coord_array) :: this

INTEGER :: lsize

IF (PRESENT(x) .AND. PRESENT(y)) THEN
  lsize = MIN(SIZE(x), SIZE(y))
  ALLOCATE(this%coord(lsize))
  this%coord(1:lsize)%x = x(1:lsize)
  this%coord(1:lsize)%y = y(1:lsize)
ENDIF
this%topo = optio_l(topo)
IF (PRESENT(proj)) this%proj = proj

END FUNCTION georef_coord_array_new


SUBROUTINE georef_coord_array_delete(this)
TYPE(georef_coord_array),INTENT(inout) :: this

TYPE(georef_coord_array) :: lobj

this = lobj

END SUBROUTINE georef_coord_array_delete


ELEMENTAL FUNCTION georef_coord_array_c_e(this) RESULT (res)
TYPE(georef_coord_array),INTENT(in) :: this
LOGICAL :: res

res = ALLOCATED(this%coord)

END FUNCTION georef_coord_array_c_e


!> Query a \a georef_coord_array object.
!! This is the correct way to retrieve the contents of a \a
!! georef_coord_array object, since its members are declared as \a
!! PRIVATE.
SUBROUTINE georef_coord_array_getval(this, x, y, topo, proj)
TYPE(georef_coord_array),INTENT(in) :: this !< object to query
DOUBLE PRECISION,OPTIONAL,ALLOCATABLE,INTENT(out) :: x(:) !< x-coordinate
DOUBLE PRECISION,OPTIONAL,ALLOCATABLE,INTENT(out) :: y(:) !< y-coordinate
! allocatable per vedere di nascosto l'effetto che fa
INTEGER,OPTIONAL,INTENT(out) :: topo !< topology associated with the coordinates
TYPE(geo_proj),OPTIONAL,INTENT(out) :: proj !< geographical projection


IF (PRESENT(x)) THEN
  IF (ALLOCATED(this%coord)) THEN
    x = this%coord%x
  ENDIF
ENDIF
IF (PRESENT(y)) THEN
  IF (ALLOCATED(this%coord)) THEN
    y = this%coord%y
  ENDIF
ENDIF
IF (PRESENT(topo)) topo = this%topo
IF (PRESENT(proj)) proj = this%proj ! warning proj has no missing value yet

END SUBROUTINE georef_coord_array_getval


!> Compute the bounding box of each shape in \a georef_coord_array object.
!! The bounding box is computed and stored in the object, it is used
!! by the inside() function for speedup; after it is computed the
!! object cannot be changed, otherwise the bounding box will not be
!! valid.
SUBROUTINE georef_coord_array_compute_bbox(this)
TYPE(georef_coord_array),INTENT(inout) :: this !< object to manipulate

IF (ALLOCATED(this%coord)) THEN
  this%bbox(1)%x = MINVAL(this%coord(:)%x)
  this%bbox(1)%y = MINVAL(this%coord(:)%y)
  this%bbox(2)%x = MAXVAL(this%coord(:)%x)
  this%bbox(2)%y = MAXVAL(this%coord(:)%y)
  this%bbox_updated = .TRUE.
ENDIF

END SUBROUTINE georef_coord_array_compute_bbox

#ifdef HAVE_SHAPELIB
! internal method for importing a single shape
SUBROUTINE georef_coord_array_import(this, shphandle, nshp)
TYPE(georef_coord_array),INTENT(OUT) :: this
TYPE(shpfileobject),INTENT(INOUT) :: shphandle
INTEGER,INTENT(IN) :: nshp

TYPE(shpobject) :: shpobj

! read shape object
shpobj = shpreadobject(shphandle, nshp)
IF (.NOT.shpisnull(shpobj)) THEN
! import it in georef_coord object
  this = georef_coord_array_new(x=DBLE(shpobj%padfx), y=DBLE(shpobj%padfy), &
   topo=shpobj%nshptype)
  IF (shpobj%nparts > 1 .AND. ASSOCIATED(shpobj%panpartstart)) THEN
    this%parts = shpobj%panpartstart(:) ! automatic f95 allocation
  ELSE IF (ALLOCATED(this%parts)) THEN
    DEALLOCATE(this%parts)
  ENDIF
  CALL shpdestroyobject(shpobj)
  CALL compute_bbox(this)
ENDIF


END SUBROUTINE georef_coord_array_import


! internal method for exporting a single shape
SUBROUTINE georef_coord_array_export(this, shphandle, nshp)
TYPE(georef_coord_array),INTENT(in) :: this
TYPE(shpfileobject),INTENT(inout) :: shphandle
INTEGER,INTENT(IN) :: nshp ! index of shape to write starting from 0, -1 to append

INTEGER :: i
TYPE(shpobject) :: shpobj

IF (ALLOCATED(this%coord)) THEN
  IF (ALLOCATED(this%parts)) THEN
    shpobj = shpcreateobject(this%topo, -1, SIZE(this%parts), this%parts, &
     this%parts, SIZE(this%coord), this%coord(:)%x, this%coord(:)%y)
  ELSE
    shpobj = shpcreatesimpleobject(this%topo, SIZE(this%coord), &
     this%coord(:)%x, this%coord(:)%y)
  ENDIF
ELSE
  RETURN
ENDIF

IF (.NOT.shpisnull(shpobj)) THEN
  i = shpwriteobject(shphandle, nshp, shpobj)
  CALL shpdestroyobject(shpobj)
ENDIF

END SUBROUTINE georef_coord_array_export


!> Import an array of \a georef_coord_array objects from a file
!! in ESRI/Shapefile format.  The \a this argument is an uninitialised
!! \a arrayof_georef_coord_array, every element of which,
!! this%array(n), is of type \a georef_coord_array and, on return,
!! will contain information from the n-th shape of the file.  Topology
!! information and possible polygon parts are imported as well, while
!! no projection information, even if available, is imported.  An
!! error condition while opening the file can be detected by checking
!! .NOT.ASSOCIATED(this%array), while an error reading shape \a n can
!! be detected by checking .NOT.c_e(this%array(n)).
SUBROUTINE arrayof_georef_coord_array_import(this, shpfile)
TYPE(arrayof_georef_coord_array),INTENT(out) :: this !< uninitialised array object
CHARACTER(len=*),INTENT(in) :: shpfile !< name of shapefile (with or without extension)

REAL(kind=fp_d) :: minb(4), maxb(4)
INTEGER :: i, ns, shptype, dbfnf, dbfnr
TYPE(shpfileobject) :: shphandle

shphandle = shpopen(TRIM(shpfile), 'rb')
IF (shpfileisnull(shphandle)) THEN
  ! log here
  CALL raise_error()
  RETURN
ENDIF

! get info about file
CALL shpgetinfo(shphandle, ns, shptype, minb, maxb, dbfnf, dbfnr)
IF (ns > 0) THEN ! allocate and read the object
  CALL insert(this, nelem=ns)
  DO i = 1, ns
    CALL georef_coord_array_import(this%array(i), shphandle=shphandle, nshp=i-1)
  ENDDO
ENDIF

CALL shpclose(shphandle)
! pack object to save memory
CALL packarray(this)

END SUBROUTINE arrayof_georef_coord_array_import


!> Export an array of \a georef_coord_array objects to a file
!! in ESRI/Shapefile format.  All the \a this%arraysize shapes
!! contained in \a this are exported to the requested
!! shapefile. Topology information and possible polygon parts are
!! exported as well, while projection information is ignored.
SUBROUTINE arrayof_georef_coord_array_export(this, shpfile)
TYPE(arrayof_georef_coord_array),INTENT(in) :: this !< array object to be exported
CHARACTER(len=*),INTENT(in) :: shpfile !< name of shapefile (with or without extension)

INTEGER :: i
TYPE(shpfileobject) :: shphandle

IF (this%arraysize > 0) THEN
  shphandle = shpcreate(TRIM(shpfile), this%array(1)%topo)
ELSE
  shphandle = shpcreate(TRIM(shpfile), georef_coord_array_polygon)
ENDIF
IF (shpfileisnull(shphandle)) THEN
  ! log here
  CALL raise_error()
  RETURN
ENDIF

DO i = 1, this%arraysize
  CALL georef_coord_array_export(this%array(i), shphandle=shphandle, nshp=i-1)
ENDDO

CALL shpclose(shphandle)

END SUBROUTINE arrayof_georef_coord_array_export
#endif

!> Determines whether the point \a this lies inside the polygon \a poly.
!! The polygon is forced to be closed if it is not already the case,
!! and there is no check about the topology of \a poly to really be of
!! polygon type.  It works also with polygons in parts (as from
!! shapefile specification) defining either multiple polygons or
!! polygons with holes.
!!
!! The method used consists in counting the number of intersections as
!! indicated in comp.graphics.algorithms FAQ
!! (http://www.faqs.org/faqs/graphics/algorithms-faq/) or in
!! http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html.
FUNCTION georef_coord_inside(this, poly) RESULT(inside)
TYPE(georef_coord), INTENT(IN) :: this !< oggetto di cui determinare la posizione
TYPE(georef_coord_array), INTENT(IN) :: poly !< poligono contenitore
LOGICAL :: inside

INTEGER :: i

inside = .FALSE. 
IF (.NOT.c_e(this)) RETURN
IF (.NOT.ALLOCATED(poly%coord)) RETURN
! if outside bounding box stop here
IF (poly%bbox_updated) THEN
  IF (.NOT.georef_coord_inside_rectang(this, poly%bbox(1), poly%bbox(2))) RETURN
ENDIF

IF (ALLOCATED(poly%parts)) THEN
  DO i = 1, SIZE(poly%parts)-1
    inside = inside .NEQV. pointinpoly(this%x, this%y, &
     poly%coord(poly%parts(i)+1:poly%parts(i+1))%x, &
     poly%coord(poly%parts(i)+1:poly%parts(i+1))%y)
  ENDDO
  IF (SIZE(poly%parts) > 0) THEN ! safety check
    inside = inside .NEQV. pointinpoly(this%x, this%y, &
     poly%coord(poly%parts(i)+1:)%x, &
     poly%coord(poly%parts(i)+1:)%y)
  ENDIF

ELSE
  IF (SIZE(poly%coord) < 1) RETURN ! safety check
  inside = pointinpoly(this%x, this%y, &
     poly%coord(:)%x, poly%coord(:)%y)
ENDIF

CONTAINS

FUNCTION pointinpoly(x, y, px, py)
DOUBLE PRECISION, INTENT(in) :: x, y, px(:), py(:)
LOGICAL :: pointinpoly

INTEGER :: i, j, starti

pointinpoly = .FALSE.

IF (px(1) == px(SIZE(px)) .AND. py(1) == py(SIZE(px))) THEN ! closed polygon
  starti = 2
  j = 1
ELSE ! unclosed polygon
  starti = 1
  j = SIZE(px)
ENDIF
DO i = starti, SIZE(px)
  IF ((py(i) <= y .AND. y < py(j)) .OR. &
   (py(j) <= y .AND. y < py(i))) THEN
    IF (x < (px(j) - px(i)) * (y - py(i)) / (py(j) - py(i)) + px(i)) THEN
      pointinpoly = .NOT. pointinpoly
    ENDIF
  ENDIF
  j = i
ENDDO

END FUNCTION pointinpoly

END FUNCTION georef_coord_inside



END MODULE georef_coord_class
