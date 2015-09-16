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
!> Classes for handling georeferenced sparse points in geographical
!! corodinates.  This module defines two classes for managing
!! georeferenced points on the Earth in geographical polar
!! coordinates. It allows importing and exporting blocks of points
!! from/to plain text files and \a ESRI \a shapefile's.  Both classes
!! have \a PRIVATE members, so that they cannot be manipulated
!! directly, but only through the proper methods.
!!
!! \ingroup base
MODULE geo_coord_class
USE kinds
USE err_handling
USE char_utilities
USE missing_values
!USE doubleprecision_phys_const
USE file_utilities
#ifdef HAVE_SHAPELIB
USE shapelib
!, ONLY: shpobject, shpreadobject, shpdestroyobject, shpwriteobject, &
! shpcreatesimpleobject
#endif
IMPLICIT NONE


!> REAL Kind for geographical coordinates.
!! This constant has to be used when defining or converting values to be
!! used as geographical coordinates, e.g.:
!! \code
!! REAL(kind=fp_geo) :: x, y
!! coordx = REAL(mylon, kind=fp_geo)
!! \endcode
INTEGER, PARAMETER :: fp_geo=fp_d 
real(kind=fp_geo), PARAMETER :: fp_geo_miss=dmiss

!> Derived type defining an isolated georeferenced point on Earth in
!! polar geographical coordinates
TYPE geo_coord
  PRIVATE
  INTEGER(kind=int_l) :: ilon, ilat
END TYPE geo_coord

TYPE geo_coord_r
  PRIVATE
  REAL(kind=fp_geo) :: lon, lat
END TYPE geo_coord_r


!> Derived type defining a one-dimensional array of georeferenced points
!! with an associated topology (isolated point, arc, polygon, group of
!! points)
TYPE geo_coordvect
  PRIVATE
  REAL(kind=fp_geo),POINTER :: ll(:,:) => null()
  INTEGER :: vsize, vtype
END TYPE geo_coordvect

!> Missing value for geo_coord
TYPE(geo_coord),PARAMETER :: geo_coord_miss=geo_coord(imiss,imiss)

INTEGER, PARAMETER :: geo_coordvect_point = 1 !< Topology for geo_coordvect (from shapelib): isolated point
INTEGER, PARAMETER :: geo_coordvect_arc = 3 !< Topology for geo_coordvect (from shapelib): arc (multiple arcs unsupported)
INTEGER, PARAMETER :: geo_coordvect_polygon = 5 !< Topology for geo_coordvect (from shapelib): polygon (necessarily closed, multiple polygons unsupported)
INTEGER, PARAMETER :: geo_coordvect_multipoint = 8 !< Topology for geo_coordvect (from shapelib): group of points


!> Constructors for the two classes.
!! They have to be called for every object of these types which is
!! going to be used.
INTERFACE init
  MODULE PROCEDURE geo_coord_init, geo_coordvect_init
END INTERFACE

!> Detructors for the two classes.
!! They clean up all the information associated with the corresponding
!! objects.
INTERFACE delete
  MODULE PROCEDURE geo_coord_delete, geo_coordvect_delete
END INTERFACE

!> Methods for returning the value of object members.
INTERFACE getval
  MODULE PROCEDURE geo_coord_getval, geo_coordvect_getval
END INTERFACE

!> Logical equality operator.
INTERFACE OPERATOR (==)
  MODULE PROCEDURE geo_coord_eq
END INTERFACE

!> Logical inequality operator.
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE geo_coord_ne
END INTERFACE

INTERFACE OPERATOR (>=)
  MODULE PROCEDURE geo_coord_ge
END INTERFACE

INTERFACE OPERATOR (>)
  MODULE PROCEDURE geo_coord_gt
END INTERFACE

INTERFACE OPERATOR (<=)
  MODULE PROCEDURE geo_coord_le
END INTERFACE

INTERFACE OPERATOR (<)
  MODULE PROCEDURE geo_coord_lt
END INTERFACE

!> Import one or more \a geo_coordvect objects from a plain text file
!! or for a file in ESRI/Shapefile format.
INTERFACE import
  MODULE PROCEDURE geo_coordvect_import, geo_coordvect_importvect
END INTERFACE

!> Export one or more \a geo_coordvect objects to a plain text file
!! or to a file in ESRI/Shapefile format.
INTERFACE export
  MODULE PROCEDURE geo_coordvect_export, geo_coordvect_exportvect
END INTERFACE

!> Read a single \a geo_coord object or an array of \a geo_coord objects
!! from a Fortran \c FORMATTED or \c UNFORMATTED file.
INTERFACE read_unit
  MODULE PROCEDURE geo_coord_read_unit, geo_coord_vect_read_unit
END INTERFACE

!> Write a single \a geo_coord object or an array of \a geo_coord objects
!! to a Fortran \c FORMATTED or \c UNFORMATTED file.
INTERFACE write_unit
  MODULE PROCEDURE geo_coord_write_unit, geo_coord_vect_write_unit
END INTERFACE

!> Determine whether a point lies inside a polygon or a rectangle.
INTERFACE inside
  MODULE PROCEDURE geo_coord_inside, geo_coord_inside_rectang
END INTERFACE

!> Missing check
INTERFACE c_e
  MODULE PROCEDURE c_e_geo_coord
END INTERFACE

!>Represent geo_coord object in a pretty string
INTERFACE to_char
  MODULE PROCEDURE to_char_geo_coord
END INTERFACE

!>Print object
INTERFACE display
  MODULE PROCEDURE display_geo_coord
END INTERFACE

CONTAINS


! ===================
! ==   geo_coord   ==
! ===================
!> Costruisce un oggetto \a geo_coord con i parametri opzionali forniti.
!! Se sono presenti \a lon e \a lat, inizializza le coordinate geografiche
!! ignorando \a utme e \a utmn, mentre se sono specificati \a utme e \a utmn
!! succede il contrario; non è possibile specificare le coordinate in entrambi
!! i sistemi, usare eventualmente \a to_geo. Se non viene passato nessun parametro
!! opzionale l'oggetto è inizializzato a valore mancante.
SUBROUTINE geo_coord_init(this, lon, lat, ilon, ilat)
TYPE(geo_coord) :: this !< oggetto da inizializzare
REAL(kind=fp_geo), INTENT(IN), OPTIONAL :: lon !< longitudine geografica
REAL(kind=fp_geo), INTENT(IN), OPTIONAL :: lat !< latitudine geografica
integer(kind=int_l), INTENT(IN), OPTIONAL :: ilon !< integer longitudine geografica (nint(lon*1.e5)
integer(kind=int_l), INTENT(IN), OPTIONAL :: ilat !< integer latitudine geografica (nint(lat*1.e5)

real(kind=fp_geo) :: llon,llat

CALL optio(ilon, this%ilon)
CALL optio(ilat, this%ilat)

if (.not. c_e(this%ilon)) then
  CALL optio(lon, llon)
  if (c_e(llon)) this%ilon=nint(llon*1.d5)
end if

if (.not. c_e(this%ilat)) then
  CALL optio(lat, llat)
  if (c_e(llat)) this%ilat=nint(llat*1.d5)
end if

END SUBROUTINE geo_coord_init

!> Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
SUBROUTINE geo_coord_delete(this)
TYPE(geo_coord), INTENT(INOUT) :: this !< oggetto da distruggre

this%ilon = imiss
this%ilat = imiss

END SUBROUTINE geo_coord_delete

!> Restituisce il valore di uno o più componenti di un oggetto \a geo_coord.
!! Qualsiasi combinazione dei parametri opzionali è consentita; se
!! il tipo di coordinata richiesta non è stato inizializzato né calcolato,
!! restituisce il corrispondente valore mancante.
elemental SUBROUTINE geo_coord_getval(this, lon, lat,ilon,ilat)
TYPE(geo_coord),INTENT(IN) :: this !< oggetto di cui restituire i componenti
REAL(kind=fp_geo), INTENT(OUT), OPTIONAL :: lon !< longitudine geografica
REAL(kind=fp_geo), INTENT(OUT), OPTIONAL :: lat !< latitudine geografica
integer(kind=int_l), INTENT(OUT), OPTIONAL :: ilon !< integer longitudine geografica (nint(lon*1.e5)
integer(kind=int_l), INTENT(OUT), OPTIONAL :: ilat !< integer latitudine geografica (nint(lat*1.e5)

IF (PRESENT(ilon)) ilon = getilon(this)
IF (PRESENT(ilat)) ilat = getilat(this)

IF (PRESENT(lon)) lon = getlon(this)
IF (PRESENT(lat)) lat =  getlat(this)

END SUBROUTINE geo_coord_getval


!> Restituisce la latitudine di uno o più componenti di un oggetto \a geo_coord.
!! Se la latitudine non è stata inizializzata né calcolata
!! restituisce il corrispondente valore mancante.
!! Nata per permettere operazioni vettorizzate
elemental FUNCTION  getilat(this)
TYPE(geo_coord),INTENT(IN) :: this !< oggetto di cui restituire latitudine
integer(kind=int_l) :: getilat !< latitudine geografica

getilat = this%ilat

END FUNCTION getilat

!> Restituisce la latitudine di uno o più componenti di un oggetto \a geo_coord.
!! Se la latitudine non è stata inizializzata né calcolata
!! restituisce il corrispondente valore mancante.
!! Nata per permettere operazioni vettorizzate
elemental FUNCTION  getlat(this)
TYPE(geo_coord),INTENT(IN) :: this !< oggetto di cui restituire latitudine
real(kind=fp_geo) :: getlat !< latitudine geografica
integer(kind=int_l) :: ilat

ilat=getilat(this)
if (c_e(ilat)) then
  getlat = ilat*1.d-5
else
  getlat=fp_geo_miss
end if

END FUNCTION getlat

!> Restituisce la longitudine di uno o più componenti di un oggetto \a geo_coord.
!! Se la latitudine non è stata inizializzata né calcolata
!! restituisce il corrispondente valore mancante.
!! Nata per permettere operazioni vettorizzate
elemental FUNCTION  getilon(this)
TYPE(geo_coord),INTENT(IN) :: this !< oggetto di cui restituire latitudine
integer(kind=int_l) :: getilon !< longitudine geografica

getilon = this%ilon

END FUNCTION getilon


!> Restituisce la longitudine di uno o più componenti di un oggetto \a geo_coord.
!! Se la latitudine non è stata inizializzata né calcolata
!! restituisce il corrispondente valore mancante.
!! Nata per permettere operazioni vettorizzate
elemental FUNCTION  getlon(this)
TYPE(geo_coord),INTENT(IN) :: this !< oggetto di cui restituire latitudine
real(kind=fp_geo) :: getlon !< longitudine geografica
integer(kind=int_l) :: ilon

ilon=getilon(this)
if (c_e(ilon)) then
  getlon = ilon*1.d-5
else
  getlon=fp_geo_miss
end if

END FUNCTION getlon


elemental FUNCTION geo_coord_eq(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%ilon == that%ilon .AND. this%ilat == that%ilat)

END FUNCTION geo_coord_eq


elemental FUNCTION geo_coord_ne(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = .not. this == that 

END FUNCTION geo_coord_ne

!> Logical great operator. Returns true if the first point
!! lies to the west of the second or if lon is equal north
elemental FUNCTION geo_coord_gt(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = this%ilon > that%ilon 

if ( this%ilon == that%ilon ) then
  res =  this%ilat > that%ilat
end if

END FUNCTION geo_coord_gt


!> Logical great-equal operator. Returns true if the first point
!! lies to the west of the second or if lon is equal north
elemental FUNCTION geo_coord_ge(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = .not. this < that

END FUNCTION geo_coord_ge

!> Logical less operator. Returns true if the first point
!! lies to the west of the second or if lon is equal south
elemental FUNCTION geo_coord_lt(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res =  this%ilon < that%ilon

if ( this%ilon == that%ilon ) then
  res =  this%ilat < that%ilat
end if


END FUNCTION geo_coord_lt


!> Logical less-equal operator. Returns true if the first point
!! lies to the west of the second or if lon is equal south
elemental FUNCTION geo_coord_le(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = .not. this > that

END FUNCTION geo_coord_le


!> Logical greater-equal operator. Returns true if the first point
!! lies to the northeast of the second
elemental FUNCTION geo_coord_ure(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%ilon >= that%ilon .AND. this%ilat >= that%ilat)

END FUNCTION geo_coord_ure

!> Logical greater operator. Returns true if the first point
!! lies to the northeast of the second
elemental FUNCTION geo_coord_ur(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%ilon > that%ilon .AND. this%ilat > that%ilat)

END FUNCTION geo_coord_ur


!> Logical less-equal operator. Returns true if the first point
!! lies to the southwest of the second
elemental FUNCTION geo_coord_lle(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%ilon <= that%ilon .AND. this%ilat <= that%ilat)

END FUNCTION geo_coord_lle

!> Logical less operator. Returns true if the first point
!! lies to the southwest of the second
elemental FUNCTION geo_coord_ll(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%ilon < that%ilon .AND. this%ilat < that%ilat)

END FUNCTION geo_coord_ll


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! e, nel caso \a this sia un vettore, la lunghezza del record e quella
!! del vettore devono essere accordate. Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE geo_coord_read_unit(this, unit)
TYPE(geo_coord),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CALL geo_coord_vect_read_unit((/this/), unit)

END SUBROUTINE geo_coord_read_unit


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! e, nel caso \a this sia un vettore, la lunghezza del record e quella
!! del vettore devono essere accordate. Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE geo_coord_vect_read_unit(this, unit)
TYPE(geo_coord) :: this(:) !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form
INTEGER :: i

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  read(unit,*) (this(i)%ilon,this(i)%ilat, i=1,SIZE(this))
!TODO bug gfortran compiler !
!missing values are unredeable when formatted
ELSE
  READ(unit) (this(i)%ilon,this(i)%ilat, i=1,SIZE(this))
ENDIF

END SUBROUTINE geo_coord_vect_read_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE geo_coord_write_unit(this, unit)
TYPE(geo_coord),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CALL geo_coord_vect_write_unit((/this/), unit)

END SUBROUTINE geo_coord_write_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE geo_coord_vect_write_unit(this, unit)
TYPE(geo_coord),INTENT(in) :: this(:) !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form
INTEGER :: i

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*) (this(i)%ilon,this(i)%ilat, i=1,SIZE(this))
!TODO bug gfortran compiler !
!missing values are unredeable when formatted
ELSE
  WRITE(unit) (this(i)%ilon,this(i)%ilat, i=1,SIZE(this))
ENDIF

END SUBROUTINE geo_coord_vect_write_unit


!> Restituisce la distanza in m tra 2 oggetti geo_coord.
!! La distanza è calcolata approssimativamente ed è valida per piccoli angoli.
FUNCTION geo_coord_dist(this, that) RESULT(dist)
USE doubleprecision_phys_const
TYPE(geo_coord), INTENT (IN) :: this !< primo punto
TYPE(geo_coord), INTENT (IN) :: that !< secondo punto
REAL(kind=fp_geo) :: dist !< distanza in metri

REAL(kind=fp_geo) :: x,y
! Distanza approssimata, valida per piccoli angoli

x = (getlon(this)-getlon(that))*COS(((getlat(this)+getlat(that))/2.)*degrad)
y = getlat(this)-getlat(that)
dist = SQRT(x**2 + y**2)*degrad*rearth

END FUNCTION geo_coord_dist


!> Determina se il punto indicato da \a this è contenuto in un rettangolo.
!! Il rettangolo è orientato parallelamente agli assi del sistema,
!! i suoi vertici sud-ovest e nord-est sono specificati da altri due punti.
!! La funzione restituisce \c .TRUE. anche se il punto si trova sulla frontiera
!! del rettangolo.
!! Tutti gli oggetti devono essere già stati
!! convertiti ad un sistema di coordinate comune, altrimenti viene
!! restituito \c .FALSE. .
FUNCTION geo_coord_inside_rectang(this, coordmin, coordmax) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this !< oggetto di cui determinare la posizione
TYPE(geo_coord),INTENT(IN) :: coordmin !< vertice sud-ovest del rettangolo
TYPE(geo_coord),INTENT(IN) :: coordmax !< vertice nord-est del rettangolo
LOGICAL :: res !< \c .TRUE. se \a this è dentro il rettangolo o sul bordo e \c .FALSE. se è fuori

res = (geo_coord_ure(this, coordmin) .AND. geo_coord_lle(this,coordmax))

END FUNCTION geo_coord_inside_rectang


! ===================
! == geo_coordvect ==
! ===================
!> Costruisce un oggetto \a geo_coordvect con i parametri opzionali forniti.
!! Se sono presenti \a lon e \a lat, inizializza le coordinate geografiche
!! ignorando \a utme e \a utmn, mentre se sono specificati \a utme e \a utmn
!! succede il contrario; non è possibile specificare le coordinate in entrambi
!! i sistemi, usare eventualmente \a to_geo. Se non viene passato nessun parametro
!! opzionale l'oggetto è inizializzato a valore mancante.
!! Il numero di punti dell'oggetto finale sarà uguale all'estensione
!! del più breve vettore della coppia fornita.
RECURSIVE SUBROUTINE geo_coordvect_init(this, lon, lat)
TYPE(geo_coordvect), INTENT(OUT) :: this !< oggetto da inizializzare
REAL(kind=fp_geo), INTENT(IN), OPTIONAL :: lon(:) !< longitudine geografica
REAL(kind=fp_geo), INTENT(IN), OPTIONAL :: lat(:) !< latitudine geografica

IF (PRESENT(lon) .AND. PRESENT(lat)) THEN
  this%vsize = MIN(SIZE(lon), SIZE(lat))
  ALLOCATE(this%ll(this%vsize,2))
  this%ll(1:this%vsize,1) = lon(1:this%vsize)
  this%ll(1:this%vsize,2) = lat(1:this%vsize)
ELSE
  this%vsize = 0
  NULLIFY(this%ll)
ENDIF
this%vtype = 0 !?

END SUBROUTINE geo_coordvect_init


!> Distrugge l'oggetto in maniera pulita, liberando l'eventuale spazio
!! dinamicamente allocato.
SUBROUTINE geo_coordvect_delete(this)
TYPE(geo_coordvect), INTENT(INOUT) :: this

IF (ASSOCIATED(this%ll)) DEALLOCATE(this%ll)
this%vsize = 0
this%vtype = 0

END SUBROUTINE geo_coordvect_delete


!> Restituisce il valore di uno o più componenti di un oggetto \a geo_coordvect.
!! Qualsiasi combinazione dei parametri opzionali è consentita; se
!! il tipo di coordinata richiesta non è stato inizializzato né calcolato,
!! restituisce il corrispondente valore mancante.
!! Se forniti, i parametri \a lon, \a lat, \a utme, \a utmn devono essere
!! dichiarati come puntatori che vengono
!! allocati dalla \a getval stessa e che devono poi essere
!! deallocati esplicitamente dal programma chiamante.
SUBROUTINE geo_coordvect_getval(this, lon, lat)
TYPE(geo_coordvect),INTENT(IN) :: this !< oggetto di cui restituire i componenti
REAL(kind=fp_geo), OPTIONAL, POINTER :: lon(:) !< longitudine geografica
REAL(kind=fp_geo), OPTIONAL, POINTER :: lat(:) !< latitudine geografica

IF (PRESENT(lon)) THEN
  IF (ASSOCIATED(this%ll)) THEN
    ALLOCATE(lon(this%vsize))
    lon(:) = this%ll(1:this%vsize,1)
  ENDIF
ENDIF
IF (PRESENT(lat)) THEN
  IF (ASSOCIATED(this%ll)) THEN
    ALLOCATE(lat(this%vsize))
    lat(:) = this%ll(1:this%vsize,2)
  ENDIF
ENDIF

END SUBROUTINE geo_coordvect_getval


SUBROUTINE geo_coordvect_import(this, unitsim, &
#ifdef HAVE_SHAPELIB
 shphandle, &
#endif
 nshp)
TYPE(geo_coordvect), INTENT(OUT) :: this
INTEGER,OPTIONAL,INTENT(IN) :: unitsim
#ifdef HAVE_SHAPELIB
TYPE(shpfileobject),OPTIONAL,INTENT(INOUT) :: shphandle
#endif
INTEGER,OPTIONAL,INTENT(IN) :: nshp

REAL(kind=fp_geo),ALLOCATABLE :: llon(:), llat(:)
REAL(kind=fp_geo) :: lv1,lv2,lv3,lv4
INTEGER :: i, lvsize
CHARACTER(len=40) :: lname
#ifdef HAVE_SHAPELIB
TYPE(shpobject) :: shpobj
#endif

IF (PRESENT(unitsim)) THEN
  ! Leggo l'intestazione
  READ(unitsim,*,END=10)lvsize,lv1,lv2,lv3,lv4,lname
  ALLOCATE(llon(lvsize+1), llat(lvsize+1))
  ! Leggo il poligono
  READ(unitsim,*)(llon(i),llat(i), i=1,lvsize)
  ! Lo chiudo se necessario
  IF (llon(1) /= llon(lvsize) .OR. llat(1) /= llat(lvsize)) THEN
    lvsize = lvsize + 1
    llon(lvsize) = llon(1)
    llat(lvsize) = llat(1)
  ENDIF
  ! Lo inserisco nel mio oggetto
  CALL init(this, lon=llon(1:lvsize), lat=llat(1:lvsize))
  this%vtype = geo_coordvect_polygon ! Sempre un poligono
  
  DEALLOCATE(llon, llat)
  RETURN
10 CALL raise_error('nella lettura del file '//TRIM(to_char(unitsim)))
  DEALLOCATE(llon, llat) ! End of file, ritorno un oggetto non assegnato
#ifdef HAVE_SHAPELIB
ELSE IF (PRESENT(shphandle) .AND. PRESENT(nshp)) THEN
  ! Leggo l'oggetto shape
  shpobj = shpreadobject(shphandle, nshp)
  IF (.NOT.shpisnull(shpobj)) THEN
    ! Lo inserisco nel mio oggetto
    CALL init(this, lon=REAL(shpobj%padfx,kind=fp_geo), &
     lat=REAL(shpobj%padfy,kind=fp_geo))
    this%vtype = shpobj%nshptype
    CALL shpdestroyobject(shpobj)
  ELSE
    CALL init(this)
  ENDIF
#endif
ENDIF

END SUBROUTINE geo_coordvect_import


SUBROUTINE geo_coordvect_export(this, unitsim, &
#ifdef HAVE_SHAPELIB
 shphandle, &
#endif
 nshp)
TYPE(geo_coordvect), INTENT(INOUT) :: this
INTEGER,OPTIONAL,INTENT(IN) :: unitsim
#ifdef HAVE_SHAPELIB
TYPE(shpfileobject),OPTIONAL,INTENT(INOUT) :: shphandle
#endif
INTEGER,OPTIONAL,INTENT(IN) :: nshp

INTEGER :: i, lnshp
#ifdef HAVE_SHAPELIB
TYPE(shpobject) :: shpobj
#endif

IF (PRESENT(unitsim)) THEN
  IF (this%vsize > 0) THEN
    ! Scrivo l'intestazione
    WRITE(unitsim,*)SIZE(this%ll,1),-1.,5000.,-0.1,1.1,'Area'
    ! Scrivo il poligono
    WRITE(unitsim,*)(this%ll(i,1:2), i=1,this%vsize)
  ELSE
    CALL raise_warning('oggetto geo_coordvect vuoto, non scrivo niente in '// &
     TRIM(to_char(unitsim)))
  ENDIF
#ifdef HAVE_SHAPELIB
ELSE IF (PRESENT(shphandle)) THEN
  IF (PRESENT(nshp)) THEN
    lnshp = nshp
  ELSE
    lnshp = -1 ! -1 = append
  ENDIF
  ! Creo l'oggetto shape inizializzandolo con il mio oggetto
  shpobj = shpcreatesimpleobject(this%vtype, this%vsize, &
   REAL(this%ll(1:this%vsize,1),kind=fp_d), &
   REAL(this%ll(1:this%vsize,2),kind=fp_d))
  IF (.NOT.shpisnull(shpobj)) THEN
    ! Lo scrivo nello shapefile
    i=shpwriteobject(shphandle, lnshp, shpobj)
    CALL shpdestroyobject(shpobj)
  ENDIF
#endif
ENDIF

END SUBROUTINE geo_coordvect_export

!> Importa un vettore di oggetti \a geo_coordvect da un file in
!! formato testo o in formato \a shapefile.
!! Il parametro \a this è un puntatore che sarà allocato a cura del metodo stesso e
!! dovrà invece essere deallocato da parte del programma chiamante
!! dopo aver chiamato il metodo \a delete per ogni suo elemento.
!! In caso di errore nella fase iniziale di importazione,
!! \a this non verrà associato, e quindi è opportuno testare
!! \code
!! IF (ASSOCIATED(my_coord_vect)) THEN...
!! \endcode
!! nel programma chiamante
!! per intrappolare eventuale condizioni di errore (tipicamente file non
!! trovato o in un formato non compatibile).
!! Entrambi i formati di ingresso non contengono informazioni sul tipo
!! di coordinate dei dati
!! (per il formato shapefile è possibile solo con delle estensioni non standard),
!! per cui questa informazione, se desiderata, deve essere fornita dal
!! programma chiamante.
SUBROUTINE geo_coordvect_importvect(this, shpfilesim, shpfile)
TYPE(geo_coordvect),POINTER :: this(:) !< puntatore all'oggetto su cui importare i dati, viene allocato dalla \a import stessa
CHARACTER(len=*),INTENT(in),OPTIONAL :: shpfilesim !< nome del file in formato testo "SIM", il parametro deve essere fornito solo se si vuole importare da un file di quel tipo
CHARACTER(len=*),INTENT(in),OPTIONAL :: shpfile !< nome delllo shapefile, il parametro deve essere fornito solo se si vuole importare da un file di quel tipo

REAL(kind=fp_geo) :: inu
REAL(kind=fp_d) :: minb(4), maxb(4)
INTEGER :: i, u, ns, lvsize, shptype, dbfnf, dbfnr
CHARACTER(len=40) :: lname
#ifdef HAVE_SHAPELIB
TYPE(shpfileobject) :: shphandle
#endif

NULLIFY(this)

IF (PRESENT(shpfilesim)) THEN
  u = getunit()
  OPEN(u, file=shpfilesim, status='old', ERR=30)
  ns = 0 ! Conto il numero di shape contenute
  DO WHILE(.TRUE.)
    READ(u,*,END=10,ERR=20)lvsize,inu,inu,inu,inu,lname
    READ(u,*,END=20,ERR=20)(inu,inu, i=1,lvsize)
    ns = ns + 1
  ENDDO
10 CONTINUE
  IF (ns > 0) THEN ! Alloco e leggo il mio oggetto
    ALLOCATE(this(ns))
    REWIND(u)
    DO i = 1, ns
      CALL import(this(i), unitsim=u)
    ENDDO
  ENDIF
20 CONTINUE
  CLOSE(u)
  IF (.NOT.ASSOCIATED(this)) THEN
    CALL raise_warning('file '//TRIM(shpfilesim)//' vuoto o corrotto')
  ENDIF
  RETURN
30 CONTINUE
  CALL raise_error('Impossibile aprire il file '//TRIM(shpfile))
  RETURN

ELSE IF (PRESENT(shpfile)) THEN
#ifdef HAVE_SHAPELIB
  shphandle = shpopen(TRIM(shpfile), 'rb')
  IF (shpfileisnull(shphandle)) THEN
    CALL raise_error('Impossibile aprire lo shapefile '//trim(shpfile))
    RETURN
  ENDIF
  CALL shpgetinfo(shphandle, ns, shptype, minb, maxb, dbfnf, dbfnr) ! Ottengo le info sul file
  IF (ns > 0) THEN ! Alloco e leggo il mio oggetto
    ALLOCATE(this(ns))
    this(:)%vtype = shptype
    DO i = 1, ns
      CALL import(this(i), shphandle=shphandle, nshp=i-1)
    ENDDO
  ENDIF
  CALL shpclose(shphandle)
  RETURN
#endif
ENDIF

END SUBROUTINE geo_coordvect_importvect


!> Esporta un vettore di oggetti \a geo_coordvect su un file in
!! formato testo o in formato \a shapefile.
SUBROUTINE geo_coordvect_exportvect(this, shpfilesim, shpfile, append)
TYPE(geo_coordvect) :: this(:) !< oggetto da esportare
CHARACTER(len=*),INTENT(in),OPTIONAL :: shpfilesim !< nome del file in formato testo "SIM", il parametro deve essere fornito solo se si vuole esportare su un file di quel tipo
CHARACTER(len=*),INTENT(in),OPTIONAL :: shpfile !< nome dello shapefile, il parametro deve essere fornito solo se si vuole esportare su un file di quel tipo
!> sistema di coordinate (proiezione) dei dati,
!! usare i parametri \a ::proj_geo (default) o \a ::proj_utm,
!! ha senso se \a this, a seguito di una chiamata a \a ::to_geo o a \a ::to_utm,
!! contiene le coordinate in entrambi i sistemi, altrimenti i dati vengono
!! esportati automaticamente nel solo sistema disponibile
LOGICAL,INTENT(in),OPTIONAL :: append !< se è presente e vale \c .TRUE. , ::export accoda all'eventuale file esistente anziché sovrascriverlo

REAL(kind=fp_d) :: minb(4), maxb(4)
INTEGER :: i, u, ns, shptype, dbfnf, dbfnr
LOGICAL :: lappend
#ifdef HAVE_SHAPELIB
TYPE(shpfileobject) :: shphandle
#endif

IF (PRESENT(append)) THEN
  lappend = append
ELSE
  lappend = .FALSE.
ENDIF
IF (PRESENT(shpfilesim)) THEN
  u = getunit()
  IF (lappend) THEN
    OPEN(u, file=shpfilesim, status='unknown', position='append', ERR=30)
  ELSE
    OPEN(u, file=shpfilesim, status='unknown', ERR=30)
  ENDIF
  DO i = 1, SIZE(this)
    CALL export(this(i), unitsim=u)
  ENDDO
  CLOSE(u)
  RETURN
30 CONTINUE
  CALL raise_error('Impossibile aprire il file '//TRIM(shpfile))
  RETURN
ELSE IF (PRESENT(shpfile)) THEN
#ifdef HAVE_SHAPELIB
  IF (lappend) THEN
    shphandle = shpopen(TRIM(shpfile), 'r+b')
    IF (shpfileisnull(shphandle)) THEN ! shpopen funziona solo su file esistenti
      shphandle = shpcreate(TRIM(shpfile), geo_coordvect_polygon)
    ENDIF
  ELSE
    shphandle = shpcreate(TRIM(shpfile), geo_coordvect_polygon)
  ENDIF
  IF (shpfileisnull(shphandle)) THEN
    CALL raise_error('Impossibile aprire lo shapefile '//TRIM(shpfile))
    RETURN
  ENDIF
  CALL shpgetinfo(shphandle, ns, shptype, minb, maxb, dbfnf, dbfnr) ! Ottengo le info sul file
  DO i = 1, SIZE(this)
    IF (i > ns .OR. lappend) THEN ! Append shape
      CALL export(this(i), shphandle=shphandle)
    ELSE ! Overwrite shape
      CALL export(this(i), shphandle=shphandle, nshp=i-1)
    ENDIF
  ENDDO
  CALL shpclose(shphandle)
  RETURN
#endif
ENDIF

END SUBROUTINE geo_coordvect_exportvect


!> Determina se il punto indicato da \a this si trova
!! dentro o fuori dal poligono descritto dall'oggetto \a poly.
!! Funziona anche se la topologia di \a poly non è poligonale,
!! forzandone la chiusura; usa un algoritmo di ricerca del numero di
!! intersezioni, come indicato in
!! comp.graphics.algorithms FAQ (http://www.faqs.org/faqs/graphics/algorithms-faq/)
!!  o in
!! http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
FUNCTION geo_coord_inside(this, poly) RESULT(inside)
TYPE(geo_coord), INTENT(IN) :: this !< oggetto di cui determinare la posizione
TYPE(geo_coordvect), INTENT(IN) :: poly !< poligono contenitore
LOGICAL :: inside

INTEGER :: i, j, starti

inside = .FALSE. 
IF (ALL(poly%ll(1,:) == poly%ll(poly%vsize,:))) THEN ! Poligono chiuso
  starti = 2
  j = 1
ELSE ! Poligono non chiuso
  starti = 1
  j = poly%vsize
ENDIF
DO i = starti, poly%vsize
  IF ((poly%ll(i,2) <= getlat(this) .AND. &
   getlat(this) < poly%ll(j,2)) .OR. &
   (poly%ll(j,2) <= getlat(this) .AND. &
   getlat(this) < poly%ll(i,2))) THEN
    IF (getlon(this) < (poly%ll(j,1) - poly%ll(i,1)) * &
     (getlat(this) - poly%ll(i,2)) / &
     (poly%ll(j,2) - poly%ll(i,2)) + poly%ll(i,1)) THEN
      inside = .NOT. inside
    ENDIF
  ENDIF
  j = i
ENDDO

END FUNCTION geo_coord_inside


ELEMENTAL FUNCTION c_e_geo_coord(this) result (res)
TYPE(geo_coord),INTENT(in) :: this
LOGICAL :: res

res = .not. this == geo_coord_miss 

end FUNCTION c_e_geo_coord


character(len=80) function to_char_geo_coord(this)
TYPE(geo_coord),INTENT(in) :: this

to_char_geo_coord = "GEO_COORD: Lon="// &
 trim(to_char(getlon(this),miss="Missing lon",form="(f11.5)"))//&
 " Lat="// &
 trim(to_char(getlat(this),miss="Missing lat",form="(f11.5)"))

end function to_char_geo_coord


subroutine display_geo_coord(this)
TYPE(geo_coord),INTENT(in) :: this

print*,trim(to_char(this))

end subroutine display_geo_coord


END MODULE geo_coord_class
