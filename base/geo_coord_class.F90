#include "config.h"
MODULE geo_coord_class
USE kinds
USE err_handling
USE char_utilities
USE missing_values
USE phys_const
USE file_utilities
#ifdef HAVE_LIBSHP_FORTRAN
USE shplib
!, ONLY: shpobject, shpreadobject, shpdestroyobject, shpwriteobject, &
! shpcreatesimpleobject
#endif
IMPLICIT NONE

!omstart geo_coord_class
!idx classe per la gestione di dati puntuali georeferenziati
!Questo modulo definisce gli oggetti e i metodi per gestire
!dati puntuali georeferenziati con coordinate geografiche o UTM.
!Quando sar&agrave; completo dovr&agrave; gestire indifferentemente,
!anche mescolandoli tra loro, dati in coordinate geografiche e UTM,
!eseguendo le conversioni solo quando necessario.
!
!La classe definisce le seguenti costanti:
!
!geoprec => intero che definisce il KIND dei reali associati a coordinate geografiche
!utmprec => intero che definisce il KIND dei reali associati a coordinate UTM
!
!L'oggetto principale definito dalla classe &egrave;:
!
!geo_coord
!l'oggetto &egrave; "opaco", cio&egrave; le sue componenti sono invisibili
!all'utente che pu&ograve; accedervi solo attraverso i metodi pubblici
!
!I metodi definiti sono i seguenti:
!
!Costruttore (obbligatorio per ogni oggetto)
!SUBROUTINE init(this, lon, lat, utme, utmn, fuso, elliss)
!TYPE(geo_coord) :: this
!REAL(kind=geoprec), INTENT(IN), OPTIONAL :: lon, lat
!REAL(kind=utmprec), INTENT(IN), OPTIONAL  :: utme, utmn
!INTEGER, INTENT(IN), OPTIONAL  :: fuso
!CHARACTER(LEN=20), INTENT(IN), OPTIONAL  :: elliss
!
!Definisce un oggetto geo_coord inizializzando le coordinate ai valori
!geografici (lon, lat) o UTM (utme, utmn, fuso, elliss) forniti
!
!Distruttore (obbligatorio per ogni oggetto)
!SUBROUTINE delete(this)
!TYPE(geo_coord) :: this
!
!Operatori ==, /=
!
!Confrontano oggetti del tipo geo_coord, esiste anche la versione vettoriale
!in cui il secondo membro &egrave; un array
!
!FUNCTION geo_coord_dist(this, that)
!TYPE(geo_coord), INTENT (IN) :: this, that
!REAL :: dist
!
!Restituisce la distanza tra i due punti in m
!
!omend

INTEGER, PARAMETER :: geoprec=fp_s, utmprec=fp_d

TYPE geo_coord
  PRIVATE
  REAL(kind=geoprec) :: lon, lat
  REAL(kind=utmprec) :: utme, utmn
  LOGICAL :: geoce, utmce
  INTEGER :: fuso
  CHARACTER(LEN=20) :: elliss
END TYPE geo_coord

TYPE geo_coordvect
  PRIVATE
  REAL(kind=geoprec),POINTER :: ll(:,:) = null()
  REAL(kind=utmprec),POINTER :: utm(:,:) = null()
  LOGICAL :: geoce, utmce
  INTEGER :: fuso
  CHARACTER(LEN=20) :: elliss
  INTEGER :: vsize, vtype
END TYPE geo_coordvect

TYPE(geo_coord),PARAMETER :: geo_coord_miss= &
 geo_coord(rmiss,rmiss,dmiss,dmiss,.FALSE.,.FALSE.,imiss,cmiss)

INTEGER, PARAMETER :: & ! Tipi di coordvect (da shapelib)
 geo_coordvect_point = 1, & ! Points
 geo_coordvect_arc = 3, & ! Arcs (Polylines, possible in parts)
 geo_coordvect_polygon = 5, & ! Polygons (possible in parts)
 geo_coordvect_multipoint = 8 ! MultiPoint (related points)

REAL :: overalloc = 2.0 ! fattore di sovrallocazione

PRIVATE geo_dist_latlon

INTERFACE init
  MODULE PROCEDURE geo_coord_init, geo_coordvect_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE geo_coord_delete, geo_coordvect_delete
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

INTERFACE import
  MODULE PROCEDURE geo_coordvect_import, geo_coordvect_importvect
END INTERFACE

INTERFACE export
  MODULE PROCEDURE geo_coordvect_export, geo_coordvect_exportvect
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


FUNCTION geo_dist_latlon(lat1, lon1, lat2, lon2) RESULT(dist)
REAL (kind=geoprec), INTENT(IN) :: lat1, lat2, lon1, lon2
REAL :: dist
REAL :: x,y

y = (lat1-lat2)*degrad*rearth
x = (lon1-lon2)*degrad*rearth*COS(((lat1+lat2)/2.)*degrad)
dist=SQRT(x**2+y**2)

END FUNCTION geo_dist_latlon


SUBROUTINE geo_coord_to_utm(this)
TYPE(geo_coord), INTENT (INOUT) :: this

INTEGER :: ierr

IF (this%utmce) RETURN ! gia` fatto
IF (.NOT.this%geoce .AND. .NOT.this%utmce) RETURN ! oggetto vuoto
!!$CALL geo2utm(this%lon, this%lat, this%utme, this%utmn, this%fuso, this%elliss, &
!!$ ierr)
IF (ierr /= 0) THEN
  CALL raise_error('in conversione a utm') ! chiamo delete?
ELSE
  this%utmce = .TRUE.
ENDIF

END SUBROUTINE geo_coord_to_utm


SUBROUTINE geo_coord_to_geo(this)
TYPE(geo_coord), INTENT (INOUT) :: this

INTEGER :: ierr

IF (this%geoce) RETURN ! gia` fatto
IF (.NOT.this%geoce .AND. .NOT.this%utmce) RETURN ! oggetto vuoto
!!$CALL utm2geo(this%utme, this%utmn, this%fuso, this%elliss, this%lon, this%lat, &
!!$ ierr)
IF (ierr /= 0) THEN
  CALL raise_error('in conversione a geo') ! chiamo delete?
ELSE
  this%geoce = .TRUE.
ENDIF

END SUBROUTINE geo_coord_to_geo


! ===================
! == geo_coordvect ==
! ===================
RECURSIVE SUBROUTINE geo_coordvect_init(this, lon, lat, &
 utme, utmn, fuso, elliss)
TYPE(geo_coordvect), INTENT(OUT) :: this
REAL(kind=geoprec), INTENT(IN), OPTIONAL :: lon(:), lat(:)
REAL(kind=utmprec), INTENT(IN), OPTIONAL  :: utme(:), utmn(:)
INTEGER, INTENT(IN), OPTIONAL  :: fuso
CHARACTER(LEN=20), INTENT(IN), OPTIONAL  :: elliss

CALL delete(this)
! Inizializza l'oggetto geo_coordvect da un file di poligoni formato SIM
IF (PRESENT(lon) .AND. PRESENT(lat)) THEN
  this%vsize = MIN(SIZE(lon), SIZE(lat))
  ALLOCATE(this%ll(this%vsize,2))
  this%ll(1:this%vsize,1) = lon(1:this%vsize)
  this%ll(1:this%vsize,2) = lat(1:this%vsize)
  this%geoce = .TRUE.
  this%utmce = .FALSE.
ELSE IF (PRESENT(utme) .AND. PRESENT(utmn) .AND. &
 PRESENT(fuso) .AND. PRESENT(elliss)) THEN
  this%vsize = MIN(SIZE(utme), SIZE(utmn))
  ALLOCATE(this%utm(this%vsize,2))
  this%utm(1:this%vsize,1) = utme(1:this%vsize)
  this%utm(1:this%vsize,2) = utmn(1:this%vsize)
  this%fuso = fuso
  this%elliss = elliss
  this%geoce = .FALSE.
  this%utmce = .TRUE.
ELSE
  this%vsize = 0
ENDIF

END SUBROUTINE geo_coordvect_init


SUBROUTINE geo_coordvect_import(this, unitsim, shphandle, nshp)
TYPE(geo_coordvect), INTENT(OUT) :: this
INTEGER,OPTIONAL,INTENT(IN) :: unitsim
INTEGER(kind=ptr_c),OPTIONAL,INTENT(IN) :: shphandle
INTEGER,OPTIONAL,INTENT(IN) :: nshp

REAL(kind=geoprec),ALLOCATABLE :: llon(:), llat(:)
REAL(kind=geoprec) :: lv1,lv2,lv3,lv4
INTEGER :: i, lvsize
CHARACTER(len=40) :: lname
#ifdef HAVE_LIBSHP_FORTRAN
TYPE(shpobject),POINTER :: shpobj
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
  CALL init(this, lon=llon, lat=llat)
  this%vtype = geo_coordvect_polygon ! Sempre un poligono
  
  DEALLOCATE(llon, llat)
  RETURN
10 CALL raise_error('nella lettura del file '//TRIM(to_char(unitsim)))
  DEALLOCATE(llon, llat) ! End of file, ritorno un oggetto non assegnato
ELSE IF (PRESENT(shphandle) .AND. PRESENT(nshp)) THEN
#ifdef HAVE_LIBSHP_FORTRAN
  NULLIFY(shpobj)
  ! Leggo l'oggetto shape
  shpobj => shpreadobject(shphandle, nshp)
  IF (ASSOCIATED(shpobj)) THEN
    ! Lo inserisco nel mio oggetto
    CALL init(this, lon=REAL(shpobj%padfx,kind=geoprec), &
     lat=REAL(shpobj%padfy,kind=geoprec))
    this%vtype = shpobj%nshptype
    CALL shpdestroyobject(shpobj)
  ELSE
    CALL init(this)
  ENDIF
#endif
ENDIF

END SUBROUTINE geo_coordvect_import


SUBROUTINE geo_coordvect_export(this, unitsim, shphandle, nshp)
TYPE(geo_coordvect), INTENT(INOUT) :: this
INTEGER,OPTIONAL,INTENT(IN) :: unitsim
INTEGER(kind=ptr_c),OPTIONAL,INTENT(IN) :: shphandle
INTEGER,OPTIONAL,INTENT(IN) :: nshp

INTEGER :: i, lnshp
CHARACTER(len=40) :: lname
#ifdef HAVE_LIBSHP_FORTRAN
TYPE(shpobject),POINTER :: shpobj
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
ELSE IF (PRESENT(shphandle)) THEN
#ifdef HAVE_LIBSHP_FORTRAN
  IF (PRESENT(nshp)) THEN
    lnshp = nshp
  ELSE
    lnshp = -1 ! -1 = append
  ENDIF
  NULLIFY(shpobj)
  ! Creo l'oggetto shape inizializzandolo con il mio oggetto
  shpobj => shpcreatesimpleobject(this%vtype, this%vsize, &
   REAL(this%ll(1:this%vsize,1),kind=fp_d), &
   REAL(this%ll(1:this%vsize,2),kind=fp_d))
  IF (ASSOCIATED(shpobj)) THEN
    ! Lo scrifo nello shapefile
    i=shpwriteobject(shphandle, lnshp, shpobj)
    CALL shpdestroyobject(shpobj)
  ENDIF
#endif
ENDIF

END SUBROUTINE geo_coordvect_export


SUBROUTINE geo_coordvect_importvect(this, shpfilesim, shpfile)
TYPE(geo_coordvect),POINTER :: this(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: shpfilesim, shpfile

REAL(kind=geoprec),ALLOCATABLE :: llon(:), llat(:)
REAL(kind=geoprec) :: inu
REAL(kind=fp_d) :: minb(4), maxb(4)
INTEGER :: i, u, ns, lvsize, shptype
CHARACTER(len=40) :: lname
#ifdef HAVE_LIBSHP_FORTRAN
INTEGER(kind=ptr_c) :: shphandle
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
#ifdef HAVE_LIBSHP_FORTRAN
  shphandle = shpopen(TRIM(shpfile), 'rb')
  IF (shphandle == 0) THEN
    CALL raise_error('Impossibile aprire lo shapefile '//trim(shpfile))
    RETURN
  ENDIF
  CALL shpgetinfo(shphandle, ns, shptype, minb, maxb) ! Ottengo le info sul file
  IF (ns > 0) THEN ! Alloco e leggo il mio oggetto
    ALLOCATE(this(ns))
    this(:)%vtype = shptype
    DO i = 1, ns
      CALL import(this(i), shphandle=shphandle, nshp=i)
    ENDDO
  ENDIF
  CALL shpclose(shphandle)
  RETURN
#endif
ENDIF

END SUBROUTINE geo_coordvect_importvect


SUBROUTINE geo_coordvect_exportvect(this, shpfilesim, shpfile, append)
TYPE(geo_coordvect) :: this(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: shpfilesim, shpfile
LOGICAL,INTENT(in),OPTIONAL :: append

REAL(kind=geoprec),ALLOCATABLE :: llon(:), llat(:)
REAL(kind=geoprec) :: lv1,lv2,lv3,lv4
REAL(kind=fp_d) :: minb(4), maxb(4)
INTEGER :: i, u, ns, shptype
CHARACTER(len=40) :: lname
LOGICAL :: lappend
#ifdef HAVE_LIBSHP_FORTRAN
INTEGER(kind=ptr_c) :: shphandle
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
#ifdef HAVE_LIBSHP_FORTRAN
  shphandle = shpopen(TRIM(shpfile), 'r+b')
  IF (shphandle == 0) THEN
    CALL raise_error('Impossibile aprire lo shapefile '//TRIM(shpfile))
    RETURN
  ENDIF
  CALL shpgetinfo(shphandle, ns, shptype, minb, maxb) ! Ottengo le info sul file
  DO i = 1, SIZE(this)
    IF (i > ns .OR. lappend) THEN ! Append shape
      CALL export(this(i), shphandle=shphandle)
    ELSE ! Overwrite shape
      CALL export(this(i), shphandle=shphandle, nshp=i)
    ENDIF
  ENDDO
  CALL shpclose(shphandle)
  RETURN
#endif
ENDIF

END SUBROUTINE geo_coordvect_exportvect


SUBROUTINE geo_coordvect_delete(this)
TYPE(geo_coordvect), INTENT(INOUT) :: this

IF (ASSOCIATED(this%ll)) DEALLOCATE(this%ll)
IF (ASSOCIATED(this%utm)) DEALLOCATE(this%utm)
this%fuso = imiss
this%elliss = cmiss
this%geoce = .FALSE.
this%utmce = .FALSE.
this%vsize = 0
this%vtype = 0

END SUBROUTINE geo_coordvect_delete


!!$SUBROUTINE geo_coordvect_add(this, lon, lat, utme, utmn)
!!$TYPE(geo_coordvect), INTENT(INOUT) :: this
!!$REAL(kind=geoprec), INTENT(IN), OPTIONAL :: lon(:), lat(:)
!!$REAL(kind=utmprec), INTENT(IN), OPTIONAL  :: utme(:), utmn(:)
!!$
!!$TYPE(geo_coordvect) :: tmp
!!$INTEGER :: newsize
!!$LOGICAL :: must_alloc
!!$
!!$IF (PRESENT(lon) .AND. PRESENT(lat)) THEN
!!$  newsize = SIZE(this%ll,1) + MIN(SIZE(lon), SIZE(lat))
!!$  IF (newsize > this%vsize
!!$  must_alloc = .FALSE.
!!$
!!$
!!$IF (ASSOCIATED(this%g)) THEN
!!$  IF (this%vsize > SIZE(this%g)) THEN
!!$    must_alloc = .TRUE.
!!$  ENDIF
!!$ELSE
!!$  must_alloc = .TRUE.
!!$ENDIF
!!$
!!$IF (must_alloc) THEN
!!$  ALLOCATE(tmp%g(MAX(INT(this%vsize*overalloc), this%vsize)))
!!$  IF (ASSOCIATED(this%g)) THEN
!!$    tmp%g(1:SIZE(this%g)) = this%g
!!$    DEALLOCATE(this%g)
!!$  ENDIF
!!$  this%g => tmp%g
!!$ENDIF
!!$
!!$CALL init(this%g(this%vsize),lon, lat, utme, utmn, fuso, elliss)
!!$
!!$END SUBROUTINE geo_coordvect_add


! Determina se un punto sta dentro o fuori il poligono, rif.:
! http://www.faqs.org/faqs/graphics/algorithms-faq/
! http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
FUNCTION geo_coordvect_dentro(this, point) RESULT(dentro)
TYPE(geo_coordvect), INTENT(IN) :: this
TYPE(geo_coord), INTENT(IN) :: point
LOGICAL :: dentro

INTEGER :: i, j

dentro = .FALSE.
IF (this%geoce .AND. point%geoce) THEN
  j = this%vsize
  DO i = 1, this%vsize
    IF ((this%ll(i,2) <= point%lat .AND. &
     point%lat < this%ll(j,2)) .OR. &
     (this%ll(j,2) <= point%lat .AND. &
     point%lat < this%ll(i,2))) THEN
      IF (point%lon < (this%ll(j,1) - this%ll(i,1)) * &
       (point%lat - this%ll(i,2)) / &
       (this%ll(j,2) - this%ll(i,2)) + this%ll(i,1)) THEN
        dentro = .NOT. dentro
      ENDIF
    ENDIF
    j = i - 1
  ENDDO
ELSE IF (this%geoce .AND. point%utmce) THEN
! ripetere per UTM
ENDIF

END FUNCTION geo_coordvect_dentro



END MODULE geo_coord_class
