MODULE geo_coordvect_class
USE  geo_coord_class
IMPLICIT NONE

TYPE geo_coordvect
  PRIVATE
  TYPE(geo_coord), POINTER :: g(:)
  INTEGER :: vsize, vtype
END TYPE geo_coordvect


INTEGER, PARAMETER :: & ! Tipi di coordvect (da shapelib)
 geo_coordvect_point = 1, & ! Points
 geo_coordvect_arc = 3, & ! Arcs (Polylines, possible in parts)
 geo_coordvect_polygon = 5, & ! Polygons (possible in parts)
 geo_coordvect_multipoint = 8 ! MultiPoint (related points)

REAL :: overalloc = 2.0 ! fattore di sovrallocazione

INTERFACE init
  MODULE PROCEDURE geo_coordvect_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE geo_coordvect_delete
END INTERFACE

CONTAINS


SUBROUTINE geo_coordvect_init(this, vsize, fileunitsim)
TYPE(geo_coordvect), INTENT(INOUT) :: this
INTEGER, OPTIONAL, INTENT(IN) :: vsize, fileunitsim

REAL(kind=geoprec), ALLOCATABLE :: llon(:), llat(:)
REAL(kind=geoprec) :: lv1,lv2,lv3,lv4
INTEGER :: i, lvsize
CHARACTER(len=40) :: lname

! Inizializza l'oggetto geo_coordvect da un file di poligoni formato SIM
IF (PRESENT(fileunitsim)) THEN
  ! Leggo l'intestazione
  READ(fileunitsim,*,END=10)lvsize,lv1,lv2,lv3,lv4,lname
  ALLOCATE(llon(lvsize+1), llat(lvsize+1))
  ! Leggo il poligono
  READ(fileunitsim,*)(llon(i),llat(i), i=1,lvsize)
  ! Lo chiudo se necessario
  IF (llon(1) /= llon(lvsize) .OR. llat(1) /= llat(lvsize)) THEN
    lvsize = lvsize + 1
    llon(lvsize) = llon(1)
    llat(lvsize) = llat(1)
  ENDIF
  ! Lo inserisco nell'oggetto
  ALLOCATE(this%g(lvsize))
  DO i = 1, lvsize
    CALL init(this%g(i), lon=llon(i), lat=llat(i))
  ENDDO
  this%vsize = lvsize
  this%vtype = geo_coordvect_polygon
  
  DEALLOCATE(llon, llat)
  RETURN
10 NULLIFY(this%g) ! End of file, ritorno un oggetto non assegnato
  this%vsize = 0
  this%vtype = 0
ENDIF

IF (PRESENT(vsize)) THEN
  ALLOCATE(this%g(vsize))
ELSE
  NULLIFY(this%g)
ENDIF
this%vsize = 0
this%vtype = 0

END SUBROUTINE geo_coordvect_init


SUBROUTINE geo_coordvect_delete(this)
TYPE(geo_coordvect), INTENT(INOUT) :: this

IF (ASSOCIATED(this%g)) DEALLOCATE(this%g)
this%vsize = 0
this%vtype = 0

END SUBROUTINE geo_coordvect_delete


SUBROUTINE geo_coordvect_add(this, lon, lat, utme, utmn, fuso, elliss)
TYPE(geo_coordvect), INTENT(INOUT) :: this
REAL(kind=geoprec), INTENT(IN), OPTIONAL :: lon, lat
REAL(kind=utmprec), INTENT(IN), OPTIONAL  :: utme, utmn
INTEGER, INTENT(IN), OPTIONAL  :: fuso
CHARACTER(LEN=20), INTENT(IN), OPTIONAL  :: elliss

TYPE(geo_coordvect) :: tmp
INTEGER :: newsize
LOGICAL :: must_alloc

this%vsize = this%vsize + 1
must_alloc = .FALSE.
IF (ASSOCIATED(this%g)) THEN
  IF (this%vsize > SIZE(this%g)) THEN
    must_alloc = .TRUE.
  ENDIF
ELSE
  must_alloc = .TRUE.
ENDIF

IF (must_alloc) THEN
  ALLOCATE(tmp%g(MAX(INT(this%vsize*overalloc), this%vsize)))
  IF (ASSOCIATED(this%g)) THEN
    tmp%g(1:SIZE(this%g)) = this%g
    DEALLOCATE(this%g)
  ENDIF
  this%g => tmp%g
ENDIF

CALL init(this%g(this%vsize),lon, lat, utme, utmn, fuso, elliss)

END SUBROUTINE geo_coordvect_add


! Determina se un punto sta dentro o fuori il poligono, rif.:
! http://www.faqs.org/faqs/graphics/algorithms-faq/
! http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
FUNCTION geo_coordvect_dentro(this, point) RESULT(dentro)
TYPE(geo_coordvect), INTENT(IN) :: this
TYPE(geo_coord), INTENT(IN) :: point
LOGICAL :: dentro

INTEGER :: i, j

dentro = .FALSE.
j = SIZE(this%g)
DO i = 1, SIZE(this%g)
  IF ((getlat(this%g(i)) <= getlat(point) .AND. &
    getlat(point) < getlat(this%g(j))) .OR. &
   (getlat(this%g(j)) <= getlat(point) .AND. &
   getlat(point) < getlat(this%g(i)))) THEN
    IF (getlon(point) < (getlon(this%g(j)) - getlon(this%g(i))) * &
     (getlat(point) - getlat(this%g(i))) / &
     (getlat(this%g(j)) - getlat(this%g(i))) + getlon(this%g(i))) THEN
      dentro = .NOT. dentro
    ENDIF
  ENDIF
  j = i - 1
ENDDO

END FUNCTION geo_coordvect_dentro


END MODULE geo_coordvect_class
