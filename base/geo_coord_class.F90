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
!fp_geo => intero che definisce il KIND dei reali associati a coordinate geografiche
!fp_utm => intero che definisce il KIND dei reali associati a coordinate UTM
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
!REAL(kind=fp_geo), INTENT(IN), OPTIONAL :: lon, lat
!REAL(kind=fp_utm), INTENT(IN), OPTIONAL  :: utme, utmn
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

INTEGER, PARAMETER :: fp_geo=fp_d, fp_utm=fp_d, proj_geo=1, proj_utm=2

TYPE geo_coorddesc
  PRIVATE
  LOGICAL :: geoce, utmce
  INTEGER :: fuso, elliss
END TYPE geo_coorddesc

TYPE geo_coord
  PRIVATE
  REAL(kind=fp_geo) :: lon, lat
  REAL(kind=fp_utm) :: utme, utmn
  TYPE(geo_coorddesc) :: desc
END TYPE geo_coord

TYPE geo_coordvect
  PRIVATE
  REAL(kind=fp_geo),POINTER :: ll(:,:) => null()
  REAL(kind=fp_utm),POINTER :: utm(:,:) => null()
  TYPE(geo_coorddesc) :: desc
  INTEGER :: vsize, vtype
END TYPE geo_coordvect

TYPE(geo_coord),PARAMETER :: geo_coord_miss= &
 geo_coord(rmiss,rmiss,dmiss,dmiss,geo_coorddesc(.FALSE.,.FALSE.,imiss,imiss))

INTEGER, PARAMETER :: & ! Tipi di coordvect (da shapelib)
 geo_coordvect_point = 1, & ! Points
 geo_coordvect_arc = 3, & ! Arcs (Polylines, possible in parts)
 geo_coordvect_polygon = 5, & ! Polygons (possible in parts)
 geo_coordvect_multipoint = 8 ! MultiPoint (related points)

REAL :: overalloc = 2.0 ! fattore di sovrallocazione

LOGICAL, PRIVATE :: done_init=.FALSE.

! ===========================
! == dichiarazioni per UTM ==
! ===========================
! Ellissoidi presi da 'proj -le' (http://proj.sf.net/)

INTEGER, PARAMETER :: &
 nelliss = 41

INTEGER, PARAMETER :: &
elliss_merit =    1,  & ! MERIT 1983                       
elliss_sgs85 =    2,  & ! Soviet Geodetic System 85        
elliss_grs80 =    3,  & ! GRS 1980(IUGG, 1980)             
elliss_iau76 =    4,  & ! IAU 1976                         
elliss_airy =     5,  & ! Airy 1830                        
elliss_apl4_9 =   6,  & ! Appl. Physics. 1965              
elliss_nwl9d =    7,  & ! Naval Weapons Lab., 1965         
elliss_mod_airy = 8,  & ! Modified Airy                    
elliss_andrae =   9,  & ! Andrae 1876 (Den., Iclnd.)       
elliss_aust_sa =  10, & ! Australian Natl & S. Amer. 1969  
elliss_grs67 =    11, & ! GRS 67(IUGG 1967)                
elliss_bessel =   12, & ! Bessel 1841                      
elliss_bess_nam = 13, & ! Bessel 1841 (Namibia)            
elliss_clrk66 =   14, & ! Clarke 1866                      
elliss_clrk80 =   15, & ! Clarke 1880 mod.                 
elliss_cpm =      16, & ! Comm. des Poids et Mesures 1799  
elliss_delmbr =   17, & ! Delambre 1810 (Belgium)          
elliss_engelis =  18, & ! Engelis 1985                     
elliss_evrst30 =  19, & ! Everest 1830                     
elliss_evrst48 =  20, & ! Everest 1948                     
elliss_evrst56 =  21, & ! Everest 1956                     
elliss_evrst69 =  22, & ! Everest 1969                     
elliss_evrstss =  23, & ! Everest (Sabah & Sarawak)        
elliss_fschr60 =  24, & ! Fischer (Mercury Datum) 1960     
elliss_fschr60m = 25, & ! Modified Fischer 1960            
elliss_fschr68 =  26, & ! Fischer 1968                     
elliss_helmert =  27, & ! Helmert 1906                     
elliss_hough =    28, & ! Hough                            
elliss_intl =     29, & ! International 1909 (Hayford)     
elliss_krass =    30, & ! Krassovsky, 1942                 
elliss_kaula =    31, & ! Kaula 1961                       
elliss_lerch =    32, & ! Lerch 1979                       
elliss_mprts =    33, & ! Maupertius 1738                  
elliss_new_intl = 34, & ! New International 1967           
elliss_plessis =  35, & ! Plessis 1817 (France)            
elliss_seasia =   36, & ! Southeast Asia                   
elliss_walbeck =  37, & ! Walbeck                          
elliss_wgs60 =    38, & ! WGS 60                           
elliss_wgs66 =    39, & ! WGS 66                           
elliss_wgs72 =    40, & ! WGS 72                           
elliss_wgs84 =    41    ! WGS 84                           

REAL(kind=fp_utm), PARAMETER, PRIVATE :: &
 rf(nelliss)=(/ & ! Schiacciamento per ogni ellissoide
 298.257, &
 298.257, &
 298.257222101, &
 298.257, &
 299.325, &
 298.25, &
 298.25, &
 299.328, &
 300.0, &
 298.25, &
 298.2471674270, &
 299.1528128, &
 299.1528128, &
 294.98, &
 293.4663, &
 334.29, &
 311.5, &
 298.2566, &
 300.8017, &
 300.8017, &
 300.8017, &
 300.8017, &
 300.8017, &
 298.3, &
 298.3, &
 298.3, &
 298.3, &
 297., &
 297., &
 298.3, &
 298.24, &
 298.257, &
 191., &
 298.247, &
 308.641, &
 298.302, &
 302.782, &
 298.3, &
 298.25, &
 298.26, &
 298.257223563 /), &
 a(nelliss)=(/ & ! Semiasse maggiore per ogni ellissoide
 6378137.0, &
 6378136.0, &
 6378137.0, &
 6378140.0, &
 6377563.396, &
 6378137.0, &
 6378145.0, &
 6377340.189, &
 6377104.43, &
 6378160.0, &
 6378160.0, &
 6377397.155, &
 6377483.865, &
 6378206.4, &
 6378249.145, &
 6375738.7, &
 6376428., &
 6378136.05, &
 6377276.345, &
 6377304.063, &
 6377301.243, &
 6377295.664, &
 6377298.556, &
 6378166., &
 6378155., &
 6378150., &
 6378200., &
 6378270.0, &
 6378388.0, &
 6378245.0, &
 6378163., &
 6378139., &
 6397300., &
 6378157.5, &
 6376523., &
 6378155.0, &
 6376896.0, &
 6378165.0, &
 6378145.0, &
 6378135.0, &
 6378137.0 /)

! Costanti per UTM basate sull'ellissoide
!!$REAL(kind=fp_utm), PARAMETER, PRIVATE :: &
!!$ f(nelliss)=1.0_fp_utm/rf, e2(nelliss)=2.0_fp_utm*f-f*f, &
!!$ e1(nelliss)=f/(2.0_fp_utm-f), ep2(nelliss)=e2/(1.0_fp_utm-e2), &
!!$ e11(nelliss)=3.0_fp_utm*e1/2.0_fp_utm - 27.0_fp_utm*e1*e1*e1/32.0_fp_utm, &
!!$ e12(nelliss)=21.0_fp_utm*e1*e1/16.0_fp_utm - &
!!$ 55.0_fp_utm*e1*e1*e1*e1/32.0_fp_utm, &
!!$ e13(nelliss)=151.0_fp_utm*e1*e1*e1/96.0_fp_utm, &
!!$ e14(nelliss)=1097.0_fp_utm*e1*e1*e1*e1/512.0_fp_utm, &
!!$ e4(nelliss)=e2*e2, &
!!$ e6(nelliss)=e2*e4
! rf(nelliss) = er/(er-pr) = a/(a-b)

REAL(kind=fp_utm), PRIVATE :: &
 f(nelliss), e2(nelliss), &
 e1(nelliss), ep2(nelliss), &
 e11(nelliss), &
 e12(nelliss), &
 e13(nelliss), &
 e14(nelliss), &
 e4(nelliss), &
 e6(nelliss), &
 ef0(nelliss), ef1(nelliss), ef2(nelliss), ef3(nelliss)

REAL(kind=fp_utm), PARAMETER, PRIVATE :: &
 k0=0.9996_fp_utm, &         ! Fattore di scala al meridiano centrale
 false_e=500000.0_fp_utm, &  ! False easting (m)
 false_n=10000000._fp_utm, & ! False northing per l'Emisfero Sud (m)
 dtr=3.141592654_fp_utm/180.0_fp_utm, & ! definire in phys_const con kind fp_utm?
 rtd=1.0_fp_utm/dtr

PRIVATE ll2utm, utm2ll

!PRIVATE geo_dist_latlon

INTERFACE init
  MODULE PROCEDURE geo_coorddesc_init, geo_coord_init, geo_coordvect_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE geo_coorddesc_delete, geo_coord_delete, geo_coordvect_delete
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE geo_coorddesc_eq, geo_coord_eq, geo_coord_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE geo_coord_ne, geo_coord_nesv
END INTERFACE

INTERFACE getval
  MODULE PROCEDURE geo_coorddesc_getval, geo_coord_getval, geo_coordvect_getval
END INTERFACE

INTERFACE to_geo
  MODULE PROCEDURE geo_coord_to_geo, geo_coordvect_to_geo
END INTERFACE

INTERFACE to_utm
  MODULE PROCEDURE geo_coord_to_utm, geo_coordvect_to_utm
END INTERFACE

INTERFACE import
  MODULE PROCEDURE geo_coordvect_import, geo_coordvect_importvect
END INTERFACE

INTERFACE export
  MODULE PROCEDURE geo_coordvect_export, geo_coordvect_exportvect
END INTERFACE

CONTAINS

SUBROUTINE init_elliss()
! Queste inizializzazione sono fatte qua e non come PARAMETER sopra
! per un bug del compilatore pgf90 con gli array costanti
f(:) = 1.0_fp_utm/rf(:)
e2(:) = 2.0_fp_utm*f(:) - f(:)*f(:) ! Eccentricita`
e1(:) = f(:)/(2.0_fp_utm - f(:))
ep2(:) = e2(:)/(1.0_fp_utm - e2(:))
e11(:) = 3.0_fp_utm*e1(:)/2.0_fp_utm - 27.0_fp_utm*e1(:)*e1(:)*e1(:)/32.0_fp_utm
e12(:) = 21.0_fp_utm*e1(:)*e1(:)/16.0_fp_utm &
 - 55.0_fp_utm*e1(:)*e1(:)*e1(:)*e1(:)/32.0_fp_utm
e13(:) = 151.0_fp_utm*e1(:)*e1(:)*e1(:)/96.0_fp_utm
e14(:) = 1097.0_fp_utm*e1(:)*e1(:)*e1(:)*e1(:)/512.0_fp_utm
e4(:) = e2(:)*e2(:)
e6(:) = e2(:)*e4(:)
ef0(:) = a(:)*(1.0_fp_utm - 0.25_fp_utm*e2(:)&
 *(1.0_fp_utm+e2(:)/16.0_fp_utm*(3.0_fp_utm + 1.25_fp_utm*e2(:))))
ef1(:) = a(:)*(0.375_fp_utm*e2(:) &
 *(1.0_fp_utm + 0.25_fp_utm*e2(:)*(1.0_fp_utm + 0.46875_fp_utm*e2(:))))
ef2(:) = a(:)*(0.05859375_fp_utm*e2(:)*e2(:)*(1.0_fp_utm + 0.75_fp_utm*e2(:)))
ef3(:) = a(:)*e2(:)*e2(:)*e2(:)*35.0_fp_utm/3072.0_fp_utm

done_init = .TRUE.

END SUBROUTINE init_elliss


! ===================
! == geo_coorddesc ==
! ===================
SUBROUTINE geo_coorddesc_init(this, fuso, elliss, geoce, utmce)
TYPE(geo_coorddesc) :: this
INTEGER, INTENT(IN), OPTIONAL  :: fuso, elliss
LOGICAL, INTENT(in), OPTIONAL :: geoce, utmce

IF (PRESENT(fuso)) THEN
  this%fuso = fuso
ELSE
  this%fuso = imiss
ENDIF
IF (PRESENT(elliss)) THEN
  this%elliss = elliss
ELSE
  this%elliss = imiss
ENDIF
IF (PRESENT(geoce)) THEN
  this%geoce = geoce
ELSE
  this%geoce = .FALSE.
ENDIF
IF (PRESENT(utmce)) THEN
  this%utmce = utmce
  IF (this%utmce) THEN ! Inizializza fuso ed ellissoide a default
    IF (this%fuso == imiss) THEN
      CALL raise_error('fuso UTM non specificato')
      STOP
    ENDIF
    IF (this%elliss == imiss) THEN
      this%elliss = elliss_wgs84
    ELSE
      this%elliss = MIN(MAX(this%elliss,1), nelliss)
    ENDIF
  ENDIF
ELSE
  this%utmce = .FALSE.
ENDIF

END SUBROUTINE geo_coorddesc_init


SUBROUTINE geo_coorddesc_delete(this)
TYPE(geo_coorddesc) :: this

this%fuso = imiss
this%elliss = imiss
this%geoce = .FALSE.
this%utmce = .FALSE.

END SUBROUTINE geo_coorddesc_delete


SUBROUTINE geo_coorddesc_getval(this, fuso, elliss)
TYPE(geo_coorddesc),INTENT(IN) :: this
INTEGER, INTENT(OUT), OPTIONAL  :: fuso, elliss

IF (PRESENT(fuso)) fuso = this%fuso
IF (PRESENT(elliss)) elliss = this%elliss

END SUBROUTINE geo_coorddesc_getval

elemental FUNCTION geo_coorddesc_eq(this, that) RESULT(res)
TYPE(geo_coorddesc),INTENT(IN) :: this, that
LOGICAL :: res

! I descrittori di coordinate sono == se indicano lo stesso
! sistema di riferimento o se sono entrambi nulli
IF ((this%geoce .AND. that%geoce) .OR. & ! geo
 (this%utmce .AND. that%utmce .AND. & ! utm
 this%fuso == that%fuso .AND. this%elliss == that%elliss) .OR. &
 (.NOT.this%geoce .AND. .NOT.that%geoce .AND. & ! tutto mancante
 .NOT.this%utmce .AND. .NOT.that%utmce)) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION geo_coorddesc_eq


! ===================
! ==   geo_coord   ==
! ===================
SUBROUTINE geo_coord_init(this, lon, lat, utme, utmn, fuso, elliss)
TYPE(geo_coord) :: this
REAL(kind=fp_geo), INTENT(IN), OPTIONAL :: lon, lat
REAL(kind=fp_utm), INTENT(IN), OPTIONAL  :: utme, utmn
INTEGER, INTENT(IN), OPTIONAL  :: fuso, elliss

IF (.NOT.done_init) CALL init_elliss()
IF (PRESENT(lon) .AND. PRESENT(lat)) THEN
  CALL init(this%desc, fuso, elliss, geoce=.TRUE.)
  this%lon = lon
  this%lat = lat
  this%utme = dmiss
  this%utmn = dmiss
ELSE IF (PRESENT(utme) .AND. PRESENT(utmn)) THEN
  CALL init(this%desc, fuso, elliss, utmce=.TRUE.)
  this%lon = rmiss
  this%lat = rmiss
  this%utme = utme
  this%utmn = utmn
ELSE
  CALL init(this%desc, fuso, elliss)
  this%lon = rmiss
  this%lat = rmiss
  this%utme = dmiss
  this%utmn = dmiss
ENDIF

END SUBROUTINE geo_coord_init


SUBROUTINE geo_coord_delete(this)
TYPE(geo_coord) :: this

CALL delete(this%desc)
this%lon = rmiss
this%lat = rmiss
this%utme = dmiss
this%utmn = dmiss

END SUBROUTINE geo_coord_delete


SUBROUTINE geo_coord_getval(this, lon, lat, utme, utmn, fuso, elliss)
TYPE(geo_coord),INTENT(IN) :: this
REAL(kind=fp_geo), INTENT(OUT), OPTIONAL :: lon, lat
REAL(kind=fp_utm), INTENT(OUT), OPTIONAL  :: utme, utmn
INTEGER, INTENT(OUT), OPTIONAL  :: fuso, elliss

IF (PRESENT(lon)) lon = this%lon
IF (PRESENT(lat)) lat = this%lat
IF (PRESENT(utme)) utme = this%utme
IF (PRESENT(utmn)) utmn = this%utmn
CALL getval(this%desc, fuso, elliss)

END SUBROUTINE geo_coord_getval


elemental FUNCTION geo_coord_eq(this, that) RESULT(res)
TYPE(geo_coord),INTENT(IN) :: this, that
LOGICAL :: res

!IF (geo_coord_equalize(this, that)) THEN ! Se non sono confrontabili trasformo
  IF (this%desc%geoce) THEN
    IF (this%lon == that%lon .AND. this%lat == that%lat) THEN
      res = .TRUE.
    ELSE
      res = .FALSE.
    ENDIF
  ELSE IF (this%desc%utmce) THEN
    IF (this%utme == that%utme .AND. this%utmn == that%utmn) THEN
      res = .TRUE.
    ELSE
      res = .FALSE.
    ENDIF
  ELSE ! entrambi mancanti, e` giusto che siano uguali?
    res = .TRUE.
  ENDIF
!ELSE ! Non sono confrontabili (errore di conversione UTM?)
!  res = .FALSE.
!ENDIF

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


FUNCTION geo_coord_dist(this, that) RESULT(dist)
TYPE(geo_coord), INTENT (IN) :: this, that
REAL(kind=fp_geo) :: dist

REAL(kind=fp_geo) :: x,y
! Distanza approssimata, valida per piccoli angoli

!IF (geo_coord_equalize(this, that)) THEN
  IF (this%desc%geoce) THEN
    x = (this%lon-that%lon)*COS(((this%lat+this%lat)/2.)*degrad)
    y = (this%lat-that%lat)
    dist = SQRT(x**2 + y**2)*degrad*rearth
  ELSE IF (this%desc%utmce) THEN
    dist = SQRT((this%utme - that%utme)**2 + (this%utmn - that%utmn)**2)
  ELSE
    dist = rmiss
  ENDIF
!ELSE
!  dist = rmiss
!ENDIF

END FUNCTION geo_coord_dist


FUNCTION geo_coord_equalize(this, that) RESULT(res)
TYPE(geo_coord), INTENT (INOUT) :: this, that
LOGICAL :: res
! Tenta di rendere confrontabili due oggetti geo_coord convertendo
! opportunamente le coordinate se necessario
! Restituisce true in caso di successo o in caso di dati gia` confrontabili o nulli

IF (this%desc == that%desc) THEN
  res = .TRUE.
  RETURN
ENDIF
IF (this%desc%geoce .AND. that%desc%utmce) THEN
  CALL geo_coord_to_geo(that)
ELSE IF (this%desc%utmce .AND. that%desc%geoce) THEN
  CALL geo_coord_to_geo(this)
ELSE IF (this%desc%utmce .AND. that%desc%utmce) THEN ! diversi fusi/ellissoidi? siamo fusi!
  CALL geo_coord_to_geo(this)
  CALL geo_coord_to_utm(this, fuso=that%desc%fuso, elliss=that%desc%elliss)
ENDIF
! controllo se ho avuto successo
res = this%desc == that%desc

END FUNCTION geo_coord_equalize


SUBROUTINE geo_coord_to_utm(this, fuso, elliss)
TYPE(geo_coord), INTENT (INOUT) :: this
INTEGER, INTENT(IN), OPTIONAL  :: fuso, elliss

IF (.NOT.this%desc%geoce .OR. this%desc%utmce) RETURN ! Niente da fare
CALL init(this%desc, fuso, elliss, utmce=.TRUE.)
CALL ll2utm(this%lon, this%lat, this%desc%fuso, this%desc%elliss, &
 this%utme, this%utmn)

END SUBROUTINE geo_coord_to_utm


SUBROUTINE geo_coord_to_geo(this)
TYPE(geo_coord), INTENT (INOUT) :: this

IF (.NOT.this%desc%utmce .OR. this%desc%geoce) RETURN ! Niente da fare
CALL utm2ll(this%utme, this%utmn, this%desc%fuso, this%desc%elliss, &
 this%lon, this%lat)
this%desc%geoce = .TRUE.

END SUBROUTINE geo_coord_to_geo


! ===================
! == geo_coordvect ==
! ===================
RECURSIVE SUBROUTINE geo_coordvect_init(this, lon, lat, &
 utme, utmn, fuso, elliss)
TYPE(geo_coordvect), INTENT(OUT) :: this
REAL(kind=fp_geo), INTENT(IN), OPTIONAL :: lon(:), lat(:)
REAL(kind=fp_utm), INTENT(IN), OPTIONAL  :: utme(:), utmn(:)
INTEGER, INTENT(IN), OPTIONAL  :: fuso, elliss

IF (.NOT.done_init) CALL init_elliss()
CALL delete(this)
! Inizializza l'oggetto geo_coordvect da un file di poligoni formato SIM
IF (PRESENT(lon) .AND. PRESENT(lat)) THEN
  CALL init(this%desc, fuso, elliss, geoce=.TRUE.)
  this%vsize = MIN(SIZE(lon), SIZE(lat))
  ALLOCATE(this%ll(this%vsize,2))
  this%ll(1:this%vsize,1) = lon(1:this%vsize)
  this%ll(1:this%vsize,2) = lat(1:this%vsize)
  NULLIFY(this%utm)
ELSE IF (PRESENT(utme) .AND. PRESENT(utmn)) THEN
  CALL init(this%desc, fuso, elliss, utmce=.TRUE.)
  NULLIFY(this%ll)
  this%vsize = MIN(SIZE(utme), SIZE(utmn))
  ALLOCATE(this%utm(this%vsize,2))
  this%utm(1:this%vsize,1) = utme(1:this%vsize)
  this%utm(1:this%vsize,2) = utmn(1:this%vsize)
ELSE
  CALL init(this%desc, fuso, elliss)
  this%vsize = 0
  NULLIFY(this%ll)
  NULLIFY(this%utm)
ENDIF

END SUBROUTINE geo_coordvect_init


SUBROUTINE geo_coordvect_delete(this)
TYPE(geo_coordvect), INTENT(INOUT) :: this

CALL delete(this%desc)
IF (ASSOCIATED(this%ll)) DEALLOCATE(this%ll)
IF (ASSOCIATED(this%utm)) DEALLOCATE(this%utm)
this%vsize = 0
this%vtype = 0

END SUBROUTINE geo_coordvect_delete


SUBROUTINE geo_coordvect_getval(this, lon, lat, utme, utmn, fuso, elliss)
TYPE(geo_coordvect),INTENT(IN) :: this
REAL(kind=fp_geo), OPTIONAL, POINTER :: lon(:), lat(:)
REAL(kind=fp_utm), OPTIONAL, POINTER  :: utme(:), utmn(:)
INTEGER, INTENT(OUT), OPTIONAL  :: fuso, elliss

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
IF (PRESENT(utme)) THEN
  IF (ASSOCIATED(this%utm)) THEN
    ALLOCATE(utme(this%vsize))
    utme(:) = this%utm(1:this%vsize,1)
  ENDIF
ENDIF 
IF (PRESENT(utmn)) THEN
  IF (ASSOCIATED(this%utm)) THEN
    ALLOCATE(utmn(this%vsize))
    utmn(:) = this%utm(1:this%vsize,2)
  ENDIF
ENDIF 
CALL getval(this%desc, fuso, elliss)

END SUBROUTINE geo_coordvect_getval


FUNCTION geo_coordvect_equalize(this, that) RESULT(res)
TYPE(geo_coordvect), INTENT (INOUT) :: this, that
LOGICAL :: res
! Tenta di rendere confrontabili due oggetti geo_coordvect convertendo
! opportunamente le coordinate se necessario
! Restituisce true in caso di successo o in caso di dati gia` confrontabili o nulli

IF (this%desc == that%desc) THEN
  res = .TRUE.
  RETURN
ENDIF
IF (this%desc%geoce .AND. that%desc%utmce) THEN
  CALL geo_coordvect_to_geo(that)
ELSE IF (this%desc%utmce .AND. that%desc%geoce) THEN
  CALL geo_coordvect_to_geo(this)
ELSE IF (this%desc%utmce .AND. that%desc%utmce) THEN ! diversi fusi/ellissoidi? siamo fusi!
  CALL geo_coordvect_to_geo(this)
  CALL geo_coordvect_to_utm(this, fuso=that%desc%fuso, elliss=that%desc%elliss)
ENDIF
! controllo se ho avuto successo
res = this%desc == that%desc

END FUNCTION geo_coordvect_equalize


SUBROUTINE geo_coordvect_import(this, unitsim, shphandle, nshp, proj, fuso, elliss)
TYPE(geo_coordvect), INTENT(OUT) :: this
INTEGER,OPTIONAL,INTENT(IN) :: unitsim
INTEGER(kind=ptr_c),OPTIONAL,INTENT(IN) :: shphandle
INTEGER,OPTIONAL,INTENT(IN) :: nshp, proj, fuso, elliss

REAL(kind=fp_geo),ALLOCATABLE :: llon(:), llat(:)
REAL(kind=fp_geo) :: lv1,lv2,lv3,lv4,lproj
INTEGER :: i, lvsize
CHARACTER(len=40) :: lname
#ifdef HAVE_LIBSHP_FORTRAN
TYPE(shpobject),POINTER :: shpobj
#endif

IF (PRESENT(proj)) THEN
  lproj = proj
ELSE
  lproj = 1
ENDIF
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
  IF (lproj == proj_geo) THEN
    CALL init(this, lon=llon(1:lvsize), lat=llat(1:lvsize), &
     fuso=fuso, elliss=elliss)
  ELSE
    CALL init(this, utme=REAL(llon(1:lvsize),kind=fp_utm), &
     utmn=REAL(llat(1:lvsize),kind=fp_utm), fuso=fuso, elliss=elliss)
  ENDIF
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
    IF (lproj == proj_geo) THEN
      CALL init(this, lon=REAL(shpobj%padfx,kind=fp_geo), &
       lat=REAL(shpobj%padfy,kind=fp_geo), fuso=fuso, elliss=elliss)
    ELSE
      CALL init(this, utme=REAL(shpobj%padfx,kind=fp_utm), &
       utmn=REAL(shpobj%padfy,kind=fp_utm), fuso=fuso, elliss=elliss)
    ENDIF
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
    IF (this%desc%geoce) THEN
      WRITE(unitsim,*)(this%ll(i,1:2), i=1,this%vsize)
    ELSE IF (this%desc%utmce) THEN
      WRITE(unitsim,*)(this%utm(i,1:2), i=1,this%vsize)
    ENDIF
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
  IF (this%desc%geoce) THEN
    shpobj => shpcreatesimpleobject(this%vtype, this%vsize, &
     REAL(this%ll(1:this%vsize,1),kind=fp_d), &
     REAL(this%ll(1:this%vsize,2),kind=fp_d))
    ELSE IF (this%desc%utmce) THEN
    shpobj => shpcreatesimpleobject(this%vtype, this%vsize, &
     REAL(this%utm(1:this%vsize,1),kind=fp_d), &
     REAL(this%utm(1:this%vsize,2),kind=fp_d))
    ENDIF
  IF (ASSOCIATED(shpobj)) THEN
    ! Lo scrivo nello shapefile
    i=shpwriteobject(shphandle, lnshp, shpobj)
    CALL shpdestroyobject(shpobj)
  ENDIF
#endif
ENDIF

END SUBROUTINE geo_coordvect_export


SUBROUTINE geo_coordvect_importvect(this, shpfilesim, shpfile, proj, fuso, elliss)
TYPE(geo_coordvect),POINTER :: this(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: shpfilesim, shpfile
INTEGER,OPTIONAL,INTENT(IN) :: proj, fuso, elliss

REAL(kind=fp_geo),ALLOCATABLE :: llon(:), llat(:)
REAL(kind=fp_geo) :: inu
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
      CALL import(this(i), unitsim=u, proj=proj, fuso=fuso, elliss=elliss)
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
      CALL import(this(i), shphandle=shphandle, nshp=i-1, proj=proj, &
       fuso=fuso, elliss=elliss)
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

REAL(kind=fp_geo),ALLOCATABLE :: llon(:), llat(:)
REAL(kind=fp_geo) :: lv1,lv2,lv3,lv4
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
  IF (lappend) THEN
    shphandle = shpopen(TRIM(shpfile), 'r+b')
    IF (shphandle == 0) THEN ! shpopen funziona solo su file esistenti
      shphandle = shpcreate(TRIM(shpfile), geo_coordvect_polygon)
    ENDIF
  ELSE
    shphandle = shpcreate(TRIM(shpfile), geo_coordvect_polygon)
  ENDIF
  IF (shphandle == 0) THEN
    CALL raise_error('Impossibile aprire lo shapefile '//TRIM(shpfile))
    RETURN
  ENDIF
  CALL shpgetinfo(shphandle, ns, shptype, minb, maxb) ! Ottengo le info sul file
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


SUBROUTINE geo_coordvect_to_utm(this, fuso, elliss)
TYPE(geo_coordvect), INTENT (INOUT) :: this
INTEGER, INTENT(IN), OPTIONAL  :: fuso, elliss

INTEGER :: i

IF (.NOT.this%desc%geoce .OR. this%desc%utmce) RETURN ! Niente da fare
CALL init(this%desc, fuso, elliss, utmce=.TRUE.)
ALLOCATE(this%utm(this%vsize,2))

DO i = 1, this%vsize
  CALL ll2utm(this%ll(i,1), this%ll(i,2), this%desc%fuso, this%desc%elliss, &
   this%utm(i,1), this%utm(i,2))
ENDDO

END SUBROUTINE geo_coordvect_to_utm


SUBROUTINE geo_coordvect_to_geo(this)
TYPE(geo_coordvect), INTENT (INOUT) :: this

INTEGER :: i

IF (.NOT.this%desc%utmce .OR. this%desc%geoce) RETURN ! Niente da fare
CALL init(this%desc, geoce=.TRUE.)
ALLOCATE(this%ll(this%vsize,2))

DO i = 1, this%vsize
  CALL utm2ll(this%utm(i,1), this%utm(i,2), this%desc%fuso, this%desc%elliss, &
   this%ll(i,1), this%ll(i,2))
ENDDO

END SUBROUTINE geo_coordvect_to_geo


!!$SUBROUTINE geo_coordvect_add(this, lon, lat, utme, utmn)
!!$TYPE(geo_coordvect), INTENT(INOUT) :: this
!!$REAL(kind=fp_geo), INTENT(IN), OPTIONAL :: lon(:), lat(:)
!!$REAL(kind=fp_utm), INTENT(IN), OPTIONAL  :: utme(:), utmn(:)
!!$
!!$TYPE(geo_coordvect) :: tmp
!!$INTEGER :: newsize
!!$LOGICAL :: must_alloc
!!$
!!$IF (PRESENT(lon) .AND. PRESENT(lat)) THEN
!!$  newsize = SIZE(this%ll,1) + MIN(SIZE(lon), SIZE(lat))
!!$  IF (newsize > this%vsize)
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

! presuppongo che il poligono sia chiuso, altrimenti:
! j = this%vsize; DO i = 1, this%vsize
dentro = .FALSE. 
IF (this%desc%geoce .AND. point%desc%geoce) THEN
  j = 1
  DO i = 2, this%vsize
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
    j = i
  ENDDO
ELSE IF (this%desc%utmce .AND. point%desc%utmce) THEN
  j = 1
  DO i = 2, this%vsize
    IF ((this%utm(i,2) <= point%utmn .AND. &
     point%utmn < this%utm(j,2)) .OR. &
     (this%utm(j,2) <= point%utmn .AND. &
     point%utmn < this%utm(i,2))) THEN
      IF (point%utme < (this%utm(j,1) - this%utm(i,1)) * &
       (point%utmn - this%utm(i,2)) / &
       (this%utm(j,2) - this%utm(i,2)) + this%utm(i,1)) THEN
        dentro = .NOT. dentro
      ENDIF
    ENDIF
    j = i
  ENDDO
ENDIF

END FUNCTION geo_coordvect_dentro


! =================
! == conversione ==
! =================
SUBROUTINE ll2utm(lon, lat, fuso, elliss, utme, utmn)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET
!
! --- CALMET   Version: 5.0       Level: 970825                  LL2UTM
!
! --- PURPOSE:  Converts latitude/longitude to UTM coordinates
!
!           *** Universal Transverse Mercator (UTM) grid system divides
!           *** the world into 60 north-south zones, covering a 6 deg.
!           *** strip of longitude. Zone 1 begins between 180 and 174
!           *** degrees West longitude and progresses eastward to
!           *** zone 60.
!           *** This routine works in both No. & So. Hemispheres
!
! --- INPUTS:
!               this%lat (era RLAT) - Real        - N Latitude in decimal degrees
!                                    (use negative for southern hemisphere)
!               this%lon (era RLON) - Real        - E Longitude in decimal degrees
!                                    (use negative for western hemisphere)
!                IZ0 - Integer     - UTM zone override (used only if
!                                    IZ0 .ne. zero).
!
! --- OUTPUT:
!                  this%utme (era X) - Real        - UTM easting in km
!                  this%utmn (era Y) - Real        - UTM northing in km
!                 this%fuso (era IZ) - Integer     - UTM zone
!
!----------------------------------------------------------------------
!IMPLICIT REAL(kind=fp_utm) (a-h,o-z) ! temporaneo

REAL(kind=fp_geo),INTENT(IN) :: lon, lat
INTEGER,INTENT(INOUT) :: fuso, elliss
REAL(kind=fp_utm),INTENT(OUT) :: utme, utmn
REAL(kind=fp_utm) :: deltalon, p
REAL(kind=fp_utm) :: n, t, t2, c, m, a1, a2, a3, a4, a5, a6, sinp, cosp, tanp

IF (fuso == imiss) THEN
! ---   Locate natural zone
  fuso = INT((180.0_fp_geo+lon)/6.0_fp_geo) + 1
  IF (lat < 0.0_fp_geo) fuso = -fuso
ENDIF

! --- Compute delta longitude in radians
deltalon = dtr*(lon - (6.0_fp_geo*ABS(fuso)-183.0_fp_geo))

! --- Convert phi (latitude) to radians
p = dtr*lat
sinp = SIN(p)
cosp = COS(p)
tanp = TAN(p)

n = a(elliss)/SQRT(1.0_fp_utm - e2(elliss)*sinp*sinp)
t = tanp*tanp
c = ep2(elliss)*cosp*cosp
a1 = deltalon*cosp
!!$m = 111132.0894_fp_utm*lat - 16216.94_fp_utm*SIN(2.0*p) + 17.21_fp_utm*SIN(4.0*p) &
!!$ - 0.02_fp_utm*SIN(6.0*p)
! Modificato rispetto alla routine originale, dipende dall'ellissoide
m = ef0(elliss)*p - ef1(elliss)*SIN(2.0*p) - ef2(elliss)*SIN(4.0*p) - &
 ef3(elliss)*SIN(6.0*p)

a2 = a1**2
a3 = a2*a1
a4 = a2**2
a5 = a4*a1
a6 = a4*a2
t2 = t**2

! --- Compute UTM x and y (km)
utme = k0*n*(a1 + (1.0_fp_utm - t + c)*a3/6.0_fp_utm &
 + (5.0_fp_utm - 18.0_fp_utm*t + t2 + 72.0_fp_utm*c - 58.0_fp_utm*ep2(elliss)) &
 *a5/120.0_fp_utm) + false_e
utmn = k0*(m + n*tanp &
 *(a2/2.0_fp_utm + (5.0_fp_utm - t + 9.0_fp_utm*c + &
 4.0_fp_utm*c*c)*a4/24.0_fp_utm + (61.0_fp_utm - 58.0_fp_utm*t + t2 + &
 600.0_fp_utm*c - 330.0_fp_utm*ep2(elliss))*a6/720.0_fp_utm))
IF (fuso < 0) utmn = utmn + false_n

END SUBROUTINE ll2utm


SUBROUTINE utm2ll(utme, utmn, fuso, elliss, lon, lat)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET
!
! --- CALMET   Version: 5.0       Level: 970825                  UTM2LL
!
! --- PURPOSE:  Converts UTM coordinates to latitude/longitude
!               Works in both Northern & Southern Hemispheres
!
! --- INPUTS:
!                  this%utme (era X) - real    - UTM easting in km
!                  this%utmn (era Y) - real    - UTM northing in km
!                 this%fuso (era IZ) - integer - UTM zone (6 deg N-S strip, range=1,60)
!             LSOHEM - logical - TRUE = southern hemisphere
!                                FALSE = northern hemisphere
!
! --- OUTPUT:
!               this%lat (era RLAT) - real    - N Latitude in decimal degrees
!               this%lon (era RLON) - real    - E Longitude in decimal degrees
!
!----------------------------------------------------------------------
!IMPLICIT REAL(kind=fp_utm) (a-h,o-z) ! temporaneo

REAL(kind=fp_utm),INTENT(IN) :: utme, utmn
INTEGER,INTENT(IN) :: fuso, elliss
REAL(kind=fp_utm),INTENT(OUT) :: lon, lat
REAL(kind=fp_utm) :: rlon0, xm, ym, m, u, p1, c1, c2, t1, t2, n1, &
 sinp1, cosp1, tanp1, sin2p1, r0, r1, d, d2, d3, d4, d5, d6, p, l

! --- Central meridian
rlon0 = ABS(fuso)*6.0_fp_geo - 183.0_fp_geo

! --- Correct for false easting, southern hemisphere and change to meters
xm = utme - false_e
IF (fuso < 0) THEN
  ym = utmn - false_n
ELSE
  ym = utmn
ENDIF

m = ym/k0
u = m/(a(elliss)*(1.0_fp_utm-e2(elliss)/4.0_fp_utm - &
 3.0_fp_utm*e4(elliss)/64.0_fp_utm - 5.0_fp_utm*e6(elliss)/256.0_fp_utm))
p1 = u + e11(elliss)*SIN(2.0_fp_utm*u) + e12(elliss)*SIN(4.0_fp_utm*u) + &
 e13(elliss)*SIN(6.0_fp_utm*u) + e14(elliss)*SIN(8.0_fp_utm*u)
sinp1 = SIN(p1)
cosp1 = COS(p1)
tanp1 = TAN(p1)
c1 = ep2(elliss)*cosp1**2
c2 = c1**2
t1 = tanp1**2
t2 = t1**2
sin2p1 = sinp1**2
n1 = a(elliss)/SQRT(1.0_fp_utm-e2(elliss)*sin2p1)
r0 = 1.0_fp_utm-e2(elliss)*sin2p1
r1 = a(elliss)*(1.0_fp_utm-e2(elliss))/SQRT(r0**3)

d = xm/(n1*k0)
d2=d**2
d3=d*d2
d4=d*d3
d5=d*d4
d6=d*d5

p = p1 - (n1*tanp1/r1) * (d2/2.0_fp_utm &
 - (5.0_fp_utm + 3.0_fp_utm*t1 + 10.0_fp_utm*c1 - 4.0_fp_utm*c2 &
 - 9.0_fp_utm*ep2(elliss))*d4/24.0_fp_utm &
 + (61.0_fp_utm + 90.0_fp_utm*t1 + 298.0_fp_utm*c1 + 45.0_fp_utm*t2 &
 - 252_fp_utm*ep2(elliss) - 3.0_fp_utm*c2)*d6/720.0_fp_utm)
lat = rtd*p
l = (d - (1.0_fp_utm + 2.0_fp_utm*t1 + c1)*d3/6.0_fp_utm &
 + (5.0_fp_utm - 2.0_fp_utm*c1 + 28.0_fp_utm*t1 - 3.0_fp_utm*c2 &
 + 8.0_fp_utm*ep2(elliss) + 24.0_fp_utm*t2)*d5/120.0_fp_utm)/cosp1
lon = rtd*l + rlon0

END SUBROUTINE utm2ll

END MODULE geo_coord_class
