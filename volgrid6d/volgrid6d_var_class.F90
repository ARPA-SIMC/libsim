!> Classe per la gestione delle variabili da grib.
!! Questo modulo definisce una classe per rappresentare variabili meteorologiche.
!! \ingroup volgrid6d
MODULE volgrid6d_var_class
USE kinds
USE missing_values
use grib_api

IMPLICIT NONE

!> Definisce una variabile meteorologica osservata.
!! I membri di \a volgrid6d_var sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma è comunque consigliato assegnarli tramite
!! il costruttore ::init.
TYPE volgrid6d_var

  integer :: centre !< codice della variabile secondo la tabella B del WMO.
  integer :: discipline
  integer :: category
  integer :: number
  CHARACTER(len=65) :: description !< descrizione testuale della variabile (opzionale)
  CHARACTER(len=24) :: unit !< descrizione testuale dell'unità di misura (opzionale)

END TYPE  volgrid6d_var

!> Valore mancante per volgrid6d_var.
TYPE(volgrid6d_var),PARAMETER :: volgrid6d_var_miss= &
 volgrid6d_var(imiss,imiss,imiss,imiss,cmiss,cmiss)

!> Costruttore per la classe volgrid6d_var.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE volgrid6d_var_init
END INTERFACE

!> Distruttore per la classe volgrid6d_var.
!! Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
INTERFACE delete
  MODULE PROCEDURE volgrid6d_var_delete
END INTERFACE


!> Import per la classe volgrid6d_var.
!! Legge i valori dal grib e li imposta appropriatamente
INTERFACE import
  MODULE PROCEDURE import_volgrid6d_var
END INTERFACE

!> Export per la classe volgrid6d_var.
!! Imposta i valori nel grib
INTERFACE export
  MODULE PROCEDURE export_volgrid6d_var
END INTERFACE


!> Operatore logico di uguaglianza tra oggetti della classe volgrid6d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE volgrid6d_var_eq, volgrid6d_var_eqsv
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della classe volgrid6d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE volgrid6d_var_ne, volgrid6d_var_nesv
END INTERFACE

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_var
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_var
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_var
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_var
END INTERFACE

INTERFACE index
  MODULE PROCEDURE index_var
END INTERFACE

CONTAINS

!> Inizializza un oggetto \a volgrid6d_var con i parametri opzionali forniti.
!! Se non viene passato un parametro opzionale l'oggetto è
!! inizializzato con quel parametro e tutti i successivi a valore mancante.
!! Per il grib1 omettere discipline che verrà impostato a 255 (missing del grib2)
SUBROUTINE volgrid6d_var_init(this, centre, category, number, discipline,description,unit)
TYPE(volgrid6d_var),INTENT(INOUT) :: this !< oggetto da inizializzare
!INTEGER,INTENT(in),OPTIONAL :: btable

integer,INTENT(in),OPTIONAL :: centre !< codice della variabile secondo la tabella B del WMO.
integer,INTENT(in),OPTIONAL :: category
integer,INTENT(in),OPTIONAL :: number
integer,INTENT(in),OPTIONAL :: discipline
CHARACTER(len=65),INTENT(in),OPTIONAL :: description !< descrizione testuale della variabile (opzionale)
CHARACTER(len=24),INTENT(in),OPTIONAL :: unit !< descrizione testuale dell'unità di misura (opzionale)

IF (PRESENT(centre)) THEN
  this%centre = centre
ELSE
  this%centre = imiss
  this%category = imiss
  this%number = imiss
  this%discipline = imiss
  RETURN
ENDIF

IF (PRESENT(category)) THEN
  this%category = category
ELSE
  this%category = imiss
  this%number = imiss
  this%discipline = imiss
  RETURN
ENDIF


IF (PRESENT(number)) THEN
  this%number = number
ELSE
  this%number = imiss
  this%discipline = imiss
  RETURN
ENDIF

! se sono arrivato fino a qui ho impostato centre, category e number
!per il grib 1 manca discipline e imposto 255 (missing del grib2) 

IF (PRESENT(discipline)) THEN
  this%discipline = discipline
ELSE
  this%discipline = 255
ENDIF

IF (PRESENT(description)) THEN
  this%description = description
ELSE
  this%description = cmiss
ENDIF

IF (PRESENT(unit)) THEN
  this%unit = unit
ELSE
  this%unit = cmiss
ENDIF



END SUBROUTINE volgrid6d_var_init


!> Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
SUBROUTINE volgrid6d_var_delete(this)
TYPE(volgrid6d_var),INTENT(INOUT) :: this !< oggetto da distruggre

this%centre = imiss
this%category = imiss
this%number = imiss
this%discipline = imiss
this%description = cmiss
this%unit = cmiss

END SUBROUTINE volgrid6d_var_delete


elemental FUNCTION volgrid6d_var_eq(this, that) RESULT(res)
TYPE(volgrid6d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = this%centre == that%centre .and. &
 this%category == that%category .and. &
 this%centre == that%centre .and. &
 this%discipline == that%discipline
 

END FUNCTION volgrid6d_var_eq


FUNCTION volgrid6d_var_eqsv(this, that) RESULT(res)
TYPE(volgrid6d_var),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION volgrid6d_var_eqsv


elemental FUNCTION volgrid6d_var_ne(this, that) RESULT(res)
TYPE(volgrid6d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION volgrid6d_var_ne


FUNCTION volgrid6d_var_nesv(this, that) RESULT(res)
TYPE(volgrid6d_var),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION volgrid6d_var_nesv


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(volgrid6d_var)
#define VOL7D_POLY_TYPES _var
#include "../vol7d/vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES



subroutine import_volgrid6d_var(this,gaid)

TYPE(volgrid6d_var),INTENT(out) :: this
integer,INTENT(in)              :: gaid
integer ::EditionNumber,centre,discipline,category,number

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  call grib_get(gaid,'identificationOfOriginatingGeneratingCentre',centre)
  call grib_get(gaid,'gribTablesVersionNo',category)
  call grib_get(gaid,'indicatorOfParameter',number)

  call init (this, centre, category, number)

else if (EditionNumber == 1)then

  call grib_get(gaid,'identificationOfOriginatingGeneratingCentre',centre)
  call grib_get(gaid,'discipline',discipline)
  call grib_get(gaid,'parameterCategory',category)
  call grib_get(gaid,'parameterNumber',number)

  call init (this, centre, category, number, discipline)
  
else

  CALL raise_error('GribEditionNumber not supported')

end if
                                ! da capire come ottenere 
!this%description
!this%unit


end subroutine import_volgrid6d_var



subroutine export_volgrid6d_var(this,gaid)

TYPE(volgrid6d_var),INTENT(in) :: this
integer,INTENT(in)             :: gaid
integer ::EditionNumber

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  call grib_set(gaid,'identificationOfOriginatingGeneratingCentre',this%centre)
  call grib_set(gaid,'gribTablesVersionNo',this%category)
  call grib_set(gaid,'indicatorOfParameter',this%number)

else if (EditionNumber == 2)then

  call grib_set(gaid,'identificationOfOriginatingGeneratingCentre',this%centre)
  call grib_set(gaid,'discipline',this%discipline)
  call grib_set(gaid,'parameterCategory',this%category)
  call grib_set(gaid,'parameterNumber',this%number)

else

  CALL raise_error('GribEditionNumber not supported')

end if
                                ! da capire come ottenere 
!this%description
!this%unit


end subroutine export_volgrid6d_var


END MODULE volgrid6d_var_class
