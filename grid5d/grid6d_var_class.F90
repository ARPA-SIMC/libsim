!> Classe per la gestione delle variabili da grib.
!! Questo modulo definisce una classe per rappresentare variabili meteorologiche.
!! \ingroup grid6d
MODULE grid6d_var_class
USE kinds
USE missing_values
IMPLICIT NONE

!> Definisce una variabile meteorologica osservata.
!! I membri di \a grid6d_var sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma è comunque consigliato assegnarli tramite
!! il costruttore ::init.
TYPE grid6d_var

  integer :: centre !< codice della variabile secondo la tabella B del WMO.
  integer :: discipline
  integer :: category
  integer :: number
  CHARACTER(len=65) :: description !< descrizione testuale della variabile (opzionale)
  CHARACTER(len=24) :: unit !< descrizione testuale dell'unità di misura (opzionale)

END TYPE  grid6d_var

!> Valore mancante per grid6d_var.
TYPE(grid6d_var),PARAMETER :: grid6d_var_miss= &
 grid6d_var(imiss,imiss,imiss,imiss,cmiss,cmiss)

!> Costruttore per la classe grid6d_var.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE grid6d_var_init
END INTERFACE

!> Distruttore per la classe grid6d_var.
!! Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
INTERFACE delete
  MODULE PROCEDURE grid6d_var_delete
END INTERFACE

!> Operatore logico di uguaglianza tra oggetti della classe grid6d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE grid6d_var_eq, grid6d_var_eqsv
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della classe grid6d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE grid6d_var_ne, grid6d_var_nesv
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

!> Inizializza un oggetto \a grid6d_var con i parametri opzionali forniti.
!! Se non viene passato un parametro opzionale l'oggetto è
!! inizializzato con quel parametro e tutti i successivi a valore mancante.
!! Per il grib1 omettere discipline che verrà impostato a 255 (missing del grib2)
SUBROUTINE grid6d_var_init(this, centre, category, number, discipline,description,unit)
TYPE(grid6d_var),INTENT(INOUT) :: this !< oggetto da inizializzare
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



END SUBROUTINE grid6d_var_init


!> Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
SUBROUTINE grid6d_var_delete(this)
TYPE(grid6d_var),INTENT(INOUT) :: this !< oggetto da distruggre

this%centre = imiss
this%category = imiss
this%number = imiss
this%discipline = imiss
this%description = cmiss
this%unit = cmiss

END SUBROUTINE grid6d_var_delete


elemental FUNCTION grid6d_var_eq(this, that) RESULT(res)
TYPE(grid6d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = this%centre == that%centre .and. &
 this%category == that%category .and. &
 this%centre == that%centre .and. &
 this%discipline == that%discipline
 

END FUNCTION grid6d_var_eq


FUNCTION grid6d_var_eqsv(this, that) RESULT(res)
TYPE(grid6d_var),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION grid6d_var_eqsv


elemental FUNCTION grid6d_var_ne(this, that) RESULT(res)
TYPE(grid6d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION grid6d_var_ne


FUNCTION grid6d_var_nesv(this, that) RESULT(res)
TYPE(grid6d_var),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION grid6d_var_nesv


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(grid6d_var)
#define VOL7D_POLY_TYPES _var
#include "../vol7d/vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE grid6d_var_class
