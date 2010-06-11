#include "config.h"

!> Class for managing physical variables in a grib 1/2 fashion.
!! This module defines a class which can represent Earth-science
!! related physical variables, following the classification scheme
!! adopted by WMO for grib1 and grib2 parameter definition.
!!
!! \ingroup volgrid6d
MODULE volgrid6d_var_class
USE kinds
USE missing_values
use err_handling

IMPLICIT NONE

!> Definition of a physical variable.
!! \a volgrid6d_var members are public, thus they can be freely
!! altered, but it is advisable to set them through the
!! volgrid6d_var_class::init constructor.
TYPE volgrid6d_var
  integer :: centre !< centre
  integer :: category !< grib2: category / grib1: grib table version number
  integer :: number !< parameter number
  integer :: discipline !< grib2: discipline
  CHARACTER(len=65) :: description !< textual description of the variable (optional)
  CHARACTER(len=24) :: unit !< textual description of the variable's unit (optional)
END TYPE  volgrid6d_var

TYPE(volgrid6d_var),PARAMETER :: volgrid6d_var_miss= &
 volgrid6d_var(imiss,imiss,imiss,imiss,cmiss,cmiss) !< missing value volgrid6d_var.

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


!> Operatore logico di uguaglianza tra oggetti della classe volgrid6d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE volgrid6d_var_eq
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della classe volgrid6d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE volgrid6d_var_ne
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

INTERFACE display
  MODULE PROCEDURE display_volgrid6d_var
END INTERFACE

CONTAINS

ELEMENTAL FUNCTION volgrid6d_var_new(centre, category, number, &
 discipline, description, unit) RESULT(this)
integer,INTENT(in),OPTIONAL :: centre !< centre 
integer,INTENT(in),OPTIONAL :: category !< grib2: category / grib1: grib table version number
integer,INTENT(in),OPTIONAL :: number !< parameter number
integer,INTENT(in),OPTIONAL :: discipline !< grib2: discipline
CHARACTER(len=65),INTENT(in),OPTIONAL :: description !< textual description of the variable
CHARACTER(len=24),INTENT(in),OPTIONAL :: unit !< textual description of the variable's unit

TYPE(volgrid6d_var) :: this !< object to be initialised

CALL init(this, centre, category, number, discipline, description, unit)

END FUNCTION volgrid6d_var_new


!> Inizializza un oggetto \a volgrid6d_var con i parametri opzionali forniti.
!! Se non viene passato un parametro opzionale l'oggetto è
!! inizializzato con quel parametro e tutti i successivi a valore mancante.
!! Per il grib1 omettere discipline che verrà impostato a 255 (missing del grib2)
elemental SUBROUTINE volgrid6d_var_init(this, centre, category, number, discipline,description,unit)
TYPE(volgrid6d_var),INTENT(INOUT) :: this !< oggetto da inizializzare
integer,INTENT(in),OPTIONAL :: centre !< centre 
integer,INTENT(in),OPTIONAL :: category !< grib2: categoria / grib1: grib table version number
integer,INTENT(in),OPTIONAL :: number !< parameter number
integer,INTENT(in),OPTIONAL :: discipline !< grib2: disciplina
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

!> operatore di uguaglianza tra oggetti volgrid6d_var
!!centre is tested for local definition only
elemental FUNCTION volgrid6d_var_eq(this, that) RESULT(res)
!> oggetti da comparare
TYPE(volgrid6d_var),INTENT(IN) :: this, that
LOGICAL :: res

if  ( this%discipline == that%discipline )then

  if ( this%discipline == 255 )then
                                !grib1
     res= this%category == that%category .and. &
     this%number == that%number

    if ( (this%category >= 128 .and. this%category <= 254) .or. &
     (this%number >= 128 .and. this%number <= 254) )then
      res = res .and. this%centre == that%centre           !local definition (centre is important)
    end if

  else
                                !grib2
    res = this%category == that%category .and. &
     this%number == that%number

    if ( (this%discipline >= 192 .and. this%discipline <= 254) .or. &
     (this%category >= 192 .and. this%category <= 254) .or. &
     (this%number >= 192 .and. this%number <= 254) )then
      res = res .and. this%centre == that%centre           !local definition (centre is important)
    end if

  end if

else

  ! one is grib1 and other is grib2 or different discipline
  res=.false.

end if



END FUNCTION volgrid6d_var_eq


!> operatore di disuguaglianza tra oggetti volgrid6d_var
elemental FUNCTION volgrid6d_var_ne(this, that) RESULT(res)
!> oggetti da comparare
TYPE(volgrid6d_var),INTENT(IN) :: this, that

LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION volgrid6d_var_ne


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(volgrid6d_var)
#define VOL7D_POLY_TYPES _var
#include "../vol7d/vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


!> \brief display on the screen a brief content of volgrid6d_var object
subroutine display_volgrid6d_var(this)

TYPE(volgrid6d_var),INTENT(in) :: this !< volgrid6d_var object to display

print*,"GRIDVAR: ",this%centre,this%discipline,this%category,this%number

end subroutine display_volgrid6d_var


END MODULE volgrid6d_var_class
