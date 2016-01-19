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

!> Classe per la gestione dei livelli verticali in osservazioni meteo e affini.
!! Questo modulo definisce una classe per rappresentare la localizzazione
!! verticale di un'osservazione meteorologica, prendendo in prestito
!! concetti dal formato grib.
!! \ingroup vol7d
MODULE vol7d_level_class
USE kinds
USE missing_values
use char_utilities
IMPLICIT NONE

!> Definisce il livello verticale di un'osservazione.
!! I membri di \a vol7d_level sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma è comunque consigliato assegnarli tramite
!! il costruttore ::init.
TYPE vol7d_level
  INTEGER :: level1 !< tipo di livello o strato verticale (vedi tabella 4.5 formato grib2 WMO http://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI2/LatestVERSION/WMO306_vI2_GRIB2_CodeFlag_en.pdf)
  INTEGER :: l1 !< valore numerico del primo livello, se previsto da \a level1
  INTEGER :: level2 !< tipo di livello o strato verticale (vedi tabella 4.10 formato grib2 WMO http://www.wmo.ch/pages/prog/www/WMOCodes/Operational/GRIB2/FM92-GRIB2-2007Nov.pdf )
  INTEGER :: l2 !< valore numerico del secondo livello, se previsto da \a level2 (in altre parole, se il dato è riferita ad uno strato di spessore finito)
END TYPE  vol7d_level

!> Valore mancante per vol7d_level.
TYPE(vol7d_level),PARAMETER :: vol7d_level_miss=vol7d_level(imiss,imiss,imiss,imiss)

!> Costruttore per la classe vol7d_level.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE vol7d_level_init
END INTERFACE

!> Distruttore per la classe vol7d_level.
!! Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
INTERFACE delete
  MODULE PROCEDURE vol7d_level_delete
END INTERFACE

!> Logical equality operator for objects of \a vol7d_level class.
!! It is defined as \a ELEMENTAL thus it works also with conformal arrays
!! of any shape.
INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_level_eq
END INTERFACE

!> Logical inequality operator for objects of \a vol7d_level class.
!! It is defined as \a ELEMENTAL thus it works also with conformal arrays
!! of any shape.
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_level_ne
END INTERFACE

!> Logical greater-than operator for objects of \a vol7d_level class.
!! It is defined as \a ELEMENTAL thus it works also with conformal arrays
!! of any shape.
!! Comparison is performed first on \a level, then, then on \l1, then
!! on \l2 if defined.
INTERFACE OPERATOR (>)
  MODULE PROCEDURE vol7d_level_gt
END INTERFACE

!> Logical less-than operator for objects of \a vol7d_level class.
!! It is defined as \a ELEMENTAL thus it works also with conformal arrays
!! of any shape.
!! Comparison is performed first on \a level, then, then on \l1, then
!! on \l2 if defined.
INTERFACE OPERATOR (<)
  MODULE PROCEDURE vol7d_level_lt
END INTERFACE

!> Logical greater-equal operator for objects of \a vol7d_level class.
!! It is defined as \a ELEMENTAL thus it works also with conformal arrays
!! of any shape.
!! Comparison is performed first on \a level, then, then on \l1, then
!! on \l2 if defined.
INTERFACE OPERATOR (>=)
  MODULE PROCEDURE vol7d_level_ge
END INTERFACE

!> Logical less-equal operator for objects of \a vol7d_level class.
!! It is defined as \a ELEMENTAL thus it works also with conformal arrays
!! of any shape.
!! Comparison is performed first on \a level, then, then on \l1, then
!! on \l2 if defined.
INTERFACE OPERATOR (<=)
  MODULE PROCEDURE vol7d_level_le
END INTERFACE

!> Logical almost equality operators for objects of the class \a
!! vol7d_level
!! If one component is missing it is not used in comparison
INTERFACE OPERATOR (.almosteq.)
  MODULE PROCEDURE vol7d_level_almost_eq
END INTERFACE


! da documentare in inglese assieme al resto
!> to be documented
INTERFACE c_e
  MODULE PROCEDURE vol7d_level_c_e
END INTERFACE

#define VOL7D_POLY_TYPE TYPE(vol7d_level)
#define VOL7D_POLY_TYPES _level
#define ENABLE_SORT
#include "array_utilities_pre.F90"

!>Print object
INTERFACE display
  MODULE PROCEDURE display_level
END INTERFACE

!>Represent level object in a pretty string
INTERFACE to_char
  MODULE PROCEDURE to_char_level
END INTERFACE

type(vol7d_level) :: almost_equal_levels(3)=(/&
 vol7d_level(  1,imiss,imiss,imiss),&
 vol7d_level(103,imiss,imiss,imiss),&
 vol7d_level(106,imiss,imiss,imiss)/)

TYPE level_var
  INTEGER :: level
  CHARACTER(len=10) :: btable
END TYPE level_var

! Conversion table from GRIB2 vertical level codes to corresponding
! BUFR B table variables, no unit conversion provided since there is
! no need up to now
TYPE(level_var),PARAMETER :: level_var_converter(6) = (/ &
 level_var(20, 'B12101'), & ! isothermal (K)
 level_var(100, 'B10004'), & ! isobaric (Pa)
 level_var(102, 'B10007'), & ! height over sea level (m)
 level_var(103, 'B10007'), & ! height over surface (m) (special treatment needed!)
 level_var(107, 'B12192'), & ! isentropical (K)
 level_var(108, 'B10004') /) ! pressure difference from surface (Pa) (special treatment needed!)

PRIVATE level_var, level_var_converter

CONTAINS

!> Inizializza un oggetto \a vol7d_level con i parametri opzionali forniti.
!! Questa è la versione \c FUNCTION, in stile F2003, del costruttore, da preferire
!! rispetto alla versione \c SUBROUTINE \c init.
!! Se non viene passato nessun parametro opzionale l'oggetto è
!! inizializzato a valore mancante.
FUNCTION vol7d_level_new(level1, l1, level2, l2) RESULT(this)
INTEGER,INTENT(IN),OPTIONAL :: level1 !< type for level 1
INTEGER,INTENT(IN),OPTIONAL :: l1 !< value for level 1
INTEGER,INTENT(IN),OPTIONAL :: level2 !< type for level 2
INTEGER,INTENT(IN),OPTIONAL :: l2 !< value for level 2

TYPE(vol7d_level) :: this !< object to initialize

CALL init(this, level1, l1, level2, l2)

END FUNCTION vol7d_level_new


!> Inizializza un oggetto \a vol7d_level con i parametri opzionali forniti.
!! Se non viene passato nessun parametro opzionale l'oggetto è
!! inizializzato a valore mancante.
SUBROUTINE vol7d_level_init(this, level1, l1, level2, l2)
TYPE(vol7d_level),INTENT(INOUT) :: this !< oggetto da inizializzare
INTEGER,INTENT(IN),OPTIONAL :: level1 !< type for level 1
INTEGER,INTENT(IN),OPTIONAL :: l1 !< value for level 1
INTEGER,INTENT(IN),OPTIONAL :: level2 !< type for level 2
INTEGER,INTENT(IN),OPTIONAL :: l2 !< value for level 2

this%level1 = imiss
this%l1 = imiss
this%level2 = imiss
this%l2 = imiss

IF (PRESENT(level1)) THEN
  this%level1 = level1
ELSE
  RETURN
END IF

IF (PRESENT(l1))  this%l1 = l1

IF (PRESENT(level2)) THEN
  this%level2 = level2
ELSE
  RETURN
END IF

IF (PRESENT(l2))  this%l2 = l2

END SUBROUTINE vol7d_level_init


!> Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
SUBROUTINE vol7d_level_delete(this)
TYPE(vol7d_level),INTENT(INOUT) :: this !< oggetto da distruggre

this%level1 = imiss
this%l1 = imiss
this%level2 = imiss
this%l2 = imiss

END SUBROUTINE vol7d_level_delete


subroutine display_level(this)

TYPE(vol7d_level),INTENT(in) :: this

print*,trim(to_char(this))

end subroutine display_level



character(len=255) function to_char_level(this)

TYPE(vol7d_level),INTENT(in) :: this

#ifdef HAVE_DBALLE
integer :: handle=0

call idba_messaggi(handle,"/dev/null", "w", "BUFR")
call idba_spiegal(handle,this%level1,this%l1,this%level2,this%l2,to_char_level)
call idba_fatto(handle)

to_char_level="LEVEL: "//to_char_level

#else

to_char_level="LEVEL: "//&
 " typelev1:"//trim(to_char(this%level1))//" L1:"//trim(to_char(this%l1))//&
 " typelev2:"//trim(to_char(this%level2))//" L2:"//trim(to_char(this%l2))

#endif

end function to_char_level


ELEMENTAL FUNCTION vol7d_level_eq(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

res = &
 this%level1 == that%level1 .AND. &
 this%level2 == that%level2 .AND. &
 this%l1 == that%l1 .AND. this%l2 == that%l2

END FUNCTION vol7d_level_eq


ELEMENTAL FUNCTION vol7d_level_ne(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_level_ne


ELEMENTAL FUNCTION vol7d_level_almost_eq(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF ( .not. c_e(this%level1) .or. .not. c_e(that%level1) .or. this%level1 == that%level1 .AND. &
     .not. c_e(this%level2) .or. .not. c_e(that%level2) .or. this%level2 == that%level2 .AND. &
     .not. c_e(this%l1) .or. .not. c_e(that%l1) .or. this%l1 == that%l1 .AND. &
     .not. c_e(this%l2) .or. .not. c_e(that%l2) .or. this%l2 == that%l2) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_almost_eq


ELEMENTAL FUNCTION vol7d_level_gt(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (&
 this%level1 > that%level1 .OR. &
 (this%level1 == that%level1 .AND. this%l1 > that%l1) .OR. &
 (this%level1 == that%level1 .AND. this%l1 == that%l1 .AND. &
 (&
 this%level2 > that%level2 .OR. &
 (this%level2 == that%level2 .AND. this%l2 > that%l2) &
 ))) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_gt


ELEMENTAL FUNCTION vol7d_level_lt(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (&
 this%level1 < that%level1 .OR. &
 (this%level1 == that%level1 .AND. this%l1 < that%l1) .OR. &
 (this%level1 == that%level1 .AND. this%l1 == that%l1 .AND. &
 (&
 this%level2 < that%level2 .OR. &
 (this%level2 == that%level2 .AND. this%l2 < that%l2) &
 ))) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_lt


ELEMENTAL FUNCTION vol7d_level_ge(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this > that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_ge


ELEMENTAL FUNCTION vol7d_level_le(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this < that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_le


ELEMENTAL FUNCTION vol7d_level_c_e(this) RESULT(c_e)
TYPE(vol7d_level),INTENT(IN) :: this
LOGICAL :: c_e
c_e = this /= vol7d_level_miss
END FUNCTION vol7d_level_c_e


#include "array_utilities_inc.F90"


FUNCTION vol7d_level_to_var(level) RESULT(btable)
TYPE(vol7d_level),INTENT(in) :: level
CHARACTER(len=10) :: btable

INTEGER :: i

DO i = 1, SIZE(level_var_converter)
  IF (level_var_converter(i)%level == level%level1) THEN
    btable = level_var_converter(i)%btable
    RETURN
  ENDIF
ENDDO

btable = cmiss

END FUNCTION vol7d_level_to_var


END MODULE vol7d_level_class
