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

!> Classe per la gestione delle variabili osservate da stazioni meteo e affini.
!! Questo modulo definisce una classe per rappresentare variabili meteorologiche
!! osservate, o attributi, aventi diversi tipi numerici o carattere.
!! \ingroup vol7d
MODULE vol7d_var_class
USE kinds
USE missing_values
USE file_utilities
IMPLICIT NONE

!> Definisce una variabile meteorologica osservata o un suo attributo.
!! I membri \a r, \a d, \a i, \a b, \a c servono, internamente a vol7d,
!! per associare le variabili agli attributi, e indicano
!! a quale variabile, nel descrittore delle variabili, coincide
!! la variabile corrente nel descrittore delle "variabili aventi attributo".
!! I membri di \a vol7d_var sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma � comunque consigliato assegnarli tramite
!! il costruttore ::init.
TYPE vol7d_var
  CHARACTER(len=10) :: btable=cmiss !< codice della variabile secondo la tabella B del WMO.
  CHARACTER(len=65) :: description=cmiss !< descrizione testuale della variabile (opzionale)
  CHARACTER(len=24) :: unit=cmiss !< descrizione testuale dell'unit� di misura (opzionale)
  INTEGER :: scalefactor=imiss !< numero di decimali nella rappresentazione intera o character (opzionale)

  INTEGER :: r=imiss !< indice della variabile nel volume degli attributi reali
  INTEGER :: d=imiss !< indice della variabile nel volume degli attributi double precision
  INTEGER :: i=imiss !< indice della variabile nel volume degli attributi integer
  INTEGER :: b=imiss !< indice della variabile nel volume degli attributi byte
  INTEGER :: c=imiss !< indice della variabile nel volume degli attributi character
  INTEGER :: gribhint(4)=imiss !< hint for conversion from/to grib when btable is not found
END TYPE  vol7d_var

!> Valore mancante per vol7d_var.
TYPE(vol7d_var),PARAMETER :: vol7d_var_miss= &
 vol7d_var(cmiss,cmiss,cmiss,imiss,imiss,imiss,imiss,imiss,imiss, &
 (/imiss,imiss,imiss,imiss/))

!> Costruttore per la classe vol7d_var.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE vol7d_var_init
END INTERFACE

!> Distruttore per la classe vol7d_var.
!! Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
INTERFACE delete
  MODULE PROCEDURE vol7d_var_delete
END INTERFACE

!> Operatore logico di uguaglianza tra oggetti della classe vol7d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con pi�
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_var_eq
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della classe vol7d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con pi�
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_var_ne, vol7d_var_nesv
END INTERFACE

!> to be documented
INTERFACE c_e
  MODULE PROCEDURE vol7d_var_c_e
END INTERFACE

#define VOL7D_POLY_TYPE TYPE(vol7d_var)
#define VOL7D_POLY_TYPES _var
#include "array_utilities_pre.F90"

!> \brief display on the screen a brief content of object
INTERFACE display
  MODULE PROCEDURE display_var, display_var_vect
END INTERFACE


TYPE vol7d_var_features
  TYPE(vol7d_var) :: var !< the variable (only btable is relevant)
  REAL :: posdef !< if not missing, minimum physically reasonable value for the variable
  INTEGER :: vartype !< type of variable, one of the var_* constants
END TYPE vol7d_var_features

TYPE(vol7d_var_features),ALLOCATABLE :: var_features(:)

! constants for vol7d_vartype
INTEGER,PARAMETER :: var_ord=0 !< unclassified variable (vol7d_vartype function)
INTEGER,PARAMETER :: var_dir360=1 !< direction in degrees (vol7d_vartype function)
INTEGER,PARAMETER :: var_press=2 !< pressure in Pa (vol7d_vartype function)
INTEGER,PARAMETER :: var_ucomp=3 !< u component of a vector field (vol7d_vartype function)
INTEGER,PARAMETER :: var_vcomp=4 !< v component of a vector field (vol7d_vartype function)
INTEGER,PARAMETER :: var_wcomp=5 !< w component of a vector field (vol7d_vartype function)


CONTAINS

!> Inizializza un oggetto \a vol7d_var con i parametri opzionali forniti.
!! Se non viene passato nessun parametro opzionale l'oggetto �
!! inizializzato a valore mancante.
!! I membri \a r, \a d, \a i, \a b, \a c non possono essere assegnati
!! tramite costruttore, ma solo direttamente.
elemental SUBROUTINE vol7d_var_init(this, btable, description, unit, scalefactor)
TYPE(vol7d_var),INTENT(INOUT) :: this !< oggetto da inizializzare
CHARACTER(len=*),INTENT(in),OPTIONAL :: btable !< codice della variabile
CHARACTER(len=*),INTENT(in),OPTIONAL :: description !< descrizione della variabile
CHARACTER(len=*),INTENT(in),OPTIONAL :: unit !< unit� di misura
INTEGER,INTENT(in),OPTIONAL :: scalefactor !< decimali nella rappresentazione intera e character

IF (PRESENT(btable)) THEN
  this%btable = btable
ELSE
  this%btable = cmiss
  this%description = cmiss
  this%unit = cmiss
  this%scalefactor = imiss
  RETURN
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
if (present(scalefactor)) then
  this%scalefactor = scalefactor
else
  this%scalefactor = imiss
endif

this%r = -1
this%d = -1
this%i = -1
this%b = -1
this%c = -1

END SUBROUTINE vol7d_var_init


ELEMENTAL FUNCTION vol7d_var_new(btable, description, unit, scalefactor) RESULT(this)
CHARACTER(len=*),INTENT(in),OPTIONAL :: btable !< codice della variabile
CHARACTER(len=*),INTENT(in),OPTIONAL :: description !< descrizione della variabile
CHARACTER(len=*),INTENT(in),OPTIONAL :: unit !< unit� di misura
INTEGER,INTENT(in),OPTIONAL :: scalefactor !< decimali nella rappresentazione intera e character

TYPE(vol7d_var) :: this

CALL init(this, btable, description, unit, scalefactor)

END FUNCTION vol7d_var_new


!> Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
elemental SUBROUTINE vol7d_var_delete(this)
TYPE(vol7d_var),INTENT(INOUT) :: this !< oggetto da distruggre

this%btable = cmiss
this%description = cmiss
this%unit = cmiss
this%scalefactor = imiss

END SUBROUTINE vol7d_var_delete


ELEMENTAL FUNCTION vol7d_var_eq(this, that) RESULT(res)
TYPE(vol7d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = this%btable == that%btable

END FUNCTION vol7d_var_eq


ELEMENTAL FUNCTION vol7d_var_ne(this, that) RESULT(res)
TYPE(vol7d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_var_ne


FUNCTION vol7d_var_nesv(this, that) RESULT(res)
TYPE(vol7d_var),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_var_nesv



!> \brief display on the screen a brief content of vol7d_var object
subroutine display_var(this)

TYPE(vol7d_var),INTENT(in) :: this !< vol7d_var object to display

print*,"VOL7DVAR: ",this%btable,trim(this%description)," : ",this%unit,&
 " scale factor",this%scalefactor

end subroutine display_var


!> \brief display on the screen a brief content of vector of vol7d_var object
subroutine display_var_vect(this)

TYPE(vol7d_var),INTENT(in) :: this(:) !< vol7d_var vector object to display
integer :: i

do i=1,size(this)
  call display_var(this(i))
end do

end subroutine display_var_vect

FUNCTION vol7d_var_c_e(this) RESULT(c_e)
TYPE(vol7d_var),INTENT(IN) :: this
LOGICAL :: c_e
c_e = this /= vol7d_var_miss
END FUNCTION vol7d_var_c_e


!> Initialise the global table of variable features.
!! This subroutine reads the table of variable features from an
!! external file and stores it in a global array. It has to be called
!! once at the beginning of the program. At the moment it gives access
!! to the information about type of variable and positive
!! definitness. The table is based on the unique bufr-like variable
!! table. The table is contained in the csv file `vargrib.csv`.
!! It is not harmful to call this subroutine multiple times.
SUBROUTINE vol7d_var_features_init()
INTEGER :: un, i, n
TYPE(csv_record) :: csv
CHARACTER(len=1024) :: line

IF (ALLOCATED(var_features)) RETURN

un = open_package_file('varbufr.csv', filetype_data)
n=0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  n = n + 1
ENDDO

100 CONTINUE

REWIND(un)
ALLOCATE(var_features(n))

DO i = 1, n
  READ(un,'(A)',END=200)line
  CALL init(csv, line)
  CALL csv_record_getfield(csv, var_features(i)%var%btable)
  CALL csv_record_getfield(csv)
  CALL csv_record_getfield(csv)
  CALL csv_record_getfield(csv, var_features(i)%posdef)
  CALL csv_record_getfield(csv, var_features(i)%vartype)
  CALL delete(csv)
ENDDO

200 CONTINUE
CLOSE(un)

END SUBROUTINE vol7d_var_features_init


!> Deallocate the global table of variable features.
!! This subroutine deallocates the table of variable features
!! allocated in the `vol7d_var_features_init` subroutine.
SUBROUTINE vol7d_var_features_delete()
IF (ALLOCATED(var_features)) DEALLOCATE(var_features)
END SUBROUTINE vol7d_var_features_delete


!> Return the physical type of the variable.
!! Returns a rough classification of the variable depending on the
!! physical parameter it represents. The result is one of the
!! constants vartype_* defined in the module. To be extended.
!! In order for this to work, the subroutine \a
!! vol7d_var_features_init has to be preliminary called.
ELEMENTAL FUNCTION vol7d_var_features_vartype(this) RESULT(vartype)
TYPE(vol7d_var),INTENT(in) :: this !< vol7d_var object to be tested
INTEGER :: vartype

INTEGER :: i

vartype = imiss

IF (ALLOCATED(var_features)) THEN
  DO i = 1, SIZE(var_features)
    IF (this == var_features(i)%var) THEN
      vartype = var_features(i)%vartype
      RETURN
    ENDIF
  ENDDO
ENDIF

END FUNCTION vol7d_var_features_vartype


!> Apply a positive definite flag to a variable.
!! This subroutine resets the value of a variable depending on its
!! positive definite flag defined in the associated \a c_func object.
!! The \a c_func object can be obtained for example by the \a convert
!! (interfaced to vargrib2varbufr_convert) function. The value is
!! reset to the maximum between the value itsel and and 0 (or the
!! value set in \a c_func%posdef. These values are set from the
!! vargrib2bufr.csv file.
!! In order for this to work, the subroutine \a
!! vol7d_var_features_init has to be preliminary called.
ELEMENTAL SUBROUTINE vol7d_var_features_posdef_apply(this, val)
TYPE(vol7d_var),INTENT(in) :: this !< vol7d_var object to be reset
REAL,INTENT(inout) :: val !< value to be reset, it is reset in place

INTEGER :: i

IF (ALLOCATED(var_features)) THEN
  DO i = 1, SIZE(var_features)
    IF (this == var_features(i)%var) THEN
      IF (c_e(var_features(i)%posdef)) val = MAX(var_features(i)%posdef, val)
      RETURN
    ENDIF
  ENDDO
ENDIF

END SUBROUTINE vol7d_var_features_posdef_apply


!> Return the physical type of the variable.
!! Returns a rough classification of the variable depending on the
!! physical parameter it represents. The result is one of the
!! constants vartype_* defined in the module. To be extended.
ELEMENTAL FUNCTION vol7d_vartype(this) RESULT(vartype)
TYPE(vol7d_var),INTENT(in) :: this !< vol7d_var object to be tested

INTEGER :: vartype

vartype = var_ord
SELECT CASE(this%btable)
CASE('B01012', 'B11001', 'B11043', 'B22001') ! direction, degree true
  vartype = var_dir360
CASE('B07004', 'B10004', 'B10051', 'B10060') ! pressure, Pa
  vartype = var_press
CASE('B11003', 'B11200') ! u-component
  vartype = var_ucomp
CASE('B11004', 'B11201') ! v-component
  vartype = var_vcomp
CASE('B11005', 'B11006') ! w-component
  vartype = var_wcomp
END SELECT

END FUNCTION vol7d_vartype


#include "array_utilities_inc.F90"


END MODULE vol7d_var_class
