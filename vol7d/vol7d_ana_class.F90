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

!> Classe per la gestione dell'anagrafica di stazioni meteo e affini.
!! Questo modulo definisce una classe in grado di rappresentare
!! le caratteristiche di una stazione meteo fissa o mobile.
!! \ingroup vol7d
MODULE vol7d_ana_class
USE kinds
USE missing_values
USE geo_coord_class
IMPLICIT NONE

!> Lunghezza della stringa che indica l'identificativo del volo.
INTEGER,PARAMETER :: vol7d_ana_lenident=20

!> Definisce l'anagrafica di una stazione.
!! I membri di \a vol7d_ana sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma è comunque consigliato assegnarli tramite
!! il costruttore ::init.
TYPE vol7d_ana
  TYPE(geo_coord) :: coord !< coordinata per una stazione fissa
  CHARACTER(len=vol7d_ana_lenident) :: ident !< identificativo per una stazione mobile (es. aereo)
END TYPE  vol7d_ana

! Deve essere dichiarata PARAMETER, non e` cosi` per un bug del pgi
!> Valore mancante per vo7d_ana.
TYPE(vol7d_ana) :: vol7d_ana_miss=vol7d_ana(geo_coord_miss,cmiss)

!> Costruttore per la classe vol7d_ana.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE vol7d_ana_init
END INTERFACE

!> Distruttore per la classe vol7d_ana.
!! Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
INTERFACE delete
  MODULE PROCEDURE vol7d_ana_delete
END INTERFACE

!> Operatore logico di uguaglianza tra oggetti della classe vol7d_ana.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_ana_eq, vol7d_ana_eqsv
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della classe vol7d_ana.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_ana_ne, vol7d_ana_nesv
END INTERFACE

!> Legge un oggetto vol7d_ana o un vettore di oggetti vol7d_ana da
!! un file \c FORMATTED o \c UNFORMATTED.
INTERFACE read_unit
  MODULE PROCEDURE vol7d_ana_read_unit, vol7d_ana_vect_read_unit
END INTERFACE

!> Scrive un oggetto vol7d_ana o un vettore di oggetti vol7d_ana su
!! un file \c FORMATTED o \c UNFORMATTED.
INTERFACE write_unit
  MODULE PROCEDURE vol7d_ana_write_unit, vol7d_ana_vect_write_unit
END INTERFACE

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_ana
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_ana
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_ana
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_ana
END INTERFACE

INTERFACE index
  MODULE PROCEDURE index_ana
END INTERFACE

!>Print object
INTERFACE display
  MODULE PROCEDURE display_ana
END INTERFACE

CONTAINS

!> Inizializza un oggetto \a vol7d_ana con i parametri opzionali forniti.
!! Se non viene passato nessun parametro opzionale l'oggetto è
!! inizializzato a valore mancante.
SUBROUTINE vol7d_ana_init(this, lon, lat, ident)
TYPE(vol7d_ana),INTENT(INOUT) :: this !< oggetto da inizializzare
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lon !< longitudine
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lat !< latitudine
CHARACTER(len=vol7d_ana_lenident),INTENT(in),OPTIONAL :: ident !< identificativo del volo

CALL init(this%coord, lon=lon, lat=lat)
IF (PRESENT(ident)) THEN
  this%ident = ident
ELSE
  this%ident = cmiss
ENDIF

END SUBROUTINE vol7d_ana_init


!> Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
SUBROUTINE vol7d_ana_delete(this)
TYPE(vol7d_ana),INTENT(INOUT) :: this !< oggetto da distruggre

CALL delete(this%coord)
this%ident = cmiss

END SUBROUTINE vol7d_ana_delete


subroutine display_ana(this)

TYPE(vol7d_ana),INTENT(in) :: this
doubleprecision :: lon,lat

call getval(this%coord,lon=lon,lat=lat)
print*,"ANA: ",this%ident,lon,lat

end subroutine display_ana


elemental FUNCTION vol7d_ana_eq(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that
LOGICAL :: res

res = this%coord == that%coord .AND. this%ident == that%ident

END FUNCTION vol7d_ana_eq


FUNCTION vol7d_ana_eqsv(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION vol7d_ana_eqsv


elemental FUNCTION vol7d_ana_ne(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_ana_ne


FUNCTION vol7d_ana_nesv(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_ana_nesv


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE vol7d_ana_read_unit(this, unit)
TYPE(vol7d_ana),INTENT(out) :: this !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit

CALL vol7d_ana_vect_read_unit((/this/), unit)

END SUBROUTINE vol7d_ana_read_unit


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE vol7d_ana_vect_read_unit(this, unit)
TYPE(vol7d_ana) :: this(:) !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit

CHARACTER(len=40) :: form

CALL read_unit(this%coord, unit)
INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,'(A)')this(:)%ident
ELSE
  READ(unit)this(:)%ident
ENDIF

END SUBROUTINE vol7d_ana_vect_read_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE vol7d_ana_write_unit(this, unit)
TYPE(vol7d_ana),INTENT(in) :: this !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

CALL vol7d_ana_vect_write_unit((/this/), unit)

END SUBROUTINE vol7d_ana_write_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE vol7d_ana_vect_write_unit(this, unit)
TYPE(vol7d_ana),INTENT(in) :: this(:) !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

CHARACTER(len=40) :: form

CALL write_unit(this%coord, unit)
INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,'(A)')this(:)%ident
ELSE
  WRITE(unit)this(:)%ident
ENDIF

END SUBROUTINE vol7d_ana_vect_write_unit


#define VOL7D_POLY_TYPE TYPE(vol7d_ana)
#define VOL7D_POLY_TYPES _ana
#include "vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_ana_class
