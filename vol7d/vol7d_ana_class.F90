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


elemental FUNCTION vol7d_ana_eq(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%coord == that%coord .AND. this%ident == that%ident) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

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


#define VOL7D_POLY_TYPE TYPE(vol7d_ana)
#define VOL7D_POLY_TYPES _ana
#include "vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_ana_class
