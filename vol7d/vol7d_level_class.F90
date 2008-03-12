!> Classe per la gestione dei livelli verticali in osservazioni meteo e affini.
!! Questo modulo definisce una classe per rappresentare la localizzazione
!! verticale di un'osservazione meteorologica, prendendo in prestito
!! concetti dal formato grib.
!! \ingroup vol7d
MODULE vol7d_level_class
USE kinds
USE missing_values
IMPLICIT NONE

!> Definisce il livello verticale di un'osservazione.
!! I membri di \a vol7d_level sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma è comunque consigliato assegnarli tramite
!! il costruttore ::init.
TYPE vol7d_level
  INTEGER :: level1 !< tipo di livello o strato verticale (vedi tabella 4.10 formato grib2 WMO http://www.wmo.ch/pages/prog/www/WMOCodes/Operational/GRIB2/FM92-GRIB2-2007Nov.pdf )
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

!> Operatore logico di uguaglianza tra oggetti della classe vol7d_level.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_level_eq, vol7d_level_eqsv
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della classe vol7d_level.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_level_ne, vol7d_level_nesv
END INTERFACE

!> Operatore logico maggiore tra oggetti della classe vol7d_level.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
!! Il confronto è fatto sui valori di \a level e, a parità di \a level,
!! su \a l1 e \a l2 se definiti.
INTERFACE OPERATOR (>)
  MODULE PROCEDURE vol7d_level_gt, vol7d_level_gtsv
END INTERFACE

!> Operatore logico minore tra oggetti della classe vol7d_level.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
!! Il confronto è fatto sui valori di \a level e, a parità di \a level,
!! su \a l1 e \a l2 se definiti.
INTERFACE OPERATOR (<)
  MODULE PROCEDURE vol7d_level_lt, vol7d_level_ltsv
END INTERFACE

!> Operatore logico maggiore-uguale tra oggetti della classe vol7d_level.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
!! Il confronto è fatto sui valori di \a level e, a parità di \a level,
!! su \a l1 e \a l2 se definiti.
INTERFACE OPERATOR (>=)
  MODULE PROCEDURE vol7d_level_ge, vol7d_level_gesv
END INTERFACE

!> Operatore logico minore-uguale tra oggetti della classe vol7d_level.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
!! Il confronto è fatto sui valori di \a level e, a parità di \a level,
!! su \a l1 e \a l2 se definiti.
INTERFACE OPERATOR (<=)
  MODULE PROCEDURE vol7d_level_le, vol7d_level_lesv
END INTERFACE

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_level
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_level
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_level
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_level
END INTERFACE

INTERFACE index
  MODULE PROCEDURE index_level
END INTERFACE

CONTAINS

!> Inizializza un oggetto \a vol7d_level con i parametri opzionali forniti.
!! Se non viene passato nessun parametro opzionale l'oggetto è
!! inizializzato a valore mancante.
SUBROUTINE vol7d_level_init(this, level1, l1, level2, l2)
TYPE(vol7d_level),INTENT(INOUT) :: this !< oggetto da inizializzare
INTEGER,INTENT(IN),OPTIONAL :: level1 !< tipo di livello 1
INTEGER,INTENT(IN),OPTIONAL :: l1 !< valore per il primo livello
INTEGER,INTENT(IN),OPTIONAL :: level2 !< tipo di livello 2
INTEGER,INTENT(IN),OPTIONAL :: l2 !< valore per il secondo livello

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

this%level2 = imiss
this%l1 = imiss
this%level2 = imiss
this%l2 = imiss

END SUBROUTINE vol7d_level_delete


elemental FUNCTION vol7d_level_eq(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF ( &
 this%level1 == that%level1 .AND. &
 this%level2 == that%level2 .AND. &
 this%l1 == that%l1 .AND. this%l2 == that%l2) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_eq


FUNCTION vol7d_level_eqsv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION vol7d_level_eqsv


elemental FUNCTION vol7d_level_ne(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_level_ne


FUNCTION vol7d_level_nesv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_level_nesv


elemental FUNCTION vol7d_level_gt(this, that) RESULT(res)
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


FUNCTION vol7d_level_gtsv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this > that(i)
ENDDO

END FUNCTION vol7d_level_gtsv


elemental FUNCTION vol7d_level_lt(this, that) RESULT(res)
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


FUNCTION vol7d_level_ltsv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this < that(i)
ENDDO

END FUNCTION vol7d_level_ltsv


elemental FUNCTION vol7d_level_ge(this, that) RESULT(res)
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


FUNCTION vol7d_level_gesv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this >= that(i)
ENDDO

END FUNCTION vol7d_level_gesv


elemental FUNCTION vol7d_level_le(this, that) RESULT(res)
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


FUNCTION vol7d_level_lesv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this <= that(i)
ENDDO

END FUNCTION vol7d_level_lesv


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(vol7d_level)
#define VOL7D_POLY_TYPES _level
#include "vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_level_class
