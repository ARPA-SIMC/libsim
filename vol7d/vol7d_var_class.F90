!> Classe per la gestione delle variabili osservate da stazioni meteo e affini.
!! Questo modulo definisce una classe per rappresentare variabili meteorologiche
!! osservate, o attributi, aventi diversi tipi numerici o carattere.
!! \ingroup vol7d
MODULE vol7d_var_class
USE kinds
USE missing_values
IMPLICIT NONE

!> Definisce una variabile meteorologica osservata o un suo attributo.
!! I membri \a r, \a d, \a i, \a b, \a c servono, internamente a vol7d,
!! per associare le variabili agli attributi, e indicano
!! a quale variabile, nel descrittore delle variabili, coincide
!! la variabile corrente nel descrittore delle "variabili aventi attributo".
!! I membri di \a vol7d_var sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma è comunque consigliato assegnarli tramite
!! il costruttore ::init.
TYPE vol7d_var
  CHARACTER(len=10) :: btable !< codice della variabile secondo la tabella B del WMO.
  CHARACTER(len=65) :: description !< descrizione testuale della variabile (opzionale)
  CHARACTER(len=24) :: unit !< descrizione testuale dell'unità di misura (opzionale)
  integer :: scalefactor !< numero di decimali nella rappresentazione intera o character (opzionale)

  INTEGER :: r !< indice della variabile di dati reale che possiede l'attributo corrente
  INTEGER :: d !< indice della variabile di dati a doppia precisione che possiede l'attributo corrente
  INTEGER :: i !< indice della variabile di dati intera che possiede l'attributo corrente
  INTEGER :: b !< indice della variabile di dati byte che possiede l'attributo corrente
  INTEGER :: c !< indice della variabile di dati carattere che possiede l'attributo corrente
END TYPE  vol7d_var

!> Valore mancante per vol7d_var.
TYPE(vol7d_var),PARAMETER :: vol7d_var_miss= &
 vol7d_var(cmiss,cmiss,cmiss,imiss,imiss,imiss,imiss,imiss,imiss)

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
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_var_eq
!!$, vol7d_var_eqsv
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della classe vol7d_var.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_var_ne, vol7d_var_nesv
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

!> \brief display on the screen a brief content of object
INTERFACE display
  MODULE PROCEDURE display_var, display_var_vect
END INTERFACE


CONTAINS

!> Inizializza un oggetto \a vol7d_var con i parametri opzionali forniti.
!! Se non viene passato nessun parametro opzionale l'oggetto è
!! inizializzato a valore mancante.
!! I membri \a r, \a d, \a i, \a b, \a c non possono essere assegnati
!! tramite costruttore, ma solo direttamente.
elemental SUBROUTINE vol7d_var_init(this, btable, description, unit,scalefactor)
TYPE(vol7d_var),INTENT(INOUT) :: this !< oggetto da inizializzare
!INTEGER,INTENT(in),OPTIONAL :: btable
CHARACTER(len=*),INTENT(in),OPTIONAL :: btable !< codice della variabile
CHARACTER(len=20),INTENT(in),OPTIONAL :: description !< descrizione della variabile
CHARACTER(len=20),INTENT(in),OPTIONAL :: unit !< unità di misura
integer,INTENT(in),OPTIONAL :: scalefactor !< decimali nella rappresentazione intera e character

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


!> Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
elemental SUBROUTINE vol7d_var_delete(this)
TYPE(vol7d_var),INTENT(INOUT) :: this !< oggetto da distruggre

this%btable = cmiss
this%description = cmiss
this%unit = cmiss
this%scalefactor = imiss

END SUBROUTINE vol7d_var_delete


elemental FUNCTION vol7d_var_eq(this, that) RESULT(res)
TYPE(vol7d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = this%btable == that%btable

END FUNCTION vol7d_var_eq


!!$FUNCTION vol7d_var_eqsv(this, that) RESULT(res)
!!$TYPE(vol7d_var),INTENT(IN) :: this, that(:)
!!$LOGICAL :: res(SIZE(that))
!!$
!!$INTEGER :: i
!!$
!!$DO i = 1, SIZE(that)
!!$  res(i) = this == that(i)
!!$ENDDO
!!$
!!$END FUNCTION vol7d_var_eqsv


elemental FUNCTION vol7d_var_ne(this, that) RESULT(res)
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

  print*,"VOL7DVAR: ",this(i)%btable,trim(this(i)%description)," : ",trim(this(i)%unit),&
   " scale factor",this(i)%scalefactor

end do

end subroutine display_var_vect



! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(vol7d_var)
#define VOL7D_POLY_TYPES _var
#include "vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_var_class
