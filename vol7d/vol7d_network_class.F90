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

!> Classe per la gestione delle reti di stazioni per osservazioni meteo e affini.
!! Questo modulo definisce una classe per identificare la rete
!! a cui appartiene una stazione. Per rete si intende un insieme di stazioni
!! omogenee per tipo di sensori, tipo di variabili osservate,
!! frequenza delle osservazioni, formato dei dati.
!! \ingroup vol7d
MODULE vol7d_network_class
USE kinds
USE missing_values
use char_utilities
IMPLICIT NONE

integer, parameter :: network_name_len=20

!> Definisce la rete a cui appartiene una stazione.
!! I membri di \a vol7d_network sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma è comunque consigliato assegnarli tramite
!! il costruttore ::init.
TYPE vol7d_network
  character(len=network_name_len) :: name !<  Mnemonic alias for type of report
END TYPE vol7d_network

!> Valore mancante per vol7d_network.
TYPE(vol7d_network),PARAMETER :: vol7d_network_miss=vol7d_network(cmiss)

!> Costruttore per la classe vol7d_network.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE vol7d_network_init
END INTERFACE

!> Distruttore per la classe vol7d_network.
!! Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
INTERFACE delete
  MODULE PROCEDURE vol7d_network_delete
END INTERFACE

!> Operatore logico di uguaglianza tra oggetti della classe vol7d_network.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_network_eq, vol7d_network_eqsv
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della classe vol7d_network.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_network_ne, vol7d_network_nesv
END INTERFACE

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_network
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_network
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_network
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_network
END INTERFACE

INTERFACE index
  MODULE PROCEDURE index_network
END INTERFACE

!>Print object
INTERFACE display
  MODULE PROCEDURE display_network
END INTERFACE

!>Check object presence
INTERFACE c_e
  MODULE PROCEDURE c_e_network
END INTERFACE

!>return network object in a pretty string
INTERFACE to_char
  MODULE PROCEDURE to_char_network
END INTERFACE

CONTAINS

!> Inizializza un oggetto \a vol7d_network con i parametri opzionali forniti.
!! Se non viene passato nessun parametro opzionale l'oggetto è
!! inizializzato a valore mancante.
SUBROUTINE vol7d_network_init(this, name)
TYPE(vol7d_network),INTENT(INOUT) :: this !< oggetto da inizializzare
character(len=*),INTENT(in),optional :: name !<  Mnemonic alias for type of report

IF (PRESENT(name)) THEN
  this%name = lowercase(name)
ELSE
  this%name = cmiss
END IF

END SUBROUTINE vol7d_network_init


!> Distrugge l'oggetto in maniera pulita, assegnandogli un valore mancante.
SUBROUTINE vol7d_network_delete(this)
TYPE(vol7d_network),INTENT(INOUT) :: this !< oggetto da distruggre

this%name = cmiss

END SUBROUTINE vol7d_network_delete


subroutine display_network(this)

TYPE(vol7d_network),INTENT(in) :: this

print*,to_char_network(this)

end subroutine display_network


elemental function c_e_network(this) result(res)

TYPE(vol7d_network),INTENT(in) :: this
logical :: res

res = .not. this == vol7d_network_miss

end function c_e_network


elemental character(len=20) function to_char_network(this)

TYPE(vol7d_network),INTENT(in) :: this

to_char_network="Network: "//trim(this%name)

return

end function to_char_network



elemental FUNCTION vol7d_network_eq(this, that) RESULT(res)
TYPE(vol7d_network),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%name == that%name) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_network_eq


FUNCTION vol7d_network_eqsv(this, that) RESULT(res)
TYPE(vol7d_network),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION vol7d_network_eqsv


elemental FUNCTION vol7d_network_ne(this, that) RESULT(res)
TYPE(vol7d_network),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_network_ne


FUNCTION vol7d_network_nesv(this, that) RESULT(res)
TYPE(vol7d_network),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_network_nesv


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(vol7d_network)
#define VOL7D_POLY_TYPES _network
#include "array_utilities_inc.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_network_class
