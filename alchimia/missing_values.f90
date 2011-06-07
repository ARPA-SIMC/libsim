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
!> Valori mancanti.
!! Questo modulo fornisce strumenti per gestire i valori mancanti per
!! vari tipi di dati.  È importante usare le costanti del tipo giusto
!! per i propri dati per evitare conversioni automatiche che darebbero
!! risultati errati.
!!
!! Attenzione alla differenza filosofica tra i tipi reali a singola o
!! doppia precisione di default della piattaforma, che si ottengono
!! dichiarando una variabile come \c REAL o \c DOUBLEPRECISION e i
!! tipi reali a singola o doppia precisione IEEE (4 e 8 byte
!! rispettivamente) che sono i tipi che rispettano le precisioni
!! standard IEEE e si dichiarano con \c KIND \a fp_s e \a fp_d
!! rispettivamente; generalmente coincidono a due a due, ma non è
!! garantito.
!!
!! \ingroup base
MODULE missing_values
USE kinds
IMPLICIT NONE

REAL, PARAMETER :: rmiss = HUGE(1.0) !< reale di default
DOUBLEPRECISION, PARAMETER :: dmiss = HUGE(1.0D0) !< doppia precisione di default
REAL(kind=fp_s), PARAMETER :: rsmiss = HUGE(1.0_fp_s) !< reale a singola precisione IEEE \a (kind=fp_s)
REAL(kind=fp_d), PARAMETER :: rdmiss = HUGE(1.0_fp_d) !< reale a doppia precisione IEEE \a (kind=fp_d)
INTEGER, PARAMETER :: imiss = HUGE(0) !< intero di default
INTEGER(kind=int_b), PARAMETER :: ibmiss = HUGE(0_int_b) !< intero ad 1 byte \a (kind=int_b)
INTEGER(kind=int_b), PARAMETER :: bmiss = ibmiss !< 
INTEGER(kind=int_s), PARAMETER :: ismiss = HUGE(0_int_s) !< intero a 2 byte \a (kind=int_s)
INTEGER(kind=int_l), PARAMETER :: ilmiss = HUGE(0_int_l) !< intero a 4 byte \a (kind=int_l)
INTEGER(kind=int_ll), PARAMETER :: illmiss = HUGE(0_int_ll) !< intero a 8 byte \a (kind=int_ll)
CHARACTER(len=1), PARAMETER :: cmiss = char(0) !< carattere (qualsiasi lunghezza)


!> Insieme di funzioni che restitiuscono \a .TRUE. se l'argomento è un dato valido 
!! e \a .FALSE. se è mancante; è richiamabile per tutti i tipi definiti sopra.
INTERFACE c_e
  MODULE PROCEDURE c_e_b, c_e_s, c_e_l, c_e_r, c_e_d, c_e_c
END INTERFACE

PUBLIC

CONTAINS

! Ritorna l'indice del primo elemento vero del vettore logico v
FUNCTION firsttrue(v) RESULT(i)
LOGICAL,INTENT(in) :: v(:)
INTEGER :: i

DO i = 1, SIZE(v)
  IF (v(i)) RETURN
ENDDO
i = 0

END FUNCTION firsttrue



!> Controlla se l'argomento byte è un dato valido
elemental  logical function c_e_b(var)

!OMSTART c_e_b
!	function c_e_b(var)
!	Verifica la condizione di presenza o assenza del dato secondo
!	le specifiche dballe restituendo una variabile logical .true.
!	se c'e` il dato 
!
!	INPUT:
!	VAR	byte	dato di cui verificare la presenza
!	OUTPUT:
!	C_E_B	LOGICAL	.TRUE.se il dato e` presente
!OMEND

    integer(kind=int_b),intent(in)  :: var !< variabile da controllare

    c_e_b=.true.
    if (var == ibmiss)c_e_b= .FALSE. 
    return
    end function c_e_b


!> Controlla se l'argomento short è un dato valido
elemental    logical function c_e_s(var)

!OMSTART c_e_i
!	function c_e_s(var)
!	Verifica la condizione di presenza o assenza del dato secondo
!	le specifiche dballe restituendo una variabile logical .true.
!	se c'e` il dato
!
!	INPUT:
!	VAR	Integer	short dato di cui verificare la presenza
!	OUTPUT:
!	C_E_i	LOGICAL	.TRUE.se il dato e` presente
!OMEND

    integer (kind=int_s),intent(in) ::  var !< variabile da controllare

    c_e_s=.true.
    if (var == ismiss)c_e_s= .FALSE. 
    return
    end function c_e_s


!> Controlla se l'argomento long è un dato valido
elemental    logical function c_e_l(var)

!OMSTART c_e_l
!	function c_e_l(var)
!	Verifica la condizione di presenza o assenza del dato secondo
!	le specifiche dballe restituendo una variabile logical .true.
!	se c'e` il dato
!
!	INPUT:
!	VAR	Integer	long dato di cui verificare la presenza
!	OUTPUT:
!	C_E_l	LOGICAL	.TRUE.se il dato e` presente
!OMEND

    integer (kind=int_l),intent(in) ::  var !< variabile da controllare

    c_e_l=.true.
    if (var == ilmiss)c_e_l= .FALSE. 
    return
    end function c_e_l




!> Controlla se l'argomento real è un dato valido
elemental    logical function c_e_r(var)

!OMSTART c_e_r
!	function c_e_r(var)
!	Verifica la condizione di presenza o assenza del dato secondo
!	le specifiche dballe restituendo una variabile logical .true.
!	se c'e` il dato
!
!	INPUT:
!	VAR	Real	dato di cui verificare la presenza
!	OUTPUT:
!	C_E_R	LOGICAL	.TRUE.se il dato e` presente
!OMEND

    real,intent(in) :: var !< variabile da controllare

    c_e_r=.true.
    if (var == rmiss)c_e_r= .FALSE. 
    return
    end function c_e_r

!> Controlla se l'argomento double è un dato valido
elemental    logical function c_e_d(var)

!OMSTART c_e_d
!	function c_e_d(var)
!	Verifica la condizione di presenza o assenza del dato secondo
!	le specifiche dballe restituendo una variabile logical .true.
!	se c'e` il dato
!
!	INPUT:
!	VAR	double	dato di cui verificare la presenza
!	OUTPUT:
!	C_E_D	LOGICAL	.TRUE.se il dato e` presente
!OMEND

    real (kind=fp_d),intent(in) ::  var !< variabile da controllare

    c_e_d=.true.
    if (var == rdmiss)c_e_d= .FALSE. 
    return
    end function c_e_d



!> Controlla se l'argomento character è un dato valido
elemental    logical function c_e_c(var)
!OMSTART C_E_C
!	function c_e_c(var)
!	Verifica la condizione di presenza o assenza del dato secondo
!	le specifiche dballe restituendo una variabile logical .true.
!	se c'e` il dato

!	INPUT:
!	VAR	CHAR*(*)  	dato di cui verificare la presenza
!	OUTPUT:
!	C_E_C	LOGICAL		.TRUE.se il dato e` presente
!OMEND

      character (len=*),intent(in) :: var !< variabile da controllare

      c_e_c=.true.
      if (var == cmiss)c_e_c=.false.
      return

    end function c_e_c


END MODULE missing_values
