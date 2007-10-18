!> \brief Valori mancanti.
!!
!! Questo modulo fornisce strumenti per gestire i valori
!! mancanti per vari tipi di dati.  È importante usare le costanti del
!! tipo giusto per i propri dati per evitare conversioni automatiche che
!! darebbero risultati errati.
!! \ingroup base
MODULE missing_values
USE kinds
IMPLICIT NONE

!omstart missing_values
!idx Costanti fortran per i valori mancanti
!Questo modulo definisce le seguenti costanti da usare come
!valori mancanti:
!
!rmiss  => reale di default
!smiss  => reale a singola precisione (kind=fp_s)
!dmiss  => reale a doppia precisione (kind=fp_d)
!imiss  => intero di default
!ibmiss => intero ad un byte (kind=int_b)
!ismiss => intero a 2 byte (kind=int_s)
!ilmiss => intero a 4 byte (kind=int_l)
!cmiss  => carattere
!
!Fa uso dei tipi definiti dal modulo kinds
!
!Definisce le seguenti procedure:
!
!LOGICAL FUNCTION c_e(var)
!
!Restitiusce .TRUE. se var e` valida e .FALSE. se e` mancante;
!e` richiamabile per tutti i tipi definiti sopra
!
!vedi kinds
!omend

REAL, PARAMETER :: rmiss = HUGE(1.0) !< reale di default
REAL(kind=fp_s), PARAMETER :: rsmiss = HUGE(1.0_fp_s) !< reale a singola precisione \a (kind=fp_s)
REAL(kind=fp_d), PARAMETER :: rdmiss = HUGE(1.0_fp_d) !< reale a doppia precisione \a (kind=fp_d)
REAL(kind=fp_d), PARAMETER :: dmiss = rdmiss !< 
INTEGER, PARAMETER :: imiss = HUGE(0) !< intero di default
INTEGER(kind=int_b), PARAMETER :: ibmiss = HUGE(0_int_b) !< intero ad 1 byte \a (kind=int_b)
INTEGER(kind=int_b), PARAMETER :: bmiss = ibmiss !< 
INTEGER(kind=int_s), PARAMETER :: ismiss = HUGE(0_int_s) !< intero a 2 byte \a (kind=int_s)
INTEGER(kind=int_l), PARAMETER :: ilmiss = HUGE(0_int_l) !< intero a 4 byte \a (kind=int_l)
CHARACTER(len=1), PARAMETER :: cmiss = char(0) !< carattere (qualsiasi lunghezza)


!> Insieme di funzioni che restitiuscono \a .TRUE. se l'argomento è un dato valido 
!! e \a .FALSE. se è mancante; è richiamabile per tutti i tipi definiti sopra.
INTERFACE c_e
  MODULE PROCEDURE c_e_b, c_e_s, c_e_l, c_e_r, c_e_d, c_e_c
END INTERFACE

PUBLIC

CONTAINS

!> Controlla se l'argomento byte è un dato valido
  logical function c_e_b(var)

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

    integer(kind=int_b)  :: var !< variabile da controllare

    c_e_b=.true.
    if (var == ibmiss)c_e_b= .FALSE. 
    return
    end function c_e_b


!> Controlla se l'argomento short è un dato valido
    logical function c_e_s(var)

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

    integer (kind=int_s) ::  var !< variabile da controllare

    c_e_s=.true.
    if (var == ismiss)c_e_s= .FALSE. 
    return
    end function c_e_s


!> Controlla se l'argomento long è un dato valido
    logical function c_e_l(var)

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

    integer (kind=int_l) ::  var !< variabile da controllare

    c_e_l=.true.
    if (var == ilmiss)c_e_l= .FALSE. 
    return
    end function c_e_l




!> Controlla se l'argomento real è un dato valido
    logical function c_e_r(var)

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

    real :: var !< variabile da controllare

    c_e_r=.true.
    if (var == rmiss)c_e_r= .FALSE. 
    return
    end function c_e_r

!> Controlla se l'argomento double è un dato valido
    logical function c_e_d(var)

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

    real (kind=fp_d) ::  var !< variabile da controllare

    c_e_d=.true.
    if (var == rdmiss)c_e_d= .FALSE. 
    return
    end function c_e_d



!> Controlla se l'argomento character è un dato valido
    logical function c_e_c(var)
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

      character (len=*) var !< variabile da controllare

      c_e_c=.true.
      if (var == cmiss)c_e_c=.false.
      return

    end function c_e_c


END MODULE missing_values
