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
!> \brief Classi per la gestione delle coordinate temporali.
!! 
!! Questo module definisce un paio di classi per la gestione di
!! date assolute e di intervalli temporali.
!! Entrambe le classi hanno le componenti di tipo \c PRIVATE, per
!! cui non possono essere manipolate direttamente ma solo tramite i
!! relativi metodi.  Attualmente la precisione massima consentita � di un
!! minuto, mentre l'estensione delle date rappresentabili va dall'anno 1
!! all'anno 4074 d.C. circa, ipotizzando un calendario gregoriano per
!! tutto il periodo.  Questo fatto implica che le date precedenti
!! all'introduzione del calendario gregoriano avranno discrepanze di uno
!! o pi� giorni rispetto alle date storiche "vere", ammesso che
!! qualcuno conosca queste ultime.
!! \ingroup base
MODULE datetime_class
USE kinds
USE log4fortran
USE err_handling
USE missing_values
USE optional_values
IMPLICIT NONE

INTEGER, PARAMETER :: dateint=SELECTED_INT_KIND(13)

!> Classe che indica un istante temporale assoluto.
TYPE datetime
  PRIVATE
  INTEGER(KIND=int_ll) :: iminuti
END TYPE  datetime

!> Classe che indica un intervallo temporale relativo.
!! Pu� assumere anche valori <0. Pu� assumere  valori "puri", cio�
!! intervalli temporali di durata fissa, e valori "popolari", cio�
!! intervalli di durata variabile in unit� di mesi e/o anni, o anche
!! valori misti, cio� mesi/anni + un intervallo fisso; tuttavia negli
!! ultimi 2 casi le operazioni che si possono effettuare con
!! oggetti di questa classe sono limitate.
TYPE timedelta
  PRIVATE
  INTEGER(KIND=int_ll) :: iminuti
  INTEGER :: month
END TYPE timedelta


!> Class to specify a cyclic datetime
!! You need it to specify for example every january in all years or
!! the same time for all days and so on
TYPE cyclicdatetime
  PRIVATE
  INTEGER :: minute
  INTEGER :: hour
  INTEGER :: day
  INTEGER :: month
  INTEGER :: year
END TYPE cyclicdatetime




!> valore mancante per datetime
TYPE(datetime), PARAMETER :: datetime_miss=datetime(illmiss)
!> valore mancante per timedelta
TYPE(timedelta), PARAMETER :: timedelta_miss=timedelta(illmiss, 0)
!> intervallo timedelta di durata nulla
TYPE(timedelta), PARAMETER :: timedelta_0=timedelta(0, 0)
!> inizializza con l'ora UTC
INTEGER, PARAMETER :: datetime_utc=1
!> inizializza con l'ora locale
INTEGER, PARAMETER :: datetime_local=2
!> Minimum valid value for datetime
TYPE(datetime), PARAMETER :: datetime_min=datetime(-HUGE(1_int_ll)-1)
!> Minimum valid value for datetime
TYPE(datetime), PARAMETER :: datetime_max=datetime(HUGE(1_int_ll)-1)
!> Minimum valid value for timedelta
TYPE(timedelta), PARAMETER :: timedelta_min=timedelta(-HUGE(1_int_ll)-1,0)
!> Minimum valid value for timedelta
TYPE(timedelta), PARAMETER :: timedelta_max=timedelta(HUGE(1_int_ll)-1,0)
!> missing value for cyclicdatetime
TYPE(cyclicdatetime), PARAMETER :: cyclicdatetime_miss=cyclicdatetime(imiss,imiss,imiss,imiss,imiss)


INTEGER(kind=dateint), PARAMETER :: &
 sec_in_day=86400, &
 sec_in_hour=3600, &
 sec_in_min=60, &
 min_in_day=1440, &
 min_in_hour=60, &
 hour_in_day=24

INTEGER,PARAMETER :: &
 year0=1, & ! anno di origine per iminuti
 d1=365, & ! giorni/1 anno nel calendario gregoriano
 d4=d1*4+1, & ! giorni/4 anni nel calendario gregoriano
 d100=d1*100+25-1, & ! giorni/100 anni nel calendario gregoriano
 d400=d1*400+100-3, & ! giorni/400 anni nel calendario gregoriano
 ianno(13,2)=RESHAPE((/ &
 0,31,59,90,120,151,181,212,243,273,304,334,365, &
 0,31,60,91,121,152,182,213,244,274,305,335,366/),(/13,2/))

INTEGER(KIND=int_ll),PARAMETER :: &
 unsec=62135596800_int_ll ! differenza tra 01/01/1970 e 01/01/0001 (sec, per unixtime)

!> Costruttori per le classi datetime e timedelta. Devono essere richiamati
!! per tutti gli oggetti di questo tipo definiti in un programma
!! tranne i casi in cui un oggetto viene creato per assegnazione.
INTERFACE init
  MODULE PROCEDURE datetime_init, timedelta_init
END INTERFACE

!> Distruttori per le 2 classi. Distruggono gli oggetti in maniera pulita,
!! assegnando loro un valore mancante.
INTERFACE delete
  MODULE PROCEDURE datetime_delete, timedelta_delete
END INTERFACE

!> Restituiscono il valore dell'oggetto nella forma desiderata.
INTERFACE getval
  MODULE PROCEDURE datetime_getval, timedelta_getval
END INTERFACE

!> Restituiscono il valore dell'oggetto in forma di stringa stampabile.
INTERFACE to_char
  MODULE PROCEDURE datetime_to_char, timedelta_to_char
END INTERFACE


!> Functions that return a trimmed CHARACTER representation of the
!! input variable. The functions are analogous to \a to_char but they
!! return representation of the input in a CHARACTER with a variable
!! length, which needs not to be trimmed before use. The optional
!! format here is not accepted and these functions are not \a
!! ELEMENTAL so they work only on scalar arguments.
!!
!! \param in (datetime or timedelta) value to be represented as CHARACTER
!!
!! Example of use:
!! \code
!! USE datetime_class
!! type(datetime) :: t
!! ...
!! WRITE(*,*)'The value provided is, '//t2c(t)'
!! ...
!! \endcode
INTERFACE t2c
  MODULE PROCEDURE trim_datetime_to_char, trim_timedelta_to_char
END INTERFACE

!> Operatore logico di uguaglianza tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con pi�
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE datetime_eq, datetime_eqsv, timedelta_eq, timedelta_eqsv, &
   cyclicdatetime_eq, cyclicdatetime_datetime_eq, datetime_cyclicdatetime_eq
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con pi�
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE datetime_ne, datetime_nesv, timedelta_ne, timedelta_nesv
END INTERFACE

!> Operatore logico maggiore tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con pi�
!! di 1 dimensione e scalari). Nel caso di valori mancanti il risultato
!! non � specificato. Il risultato non � altres� specificato nel caso di oggetti
!! \a timedelta "popolari" o misti.
INTERFACE OPERATOR (>)
  MODULE PROCEDURE datetime_gt, datetime_gtsv, timedelta_gt, timedelta_gtsv
END INTERFACE

!> Operatore logico minore tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con pi�
!! di 1 dimensione e scalari). Nel caso di valori mancanti il risultato
!! non � specificato. Il risultato non � altres� specificato nel caso di oggetti
!! \a timedelta "popolari" o misti.
INTERFACE OPERATOR (<)
  MODULE PROCEDURE datetime_lt, datetime_ltsv, timedelta_lt, timedelta_ltsv
END INTERFACE

!> Operatore logico maggiore-uguale tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con pi�
!! di 1 dimensione e scalari). Nel caso di valori mancanti il risultato
!! non � specificato. Il risultato non � altres� specificato nel caso di oggetti
!! \a timedelta "popolari" o misti.
INTERFACE OPERATOR (>=)
  MODULE PROCEDURE datetime_ge, datetime_gesv, timedelta_ge, timedelta_gesv
END INTERFACE

!> Operatore logico minore-uguale tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con pi�
!! di 1 dimensione e scalari). Nel caso di valori mancanti il risultato
!! non � specificato. Il risultato non � altres� specificato nel caso di oggetti
!! \a timedelta "popolari" o misti.
INTERFACE OPERATOR (<=)
  MODULE PROCEDURE datetime_le, datetime_lesv, timedelta_le, timedelta_lesv
END INTERFACE

!> Operatore di somma per datetime e timedelta. Solo alcune combinazioni
!! sono definite:
!! - \a timedelta + \a timedelta = \a timedelta
!! - \a datetime + \a timedelta = \a datetime
!! .
!! Funzionano anche con oggetti \a timedelta "popolari" o misti.
INTERFACE OPERATOR (+)
  MODULE PROCEDURE datetime_add, timedelta_add
END INTERFACE

!> Operatore di sottrazione per datetime e timedelta. Solo alcune combinazioni
!! sono definite:
!! - \a timedelta - \a timedelta = \a timedelta
!! - \a datetime - \a timedelta = \a datetime
!! - \a datetime - \a datetime = \a timedelta
!! .
!! Funzionano anche con oggetti \a timedelta "popolari" o misti.
INTERFACE OPERATOR (-)
  MODULE PROCEDURE datetime_subdt, datetime_subtd, timedelta_sub
END INTERFACE

!> Operatore di moltiplicazione di timedelta per uno scalare. Sono definite:
!! - \a timedelta * \a INTEGER = \a timedelta
!! - \a INTEGER * \a timedelta = \a timedelta
!! .
!! Funzionano anche con oggetti \a timedelta "popolari" o misti.
INTERFACE OPERATOR (*)
  MODULE PROCEDURE timedelta_mult, timedelta_tlum
END INTERFACE

!> Operatore di divisione di timedelta. Sono definite:
!! - \a timedelta / \a INTEGER = \a timedelta
!! - \a timedelta / \a timedelta = \a INTEGER
!! .
!! La prima combinazione � valida per tutti i tipi di intervallo, mentre la
!! seconda � definita solo per intervalli "puri".
INTERFACE OPERATOR (/)
  MODULE PROCEDURE timedelta_divint, timedelta_divtd
END INTERFACE

!> Operatore di resto della divisione.
!! Sono definite le combinazioni:
!! - \a MOD(\a timedelta, \a timedelta) = \a timedelta
!! - \a MOD(\a datetime, \a timedelta) = \a timedelta.
!! 
!! Sono definite solo per intervalli "puri"
!! La seconda combinazione ha senso principalmente con intervalli di
!! 1 minuto, 1 ora o
!! 1 giorno, per calcolare di quanto l'oggetto \a datetime indicato dista
!! dal minuto, ora o giorno tondo precedente pi� vicino.
INTERFACE mod
  MODULE PROCEDURE timedelta_mod, datetime_timedelta_mod
END INTERFACE

!> Operatore di valore assoluto di un intervallo.
!! - \a ABS(\a timedelta) = \a timedelta
INTERFACE abs
  MODULE PROCEDURE timedelta_abs
END INTERFACE

!> Legge un oggetto datetime/timedelta o un vettore di oggetti datetime/timedelta da
!! un file \c FORMATTED o \c UNFORMATTED.
INTERFACE read_unit
  MODULE PROCEDURE datetime_read_unit, datetime_vect_read_unit, &
   timedelta_read_unit, timedelta_vect_read_unit
END INTERFACE

!> Scrive un oggetto datetime/timedelta o un vettore di oggetti datetime/timedelta su
!! un file \c FORMATTED o \c UNFORMATTED.
INTERFACE write_unit
  MODULE PROCEDURE datetime_write_unit, datetime_vect_write_unit, &
   timedelta_write_unit, timedelta_vect_write_unit
END INTERFACE

!> Print object
INTERFACE display
  MODULE PROCEDURE display_datetime, display_timedelta
END INTERFACE

!> Missing check
INTERFACE c_e
  MODULE PROCEDURE c_e_datetime, c_e_cyclicdatetime
END INTERFACE

!> to document
INTERFACE count_distinct
  MODULE PROCEDURE   count_distinct_datetime
END INTERFACE

!> to document
INTERFACE pack_distinct
  MODULE PROCEDURE    pack_distinct_datetime
END INTERFACE

!> to document
INTERFACE map_distinct
  MODULE PROCEDURE   map_distinct_datetime
END INTERFACE

!> to document
INTERFACE map_inv_distinct
  MODULE PROCEDURE  map_inv_distinct_datetime
END INTERFACE

!> Index method.
INTERFACE index
  MODULE PROCEDURE index_datetime
END INTERFACE

!> Sort method.
INTERFACE sort
  MODULE PROCEDURE sort_datetime
END INTERFACE


#define ARRAYOF_ORIGTYPE TYPE(datetime)
#define ARRAYOF_TYPE arrayof_datetime
#define ARRAYOF_ORIGEQ 1
#include "arrayof_pre.F90"
! from arrayof
PUBLIC insert, append, remove, packarray
PUBLIC insert_unique, append_unique

PRIVATE

PUBLIC datetime, datetime_miss, datetime_utc, datetime_local, &
 datetime_min, datetime_max, &
 datetime_new, init, delete, getval, to_char, t2c, &
 read_unit, write_unit, &
 OPERATOR(==), OPERATOR(/=), OPERATOR(>), OPERATOR(<), &
 OPERATOR(>=), OPERATOR(<=), OPERATOR(+), OPERATOR(-), &
 OPERATOR(*), OPERATOR(/), mod, abs, &
 timedelta, timedelta_miss, timedelta_new, timedelta_0, &
 timedelta_min, timedelta_max, timedelta_getamsec, timedelta_depop, &
 display, c_e, &
 count_distinct, pack_distinct, map_distinct, map_inv_distinct, index, sort, &
 cyclicdatetime, cyclicdatetime_new, cyclicdatetime_miss

CONTAINS


! ==============
! == datetime ==
! ==============

!> Costruisce un oggetto \a datetime con i parametri opzionali forniti.
!! Se non viene passato nulla lo inizializza a 1/1/1.
!! Questa � la versione \c FUNCTION, in stile F2003, del costruttore, da preferire
!! rispetto alla versione \c SUBROUTINE \c init.
!! Notare che i gruppi di parametri opzionali (\a year, \a month, \a hour,
!! \a minute, \a msec), (\a unixtime), (\a isodate), (\a simpledate),
!! (\a oraclesimdate) sono mutualmente escludentesi; \a oraclesimedate �
!! obsoleto, usare piuttosto \a simpledate.
FUNCTION datetime_new(year, month, day, hour, minute, msec, &
 unixtime, isodate, simpledate, oraclesimdate, now) RESULT(this)
INTEGER,INTENT(IN),OPTIONAL :: year !< anno d.C., se � specificato, tutti gli eventuali parametri tranne \a month, \a day, \a hour, \a minute e \a msec sono ignorati; per un problema non risolto, sono ammessi solo anni >0 (d.C.)
INTEGER,INTENT(IN),OPTIONAL :: month !< mese, default=1 se � specificato \a year; pu� assumere anche valori <1 o >12, l'oggetto finale si aggiusta coerentemente
INTEGER,INTENT(IN),OPTIONAL :: day !< mese, default=1 se � specificato \a year; pu� anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: hour !< ore, default=0 se � specificato \a year; pu� anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: minute !< minuti, default=0 se � specificato \a year; pu� anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: msec !< millisecondi, default=0 se � specificato \a year; pu� anch'esso assumere valori fuori dai limiti canonici
INTEGER(kind=int_ll),INTENT(IN),OPTIONAL :: unixtime !< inizializza l'oggetto a \a unixtime secondi dopo il 1/1/1970 (convenzione UNIX, notare che il parametro deve essere un intero a 8 byte), se � presente tutto il resto � ignorato
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate !< inizializza l'oggetto ad una data espressa nel formato \c AAAA-MM-GG \c hh:mm:ss.msc, un sottoinsieme del formato noto come \a ISO, la parte iniziale \c AAAA-MM-GG � obbligatoria, il resto � opzionale
CHARACTER(len=*),INTENT(IN),OPTIONAL :: simpledate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmmssmsc, la parte iniziale \c AAAAMMGG � obbligatoria, il resto � opzionale, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmm, come nelle routine per l'accesso al db Oracle del SIM.
INTEGER,INTENT(IN),OPTIONAL :: now !< inizializza l'oggetto all'istante corrente, se \a � \a datetime_utc inizializza con l'ora UTC (preferibile), se � \a datetime_local usa l'ora locale

TYPE(datetime) :: this !< oggetto da inizializzare

CALL datetime_init(this, year, month, day, hour, minute, msec, &
 unixtime, isodate, simpledate, oraclesimdate, now)

END FUNCTION datetime_new

!> Costruisce un oggetto \a datetime con i parametri opzionali forniti.
!! Se non viene passato nulla lo inizializza a 1/1/1.
!! Notare che i gruppi di parametri opzionali (\a year, \a month, \a hour,
!! \a minute, \a msec), (\a unixtime), (\a isodate), (\a simpledate),
!! (\a oraclesimdate) sono mutualmente escludentesi; \a oraclesimedate �
!! obsoleto, usare piuttosto \a simpledate.
RECURSIVE SUBROUTINE datetime_init(this, year, month, day, hour, minute, msec, &
 unixtime, isodate, simpledate, oraclesimdate, now)
TYPE(datetime),INTENT(INOUT) :: this !< oggetto da inizializzare
INTEGER,INTENT(IN),OPTIONAL :: year !< anno d.C., se � specificato, tutti gli eventuali parametri tranne \a month, \a day, \a hour e \a minute sono ignorati; per un problema non risolto, sono ammessi solo anni >0 (d.C.)
INTEGER,INTENT(IN),OPTIONAL :: month !< mese, default=1 se � specificato \a year; pu� assumere anche valori <1 o >12, l'oggetto finale si aggiusta coerentemente
INTEGER,INTENT(IN),OPTIONAL :: day !< mese, default=1 se � specificato \a year; pu� anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: hour !< ore, default=0 se � specificato \a year; pu� anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: minute !< minuti, default=0 se � specificato \a year; pu� anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: msec !< millisecondi, default=0 se � specificato \a year; pu� anch'esso assumere valori fuori dai limiti canonici
INTEGER(kind=int_ll),INTENT(IN),OPTIONAL :: unixtime !< inizializza l'oggetto a \a unixtime secondi dopo il 1/1/1970 (convenzione UNIX, notare che il parametro deve essere un intero a 8 byte)
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate !< inizializza l'oggetto ad una data espressa nel formato \c AAAA-MM-GG \c hh:mm:ss.msc, un sottoinsieme del formato noto come \a ISO, la parte iniziale \c AAAA-MM-GG � obbligatoria, il resto � opzionale
CHARACTER(len=*),INTENT(IN),OPTIONAL :: simpledate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmmssmsc, la parte iniziale \c AAAAMMGG � obbligatoria, il resto � opzionale, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmm, come nelle routine per l'accesso al db Oracle del SIM.
INTEGER,INTENT(IN),OPTIONAL :: now !< inizializza l'oggetto all'istante corrente, se \a � \a datetime_utc inizializza con l'ora UTC (preferibile), se � \a datetime_local usa l'ora locale

INTEGER :: lyear, lmonth, lday, lhour, lminute, lsec, lmsec, ier
INTEGER :: dt(8)
CHARACTER(len=23) :: datebuf

IF (PRESENT(year)) THEN ! anno/mese/giorno, ecc.
  lyear = year
  IF (PRESENT(month)) THEN
    lmonth = month
  ELSE
    lmonth = 1
  ENDIF
  IF (PRESENT(day)) THEN
    lday = day
  ELSE
    lday = 1
  ENDIF
  IF (PRESENT(hour)) THEN
    lhour = hour
  ELSE
    lhour = 0
  ENDIF
  IF (PRESENT(minute)) THEN
    lminute = minute
  ELSE
    lminute = 0
  ENDIF
  IF (PRESENT(msec)) THEN
    lmsec = msec
  ELSE
    lmsec = 0
  ENDIF

  if (c_e(lday) .and. c_e(lmonth) .and. c_e(lyear) .and. c_e(lhour) &
   .and. c_e(lminute) .and. c_e(lmsec)) then
    CALL jeladata5_1(lday, lmonth, lyear, lhour, lminute, lmsec, this%iminuti)
  else
    this=datetime_miss
  end if

ELSE IF (PRESENT(unixtime)) THEN ! secondi dal 01/01/1970 (unix)
  if (c_e(unixtime)) then
    this%iminuti = (unixtime + unsec)*1000
  else
    this=datetime_miss    
  end if

ELSE IF (PRESENT(isodate)) THEN ! formato iso YYYY-MM-DD hh:mm:ss.msc

  IF (c_e(isodate) .AND. LEN_TRIM(isodate) > 0) THEN
    datebuf(1:23) = '0001-01-01 00:00:00.000'
    datebuf(1:MIN(LEN(isodate),23)) = isodate(1:MIN(LEN(isodate),23))
    READ(datebuf,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2,1X,I3)', err=100) &
     lyear, lmonth, lday, lhour, lminute, lsec, lmsec
    lmsec = lmsec + lsec*1000
    CALL jeladata5_1(lday, lmonth, lyear, lhour, lminute, lmsec, this%iminuti)
    RETURN
    
100 CONTINUE ! condizione di errore in isodate
    CALL delete(this)
    CALL l4f_log(L4F_ERROR, 'isodate '//TRIM(isodate)//' not valid')
    CALL raise_error()
    RETURN
  ELSE
    this = datetime_miss
  ENDIF

ELSE IF (PRESENT(simpledate)) THEN ! formato YYYYMMDDhhmmssmsc
  IF (c_e(simpledate) .AND. LEN_TRIM(simpledate) > 0)THEN
    datebuf(1:17) = '00010101000000000'
    datebuf(1:MIN(LEN(simpledate),17)) = simpledate(1:MIN(LEN(simpledate),17))
    READ(datebuf,'(I4.4,5I2.2,I3.3)', err=120) &
     lyear, lmonth, lday, lhour, lminute, lsec, lmsec
    lmsec = lmsec + lsec*1000
    CALL jeladata5_1(lday, lmonth, lyear, lhour, lminute, lmsec, this%iminuti)
    RETURN

120 CONTINUE ! condizione di errore in simpledate
    CALL delete(this)
    CALL l4f_log(L4F_ERROR, 'simpledate '//TRIM(simpledate)//' not valid')
    CALL raise_error()
    RETURN
  ELSE
    this = datetime_miss
  ENDIF


ELSE IF (PRESENT(oraclesimdate)) THEN ! formato YYYYMMDDhhmm

  if(c_e(oraclesimdate))then
    CALL l4f_log(L4F_WARN, 'in datetime_init, parametro oraclesimdate '// &
     'obsoleto, usare piuttosto simpledate')
    READ(oraclesimdate,'(I4,4I2)', iostat=ier) lyear, lmonth, lday, lhour, lminute
    IF (ier /= 0) THEN
      CALL delete(this)
      CALL l4f_log(L4F_ERROR, 'oraclesimdate '//TRIM(oraclesimdate)//' not valid')
      CALL raise_error()
      RETURN
    ENDIF
    CALL jeladata5_1(lday,lmonth,lyear,lhour,lminute,0,this%iminuti)
  else
    this = datetime_miss
  end if

ELSE IF (PRESENT(now)) THEN
  if(c_e(now))then
    CALL DATE_AND_TIME(values=dt)
    IF (now /= datetime_local) dt(6) = dt(6) - dt(4) ! back to UTC
    CALL init(this, year=dt(1), month=dt(2), day=dt(3), hour=dt(5), minute=dt(6), &
     msec=dt(7)*1000+dt(8))
  else
    this = datetime_miss
  end if

ELSE
  this = datetime_miss
ENDIF

END SUBROUTINE datetime_init


SUBROUTINE datetime_delete(this)
TYPE(datetime),INTENT(INOUT) :: this

this%iminuti = imiss

END SUBROUTINE datetime_delete


!> Restituisce il valore di un oggetto \a datetime in una o pi�
!! modalit� desiderate. Qualsiasi combinazione dei parametri
!! opzionali � consentita. \a oraclesimedate �
!! obsoleto, usare piuttosto \a simpledate.
pure SUBROUTINE datetime_getval(this, year, month, day, hour, minute, msec, &
 unixtime, isodate, simpledate, oraclesimdate)
TYPE(datetime),INTENT(IN) :: this !< oggetto di cui restituire il valore
INTEGER,INTENT(OUT),OPTIONAL :: year !< anno
INTEGER,INTENT(OUT),OPTIONAL :: month !< mese
INTEGER,INTENT(OUT),OPTIONAL :: day !< giorno
INTEGER,INTENT(OUT),OPTIONAL :: hour !< ore
INTEGER,INTENT(OUT),OPTIONAL :: minute !< minuti
INTEGER,INTENT(OUT),OPTIONAL :: msec !< millisecondi
INTEGER(kind=int_ll),INTENT(OUT),OPTIONAL :: unixtime !< secondi a partire dal 1/1/1970
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: isodate !< data completa nel formato \c AAAA-MM-GG \c hh:mm:ss.msc (simil-ISO), la variabile pu� essere pi� corta di 23 caratteri, in tal caso conterr� solo ci� che vi cape
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: simpledate !< data completa nel formato \c AAAAMMGGhhmmssmsc , la variabile pu� essere pi� corta di 17 caratteri, in tal caso conterr� solo ci� che vi cape, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(OUT),OPTIONAL :: oraclesimdate !< data parziale nel formato \c AAAAMMGGhhmm

INTEGER :: lyear, lmonth, lday, lhour, lminute, lmsec
CHARACTER(len=23) :: datebuf

IF (PRESENT(year) .OR. PRESENT(month) .OR. PRESENT(day) .OR. PRESENT(hour) &
 .OR. PRESENT(minute) .OR. PRESENT(msec) .OR. PRESENT(isodate) &
 .OR. PRESENT(simpledate) .OR. PRESENT(oraclesimdate) .OR. PRESENT(unixtime)) THEN

  IF (this == datetime_miss) THEN

    IF (PRESENT(msec)) THEN 
      msec = imiss
    ENDIF
    IF (PRESENT(minute)) THEN 
      minute = imiss
    ENDIF
    IF (PRESENT(hour)) THEN
      hour = imiss
    ENDIF
    IF (PRESENT(day)) THEN
      day = imiss
    ENDIF
    IF (PRESENT(month)) THEN
      month = imiss
    ENDIF
    IF (PRESENT(year)) THEN
      year = imiss
    ENDIF
    IF (PRESENT(isodate)) THEN
      isodate = cmiss
    ENDIF
    IF (PRESENT(simpledate)) THEN
      simpledate = cmiss
    ENDIF
    IF (PRESENT(oraclesimdate)) THEN
!!$      CALL l4f_log(L4F_WARN, 'in datetime_getval, parametro oraclesimdate '// &
!!$       'obsoleto, usare piuttosto simpledate')
      oraclesimdate=cmiss
    ENDIF
    IF (PRESENT(unixtime)) THEN
      unixtime = illmiss
    ENDIF

  ELSE

    CALL jeladata6_1(lday, lmonth, lyear, lhour, lminute, lmsec, this%iminuti)
    IF (PRESENT(msec)) THEN 
      msec = lmsec
    ENDIF
    IF (PRESENT(minute)) THEN 
      minute = lminute
    ENDIF
    IF (PRESENT(hour)) THEN
      hour = lhour
    ENDIF
    IF (PRESENT(day)) THEN
      day = lday
    ENDIF
    IF (PRESENT(month)) THEN
      month = lmonth
    ENDIF
    IF (PRESENT(year)) THEN
      year = lyear
    ENDIF
    IF (PRESENT(isodate)) THEN
      WRITE(datebuf(1:23), '(I4.4,A1,I2.2,A1,I2.2,1X,I2.2,A1,I2.2,A1,I2.2,A1,I3.3)') &
       lyear, '-', lmonth, '-', lday, lhour, ':', lminute, ':', lmsec/1000, &
       '.', MOD(lmsec, 1000)
      isodate = datebuf(1:MIN(LEN(isodate),23))
    ENDIF
    IF (PRESENT(simpledate)) THEN
      WRITE(datebuf(1:17), '(I4.4,5I2.2,I3.3)') &
       lyear, lmonth, lday, lhour, lminute, lmsec/1000, MOD(lmsec, 1000)
      simpledate = datebuf(1:MIN(LEN(simpledate),17))
    ENDIF
    IF (PRESENT(oraclesimdate)) THEN
!!$      CALL l4f_log(L4F_WARN, 'in datetime_getval, parametro oraclesimdate '// &
!!$       'obsoleto, usare piuttosto simpledate')
      WRITE(oraclesimdate, '(I4.4,4I2.2)') lyear, lmonth, lday, lhour, lminute
    ENDIF
    IF (PRESENT(unixtime)) THEN
      unixtime = this%iminuti/1000_int_ll-unsec
    ENDIF

  ENDIF
ENDIF

END SUBROUTINE datetime_getval


!> Restituisce una rappresentazione carattere stampabile di un oggetto
!! \a datetime.
elemental FUNCTION datetime_to_char(this) RESULT(char)
TYPE(datetime),INTENT(IN) :: this

CHARACTER(len=23) :: char

CALL getval(this, isodate=char)

END FUNCTION datetime_to_char


FUNCTION trim_datetime_to_char(in) RESULT(char)
TYPE(datetime),INTENT(IN) :: in ! value to be represented as CHARACTER

CHARACTER(len=len_trim(datetime_to_char(in))) :: char

char=datetime_to_char(in)

END FUNCTION trim_datetime_to_char



SUBROUTINE display_datetime(this)
TYPE(datetime),INTENT(in) :: this

print*,"TIME: ",to_char(this)

end subroutine display_datetime



SUBROUTINE display_timedelta(this)
TYPE(timedelta),INTENT(in) :: this

print*,"TIMEDELTA: ",to_char(this)

end subroutine display_timedelta



ELEMENTAL FUNCTION c_e_datetime(this) result (res)
TYPE(datetime),INTENT(in) :: this
LOGICAL :: res

res = .not. this == datetime_miss 

end FUNCTION c_e_datetime


ELEMENTAL FUNCTION datetime_eq(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
LOGICAL :: res

res = this%iminuti == that%iminuti

END FUNCTION datetime_eq


FUNCTION datetime_eqsv(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION datetime_eqsv


elemental FUNCTION datetime_ne(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION datetime_ne


FUNCTION datetime_nesv(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION datetime_nesv


elemental FUNCTION datetime_gt(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
LOGICAL :: res

res = this%iminuti > that%iminuti

END FUNCTION datetime_gt


FUNCTION datetime_gtsv(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this > that(i)
ENDDO

END FUNCTION datetime_gtsv


elemental FUNCTION datetime_lt(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
LOGICAL :: res

res = this%iminuti < that%iminuti

END FUNCTION datetime_lt


FUNCTION datetime_ltsv(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this < that(i)
ENDDO

END FUNCTION datetime_ltsv


elemental FUNCTION datetime_ge(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this > that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION datetime_ge


FUNCTION datetime_gesv(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this >= that(i)
ENDDO

END FUNCTION datetime_gesv


elemental FUNCTION datetime_le(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this < that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION datetime_le


FUNCTION datetime_lesv(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this <= that(i)
ENDDO

END FUNCTION datetime_lesv


FUNCTION datetime_add(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this
TYPE(timedelta),INTENT(IN) :: that
TYPE(datetime) :: res

INTEGER :: lyear, lmonth, lday, lhour, lminute, lmsec

IF (this == datetime_miss .OR. that == timedelta_miss) THEN
  res = datetime_miss
ELSE
  res%iminuti = this%iminuti + that%iminuti
  IF (that%month /= 0) THEN
    CALL getval(res, year=lyear, month=lmonth, day=lday, hour=lhour, &
     minute=lminute, msec=lmsec)
    CALL init(res, year=lyear, month=lmonth+that%month, day=lday, &
     hour=lhour, minute=lminute, msec=lmsec)
  ENDIF
ENDIF

END FUNCTION datetime_add


ELEMENTAL FUNCTION datetime_subdt(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
TYPE(timedelta) :: res

IF (this == datetime_miss .OR. that == datetime_miss) THEN
  res = timedelta_miss
ELSE
  res%iminuti = this%iminuti - that%iminuti
  res%month = 0
ENDIF

END FUNCTION datetime_subdt


FUNCTION datetime_subtd(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this
TYPE(timedelta),INTENT(IN) :: that
TYPE(datetime) :: res

INTEGER :: lyear, lmonth, lday, lhour, lminute, lmsec

IF (this == datetime_miss .OR. that == timedelta_miss) THEN
  res = datetime_miss
ELSE
  res%iminuti = this%iminuti - that%iminuti
  IF (that%month /= 0) THEN
    CALL getval(res, year=lyear, month=lmonth, day=lday, hour=lhour, &
     minute=lminute, msec=lmsec)
    CALL init(res, year=lyear, month=lmonth-that%month, day=lday, &
     hour=lhour, minute=lminute, msec=lmsec)
  ENDIF
ENDIF

END FUNCTION datetime_subtd


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE datetime_read_unit(this, unit)
TYPE(datetime),INTENT(out) :: this !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit
CALL datetime_vect_read_unit((/this/), unit)

END SUBROUTINE datetime_read_unit


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE datetime_vect_read_unit(this, unit)
TYPE(datetime) :: this(:) !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit

CHARACTER(len=40) :: form
CHARACTER(len=23), ALLOCATABLE :: dateiso(:)
INTEGER :: i

ALLOCATE(dateiso(SIZE(this)))
INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,'(A23,1X)')dateiso
ELSE
  READ(unit)dateiso
ENDIF
DO i = 1, SIZE(dateiso)
  CALL init(this(i), isodate=dateiso(i))
ENDDO
DEALLOCATE(dateiso)

END SUBROUTINE datetime_vect_read_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE datetime_write_unit(this, unit)
TYPE(datetime),INTENT(in) :: this !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

CALL datetime_vect_write_unit((/this/), unit)

END SUBROUTINE datetime_write_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE datetime_vect_write_unit(this, unit)
TYPE(datetime),INTENT(in) :: this(:) !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

CHARACTER(len=40) :: form
CHARACTER(len=23), ALLOCATABLE :: dateiso(:)
INTEGER :: i

ALLOCATE(dateiso(SIZE(this)))
DO i = 1, SIZE(dateiso)
  CALL getval(this(i), isodate=dateiso(i))
ENDDO
INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,'(A23,1X)')dateiso
ELSE
  WRITE(unit)dateiso
ENDIF
DEALLOCATE(dateiso)

END SUBROUTINE datetime_vect_write_unit


#include "arrayof_post.F90"


! ===============
! == timedelta ==
! ===============
!> Costruisce un oggetto \a timedelta con i parametri opzionali forniti.
!! Se non viene passato nulla lo inizializza a intervallo di durata nulla.
!! L'intervallo ottenuto � pari alla somma dei valori di tutti i parametri
!! forniti, ovviamente non fornire un parametro equivale a fornirlo =0.
!! Questa � la versione \c FUNCTION, in stile F2003, del costruttore, da preferire
!! rispetto alla versione \c SUBROUTINE \c init.
FUNCTION timedelta_new(year, month, day, hour, minute, msec, &
 isodate, simpledate, oraclesimdate) RESULT (this)
INTEGER,INTENT(IN),OPTIONAL :: year !< anni, se presente l'oggetto diventa "popolare"
INTEGER,INTENT(IN),OPTIONAL :: month !< mesi, se presente l'oggetto diventa "popolare"
INTEGER,INTENT(IN),OPTIONAL :: day !< giorni
INTEGER,INTENT(IN),OPTIONAL :: hour !< ore
INTEGER,INTENT(IN),OPTIONAL :: minute !< minuti
INTEGER,INTENT(IN),OPTIONAL :: msec !< millisecondi
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate !< inizializza l'oggetto ad un intervallo nel formato \c GGGGGGGGGG \c hh:mm:ss.msc, ignorando tutti gli altri parametri
CHARACTER(len=*),INTENT(IN),OPTIONAL :: simpledate !< inizializza l'oggetto ad un intervallo nel formato \c GGGGGGGGhhmmmsc, ignorando tutti gli altri parametri, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate !< inizializza l'oggetto ad un intervallo nel formato \c GGGGGGGGhhmm, ignorando tutti gli altri parametri

TYPE(timedelta) :: this !< oggetto da inizializzare

CALL timedelta_init(this, year, month, day, hour, minute, msec, &
 isodate, simpledate, oraclesimdate)

END FUNCTION timedelta_new


!> Costruisce un oggetto \a timedelta con i parametri opzionali forniti.
!! Se non viene passato nulla lo inizializza a intervallo di durata nulla.
!! L'intervallo ottenuto � pari alla somma dei valori di tutti i parametri
!! forniti, ovviamente non fornire un parametro equivale a fornirlo =0.
SUBROUTINE timedelta_init(this, year, month, day, hour, minute, msec, &
 isodate, simpledate, oraclesimdate)
TYPE(timedelta),INTENT(INOUT) :: this !< oggetto da inizializzare
INTEGER,INTENT(IN),OPTIONAL :: year !< anni, se presente l'oggetto diventa "popolare"
INTEGER,INTENT(IN),OPTIONAL :: month !< mesi, se presente l'oggetto diventa "popolare"
INTEGER,INTENT(IN),OPTIONAL :: day !< giorni
INTEGER,INTENT(IN),OPTIONAL :: hour !< ore
INTEGER,INTENT(IN),OPTIONAL :: minute !< minuti
INTEGER,INTENT(IN),OPTIONAL :: msec !< millisecondi
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate !< inizializza l'oggetto ad un intervallo nel formato \c AAAAMMGGGG \c hh:mm:ss.msc, ignorando tutti gli altri parametri, se \c AAAA o \c MM sono diversi da 0 l'oggetto diventa "popolare"
CHARACTER(len=*),INTENT(IN),OPTIONAL :: simpledate !< inizializza l'oggetto ad un intervallo nel formato \c GGGGGGGGhhmmmsc, ignorando tutti gli altri parametri, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate !< inizializza l'oggetto ad un intervallo nel formato \c GGGGGGGGhhmm, ignorando tutti gli altri parametri

INTEGER :: n, l, lyear, lmonth, d, h, m, s, ms
CHARACTER(len=23) :: datebuf

this%month = 0

IF (PRESENT(isodate)) THEN
  datebuf(1:23) = '0000000000 00:00:00.000'
  l = LEN_TRIM(isodate)
!  IF (l > 0) THEN
  n = INDEX(TRIM(isodate), ' ') ! align blank space separator
  IF (n > 0) THEN
    IF (n > 11 .OR. n < l - 12) GOTO 200 ! wrong format
    datebuf(12-n:12-n+l-1) = isodate(:l)
  ELSE
    datebuf(1:l) = isodate(1:l)
  ENDIF
!  ENDIF
  
!  datebuf(1:MIN(LEN(isodate),23)) = isodate(1:MIN(LEN(isodate),23))
  READ(datebuf,'(I4,I2,I4,1X,I2,1X,I2,1X,I2,1X,I3)', err=200) lyear, lmonth, d, &
   h, m, s, ms
  this%month = lmonth + 12*lyear
  this%iminuti = 86400000_int_ll*INT(d, KIND=int_ll) + &
   3600000_int_ll*INT(h, KIND=int_ll) + 60000_int_ll*INT(m, KIND=int_ll) + &
   1000_int_ll*INT(s, KIND=int_ll) + INT(ms, KIND=int_ll)
  RETURN

200 CONTINUE ! condizione di errore in isodate
  CALL delete(this)
  CALL l4f_log(L4F_ERROR, 'isodate '//TRIM(isodate)//' not valid')
  CALL raise_error()

ELSE IF (PRESENT(simpledate)) THEN
  datebuf(1:17) = '00000000000000000'
  datebuf(1:MIN(LEN(simpledate),17)) = simpledate(1:MIN(LEN(simpledate),17))
  READ(datebuf,'(I8.8,3I2.2,I3.3)', err=220) d, h, m, s, ms
  this%iminuti = 86400000_int_ll*INT(d, KIND=int_ll) + &
   3600000_int_ll*INT(h, KIND=int_ll) + 60000_int_ll*INT(m, KIND=int_ll) + &
   1000_int_ll*INT(s, KIND=int_ll) + INT(ms, KIND=int_ll)

220 CONTINUE ! condizione di errore in simpledate
  CALL delete(this)
  CALL l4f_log(L4F_ERROR, 'simpledate '//TRIM(simpledate)//' not valid')
  CALL raise_error()
  RETURN

ELSE IF (PRESENT(oraclesimdate)) THEN
    CALL l4f_log(L4F_WARN, 'in timedelta_init, parametro oraclesimdate '// &
   'obsoleto, usare piuttosto simpledate')
  READ(oraclesimdate, '(I8,2I2)')d, h, m
  this%iminuti = 86400000_int_ll*INT(d, KIND=int_ll) + &
   3600000_int_ll*INT(h, KIND=int_ll) + 60000_int_ll*INT(m, KIND=int_ll)

ELSE IF (.not. present(year) .and. .not. present(month) .and. .not. present(day)&
 .and. .not. present(hour) .and. .not. present(minute) .and. .not. present(msec)&
 .and. .not. present(isodate) .and. .not. present(simpledate) .and. .not. present(oraclesimdate)) THEN

  this=timedelta_miss

ELSE 
  this%iminuti = 0
  IF (PRESENT(year)) THEN
    if (c_e(year))then
      this%month = this%month + year*12
    else
      this=timedelta_miss
      return
    end if
  ENDIF
  IF (PRESENT(month)) THEN
    if (c_e(month))then
      this%month = this%month + month
    else
      this=timedelta_miss
      return
    end if
  ENDIF
  IF (PRESENT(day)) THEN
    if (c_e(day))then
      this%iminuti = this%iminuti + 86400000_int_ll*INT(day, KIND=int_ll)
    else
      this=timedelta_miss
      return
    end if
  ENDIF
  IF (PRESENT(hour)) THEN
    if (c_e(hour))then
      this%iminuti = this%iminuti + 3600000_int_ll*INT(hour, KIND=int_ll)
    else
      this=timedelta_miss
      return
    end if
  ENDIF
  IF (PRESENT(minute)) THEN
    if (c_e(minute))then
      this%iminuti = this%iminuti + 60000_int_ll*INT(minute, KIND=int_ll)
    else
      this=timedelta_miss
      return
    end if
  ENDIF
  IF (PRESENT(msec)) THEN
    if (c_e(msec))then
      this%iminuti = this%iminuti + msec
    else
      this=timedelta_miss
      return
    end if
  ENDIF
ENDIF




END SUBROUTINE timedelta_init


SUBROUTINE timedelta_delete(this)
TYPE(timedelta),INTENT(INOUT) :: this

this%iminuti = imiss
this%month = 0

END SUBROUTINE timedelta_delete


!> Restituisce il valore di un oggetto \a timedelta in una o pi�
!! modalit� desiderate. Qualsiasi combinazione dei parametri
!! opzionali � consentita. \a oraclesimedate �
!! obsoleto, usare piuttosto \a simpledate.
PURE SUBROUTINE timedelta_getval(this, year, month, amonth, &
 day, hour, minute, sec, msec, &
 ahour, aminute, asec, amsec, isodate, simpledate, oraclesimdate)
TYPE(timedelta),INTENT(IN) :: this !< oggetto di cui restituire il valore
INTEGER,INTENT(OUT),OPTIONAL :: year !< anni, /=0 solo per intervalli "popolari"
INTEGER,INTENT(OUT),OPTIONAL :: month !< mesi modulo 12, /=0 solo per intervalli "popolari"
INTEGER,INTENT(OUT),OPTIONAL :: amonth !< mesi totali, /=0 solo per intervalli "popolari"
INTEGER,INTENT(OUT),OPTIONAL :: day !< giorni totali
INTEGER,INTENT(OUT),OPTIONAL :: hour !< ore modulo 24
INTEGER,INTENT(OUT),OPTIONAL :: minute !< minuti modulo 60
INTEGER,INTENT(OUT),OPTIONAL :: sec !< secondi modulo 60
INTEGER,INTENT(OUT),OPTIONAL :: msec !< millisecondi modulo 1000
INTEGER,INTENT(OUT),OPTIONAL :: ahour !< ore totali
INTEGER,INTENT(OUT),OPTIONAL :: aminute !< minuti totali
INTEGER,INTENT(OUT),OPTIONAL :: asec !< secondi totali
INTEGER(kind=int_ll),INTENT(OUT),OPTIONAL :: amsec !< millisecondi totali
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: isodate !< intervallo totale nel formato \c GGGGGGGGGG \c hh:mm:ss.msc  (simil-ISO), la variabile pu� essere pi� corta di 23 caratteri, in tal caso conterr� solo ci� che vi cape
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: simpledate  !< intervallo totale nel formato \c GGGGGGGGhhmmssmsc , la variabile pu� essere pi� corta di 17 caratteri, in tal caso conterr� solo ci� che vi cape, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(OUT),OPTIONAL :: oraclesimdate !< intervallo totale nel formato \c GGGGGGGGhhmm

CHARACTER(len=23) :: datebuf

IF (PRESENT(amsec)) THEN 
  amsec = this%iminuti
ENDIF
IF (PRESENT(asec)) THEN 
  asec = int(this%iminuti/1000_int_ll)
ENDIF
IF (PRESENT(aminute)) THEN 
  aminute = int(this%iminuti/60000_int_ll)
ENDIF
IF (PRESENT(ahour)) THEN
  ahour = int(this%iminuti/3600000_int_ll)
ENDIF
IF (PRESENT(msec)) THEN 
  msec = int(MOD(this%iminuti, 1000_int_ll))
ENDIF
IF (PRESENT(sec)) THEN 
  sec = int(MOD(this%iminuti/1000_int_ll, 60_int_ll))
ENDIF
IF (PRESENT(minute)) THEN 
  minute = int(MOD(this%iminuti/60000_int_ll, 60_int_ll))
ENDIF
IF (PRESENT(hour)) THEN
  hour = int(MOD(this%iminuti/3600000_int_ll, 24_int_ll))
ENDIF
IF (PRESENT(day)) THEN
  day = int(this%iminuti/86400000_int_ll)
ENDIF
IF (PRESENT(amonth)) THEN
  amonth = this%month
ENDIF
IF (PRESENT(month)) THEN
  month = MOD(this%month-1,12)+1
ENDIF
IF (PRESENT(year)) THEN
  year = this%month/12
ENDIF
IF (PRESENT(isodate)) THEN ! Non standard, inventato!
  WRITE(datebuf(1:23), '(I10.10,1X,I2.2,A1,I2.2,A1,I2.2,A1,I3.3)') &
   this%iminuti/86400000_int_ll, MOD(this%iminuti/3600000_int_ll, 24_int_ll), ':', &
   MOD(this%iminuti/60000_int_ll, 60_int_ll), ':', MOD(this%iminuti/1000_int_ll, 60_int_ll), &
   '.', MOD(this%iminuti, 1000_int_ll)
  isodate = datebuf(1:MIN(LEN(isodate),23))

ENDIF
IF (PRESENT(simpledate)) THEN
  WRITE(datebuf(1:17), '(I8.8,3I2.2,I3.3)') &
   this%iminuti/86400000_int_ll, MOD(this%iminuti/3600000_int_ll, 24_int_ll), &
   MOD(this%iminuti/60000_int_ll, 60_int_ll), MOD(this%iminuti/1000_int_ll, 60_int_ll), &
   MOD(this%iminuti, 1000_int_ll)
  simpledate = datebuf(1:MIN(LEN(simpledate),17))
ENDIF
IF (PRESENT(oraclesimdate)) THEN
!!$  CALL l4f_log(L4F_WARN, 'in timedelta_getval, parametro oraclesimdate '// &
!!$   'obsoleto, usare piuttosto simpledate')
  WRITE(oraclesimdate, '(I8.8,2I2.2)') this%iminuti/86400000_int_ll, &
   MOD(this%iminuti/3600000_int_ll, 24_int_ll), MOD(this%iminuti/60000_int_ll, 60_int_ll)
ENDIF

END SUBROUTINE timedelta_getval


!> Restituisce una rappresentazione carattere stampabile di un oggetto
!! \a timedelta.
elemental FUNCTION timedelta_to_char(this) RESULT(char)
TYPE(timedelta),INTENT(IN) :: this

CHARACTER(len=23) :: char

CALL getval(this, isodate=char)

END FUNCTION timedelta_to_char


FUNCTION trim_timedelta_to_char(in) RESULT(char)
TYPE(timedelta),INTENT(IN) :: in ! value to be represented as CHARACTER

CHARACTER(len=len_trim(timedelta_to_char(in))) :: char

char=timedelta_to_char(in)

END FUNCTION trim_timedelta_to_char


!> Restituisce il valore in millisecondi totali di un oggetto \a timedelta.
elemental FUNCTION timedelta_getamsec(this)
TYPE(timedelta),INTENT(IN) :: this !< oggetto di cui restituire il valore
INTEGER(kind=int_ll) :: timedelta_getamsec !< millisecondi totali

timedelta_getamsec = this%iminuti

END FUNCTION timedelta_getamsec


!> Depopularize a \a timedelta object.
!! If the object represents a "popular" or mixed interval, a fixed
!! interval representation is returned, by recomputing it on a
!! standard period (starting from 1970-01-01); if it represents
!! already a fixed interval, the same object is returned.
FUNCTION timedelta_depop(this)
TYPE(timedelta),INTENT(IN) :: this !< object to be depopularized
TYPE(timedelta) :: timedelta_depop

TYPE(datetime) :: tmpdt

IF (this%month == 0) THEN
  timedelta_depop = this
ELSE
  tmpdt = datetime_new(1970, 1, 1)
  timedelta_depop = (tmpdt + this) - tmpdt
ENDIF

END FUNCTION timedelta_depop


elemental FUNCTION timedelta_eq(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%iminuti == that%iminuti .AND. this%month == that%month)

END FUNCTION timedelta_eq


FUNCTION timedelta_eqsv(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION timedelta_eqsv


elemental FUNCTION timedelta_ne(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION timedelta_ne


FUNCTION timedelta_nesv(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION timedelta_nesv


elemental FUNCTION timedelta_gt(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
LOGICAL :: res

res = this%iminuti > that%iminuti

END FUNCTION timedelta_gt


FUNCTION timedelta_gtsv(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this > that(i)
ENDDO

END FUNCTION timedelta_gtsv


elemental FUNCTION timedelta_lt(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
LOGICAL :: res

res = this%iminuti < that%iminuti

END FUNCTION timedelta_lt


FUNCTION timedelta_ltsv(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this < that(i)
ENDDO

END FUNCTION timedelta_ltsv


elemental FUNCTION timedelta_ge(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this > that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION timedelta_ge


FUNCTION timedelta_gesv(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this >= that(i)
ENDDO

END FUNCTION timedelta_gesv


elemental FUNCTION timedelta_le(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this < that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION timedelta_le


FUNCTION timedelta_lesv(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this <= that(i)
ENDDO

END FUNCTION timedelta_lesv


FUNCTION timedelta_add(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
TYPE(timedelta) :: res

res%iminuti = this%iminuti + that%iminuti
res%month = this%month + that%month

END FUNCTION timedelta_add


FUNCTION timedelta_sub(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
TYPE(timedelta) :: res

res%iminuti = this%iminuti - that%iminuti
res%month = this%month - that%month

END FUNCTION timedelta_sub


FUNCTION timedelta_mult(this, n) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this
INTEGER,INTENT(IN) :: n
TYPE(timedelta) :: res

res%iminuti = this%iminuti*n
res%month = this%month*n

END FUNCTION timedelta_mult


FUNCTION timedelta_tlum(n, this) RESULT(res)
INTEGER,INTENT(IN) :: n
TYPE(timedelta),INTENT(IN) :: this
TYPE(timedelta) :: res

res%iminuti = this%iminuti*n
res%month = this%month*n

END FUNCTION timedelta_tlum


FUNCTION timedelta_divint(this, n) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this
INTEGER,INTENT(IN) :: n
TYPE(timedelta) :: res

res%iminuti = this%iminuti/n
res%month = this%month/n

END FUNCTION timedelta_divint


FUNCTION timedelta_divtd(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
INTEGER :: res

res = int(this%iminuti/that%iminuti)

END FUNCTION timedelta_divtd


elemental FUNCTION timedelta_mod(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
TYPE(timedelta) :: res

res%iminuti = MOD(this%iminuti, that%iminuti)
res%month = 0

END FUNCTION timedelta_mod


elemental FUNCTION datetime_timedelta_mod(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this
TYPE(timedelta),INTENT(IN) :: that
TYPE(timedelta) :: res

IF (that%iminuti == 0) THEN ! Controllo nel caso di intervalli "umani" o nulli
  res = timedelta_0
ELSE
  res%iminuti = MOD(this%iminuti, that%iminuti)
  res%month = 0
ENDIF

END FUNCTION datetime_timedelta_mod


FUNCTION timedelta_abs(this) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this
TYPE(timedelta) :: res

res%iminuti = ABS(this%iminuti)
res%month = ABS(this%month)

END FUNCTION timedelta_abs


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE timedelta_read_unit(this, unit)
TYPE(timedelta),INTENT(out) :: this !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit

CALL timedelta_vect_read_unit((/this/), unit)

END SUBROUTINE timedelta_read_unit


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE timedelta_vect_read_unit(this, unit)
TYPE(timedelta) :: this(:) !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit

CHARACTER(len=40) :: form
CHARACTER(len=23), ALLOCATABLE :: dateiso(:)
INTEGER :: i

ALLOCATE(dateiso(SIZE(this)))
INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,'(3(A23,1X))')dateiso
ELSE
  READ(unit)dateiso
ENDIF
DO i = 1, SIZE(dateiso)
  CALL init(this(i), isodate=dateiso(i))
ENDDO
DEALLOCATE(dateiso)

END SUBROUTINE timedelta_vect_read_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE timedelta_write_unit(this, unit)
TYPE(timedelta),INTENT(in) :: this !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

CALL timedelta_vect_write_unit((/this/), unit)

END SUBROUTINE timedelta_write_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE timedelta_vect_write_unit(this, unit)
TYPE(timedelta),INTENT(in) :: this(:) !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

CHARACTER(len=40) :: form
CHARACTER(len=23), ALLOCATABLE :: dateiso(:)
INTEGER :: i

ALLOCATE(dateiso(SIZE(this)))
DO i = 1, SIZE(dateiso)
  CALL getval(this(i), isodate=dateiso(i))
ENDDO
INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,'(3(A23,1X))')dateiso
ELSE
  WRITE(unit)dateiso
ENDIF
DEALLOCATE(dateiso)

END SUBROUTINE timedelta_vect_write_unit


elemental SUBROUTINE jeladata5(iday,imonth,iyear,ihour,imin,iminuti)

!!omstart JELADATA5
!     SUBROUTINE JELADATA5(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
!     1                 IMINUTI)
!
!     Calcola i minuti trascorsi tra il 1/1/1 e la data fornita
!
!     variabili integer*4
!     IN:
!     IDAY,IMONTH,IYEAR,  I*4
!     IHOUR,IMIN                GIORNO MESE ANNO ORE MINUTI
!
!     OUT:
!     IMINUTI           I*4     MINUTI AD INIZIARE DALLE ORE 00 DEL 1/1/1
!!OMEND

INTEGER,intent(in) :: iday, imonth, iyear, ihour, imin
INTEGER,intent(out) :: iminuti

iminuti = ndays(iday,imonth,iyear)*1440+(ihour*60)+imin

END SUBROUTINE jeladata5


elemental SUBROUTINE jeladata5_1(iday,imonth,iyear,ihour,imin,imsec,imillisec)
INTEGER,intent(in) :: iday, imonth, iyear, ihour, imin, imsec
INTEGER(KIND=int_ll),intent(out) :: imillisec

imillisec = INT(ndays(iday,imonth,iyear)*1440+(ihour*60)+imin, KIND=int_ll)*60000 &
 + imsec

END SUBROUTINE jeladata5_1



elemental SUBROUTINE jeladata6(iday, imonth, iyear, ihour, imin, iminuti)

!!omstart JELADATA6
!     SUBROUTINE JELADATA6(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
!     1                 IMINUTI)
!
!     Calcola la data e l'ora corrispondente a IMINUTI dopo il
!     1/1/1
!
!     variabili integer*4
!     IN:
!     IMINUTI           I*4     MINUTI AD INIZIARE DALLE ORE 00 DEL 1/1/1
!
!     OUT:
!     IDAY,IMONTH,IYEAR,  I*4
!     IHOUR,IMIN                GIORNO MESE ANNO ORE MINUTI
!!OMEND


INTEGER,intent(in)  :: iminuti
INTEGER,intent(out) :: iday, imonth, iyear, ihour, imin

INTEGER ::igiorno

imin = MOD(iminuti,60)
ihour = MOD(iminuti,1440)/60
igiorno = iminuti/1440
IF (MOD(iminuti,1440) < 0) igiorno = igiorno-1
CALL ndyin(igiorno,iday,imonth,iyear)

END SUBROUTINE jeladata6


elemental SUBROUTINE jeladata6_1(iday, imonth, iyear, ihour, imin, imsec, imillisec)
INTEGER(KIND=int_ll), INTENT(IN) :: imillisec
INTEGER, INTENT(OUT) :: iday, imonth, iyear, ihour, imin, imsec

INTEGER :: igiorno

imsec = int(MOD(imillisec, 60000_int_ll)) ! partial msec
!imin = MOD(imillisec/60000_int_ll, 60)
!ihour = MOD(imillisec/3600000_int_ll, 24)
imin = int(MOD(imillisec, 3600000_int_ll)/60000_int_ll)
ihour = int(MOD(imillisec, 86400000_int_ll)/3600000_int_ll)
igiorno = int(imillisec/86400000_int_ll)
!IF (MOD(imillisec,1440) < 0) igiorno = igiorno-1 !?!?!?
CALL ndyin(igiorno,iday,imonth,iyear)

END SUBROUTINE jeladata6_1


elemental SUBROUTINE ndyin(ndays,igg,imm,iaa)

!!OMSTART NDYIN
!     SUBROUTINE NDYIN(NDAYS,IGG,IMM,IAA)
!     restituisce la data fornendo in input il numero di
!     giorni dal 1/1/1
!
!!omend

INTEGER,intent(in) :: ndays
INTEGER,intent(out) :: igg, imm, iaa
integer :: n,lndays

lndays=ndays

n = lndays/d400
lndays = lndays - n*d400
iaa = year0 + n*400
n = MIN(lndays/d100, 3)
lndays = lndays - n*d100
iaa = iaa + n*100
n = lndays/d4
lndays = lndays - n*d4
iaa = iaa + n*4
n = MIN(lndays/d1, 3)
lndays = lndays - n*d1
iaa = iaa + n
n = bisextilis(iaa)
DO imm = 1, 12
  IF (lndays < ianno(imm+1,n)) EXIT
ENDDO
igg = lndays+1-ianno(imm,n) ! +1 perche' il mese parte da 1

END SUBROUTINE ndyin


integer elemental FUNCTION ndays(igg,imm,iaa)

!!OMSTART NDAYS
!     FUNCTION NDAYS(IGG,IMM,IAA)
!     restituisce  il numero di giorni dal 1/1/1
!     fornendo in input la data
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     nota bene                   E' SICURO !!!
!     un anno e' bisestile se divisibile per 4
!     un anno rimane bisestile se divisibile per 400
!     un anno NON e' bisestile se divisibile per 100
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!!omend

INTEGER, intent(in) :: igg, imm, iaa

INTEGER :: lmonth, lyear

! Limito il mese a [1-12] e correggo l'anno coerentemente
lmonth = MODULO(imm-1, 12) + 1 ! uso MODULO e non MOD per gestire bene i valori <0
lyear = iaa + (imm - lmonth)/12
ndays = igg+ianno(lmonth, bisextilis(lyear))
ndays = ndays-1 + 365*(lyear-year0) + (lyear-year0)/4 - (lyear-year0)/100 + &
 (lyear-year0)/400

END FUNCTION ndays


elemental FUNCTION bisextilis(annum)
INTEGER,INTENT(in) :: annum
INTEGER :: bisextilis

IF (MOD(annum,4) == 0 .AND. (MOD(annum,400) == 0 .EQV. MOD(annum,100) == 0)) THEN
  bisextilis = 2
ELSE
 bisextilis = 1
ENDIF
END FUNCTION bisextilis


ELEMENTAL FUNCTION cyclicdatetime_eq(this, that) RESULT(res)
TYPE(cyclicdatetime),INTENT(IN) :: this, that
LOGICAL :: res

res = .true.
if (this%minute /= that%minute) res=.false.
if (this%hour /= that%hour) res=.false.
if (this%day /= that%day) res=.false.
if (this%month /= that%month) res=.false.
if (this%year /= that%year) res=.false.

END FUNCTION cyclicdatetime_eq


ELEMENTAL FUNCTION cyclicdatetime_datetime_eq(this, that) RESULT(res)
TYPE(cyclicdatetime),INTENT(IN) :: this
TYPE(datetime),INTENT(IN) :: that
LOGICAL :: res

integer :: minute,hour,day,month,year

call getval(that,minute=minute,hour=hour,day=day,month=month,year=year)

res = .true.
if (c_e(this%minute) .and. this%minute /= minute) res=.false.
if (c_e(this%hour) .and. this%hour /= hour) res=.false.
if (c_e(this%day) .and. this%day /= day) res=.false.
if (c_e(this%month) .and. this%month /= month) res=.false.
if (c_e(this%year) .and. this%year /= year) res=.false.

END FUNCTION cyclicdatetime_datetime_eq


ELEMENTAL FUNCTION datetime_cyclicdatetime_eq(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this
TYPE(cyclicdatetime),INTENT(IN) :: that
LOGICAL :: res

integer :: minute,hour,day,month,year

call getval(this,minute=minute,hour=hour,day=day,month=month,year=year)

res = .true.
if (c_e(that%minute) .and. that%minute /= minute) res=.false.
if (c_e(that%hour) .and. that%hour /= hour) res=.false.
if (c_e(that%day) .and. that%day /= day) res=.false.
if (c_e(that%month) .and. that%month /= month) res=.false.
if (c_e(that%year) .and. that%year /= year) res=.false.

END FUNCTION datetime_cyclicdatetime_eq

ELEMENTAL FUNCTION c_e_cyclicdatetime(this) result (res)
TYPE(cyclicdatetime),INTENT(in) :: this
LOGICAL :: res

res = .not. this == cyclicdatetime_miss 

end FUNCTION c_e_cyclicdatetime


!> Costruisce un oggetto \a cyclicdatetime con i parametri opzionali forniti.
!! Se non viene passato nulla lo inizializza a missing.
FUNCTION cyclicdatetime_new(year, month, day, hour, minute, chardate) RESULT(this)
INTEGER,INTENT(IN),OPTIONAL :: year !< anno d.C., sono ammessi solo anni >0 (d.C.)
INTEGER,INTENT(IN),OPTIONAL :: month !< mese, default=missing
INTEGER,INTENT(IN),OPTIONAL :: day !< mese, default=missing
INTEGER,INTENT(IN),OPTIONAL :: hour !< ore, default=missing
INTEGER,INTENT(IN),OPTIONAL :: minute !< minuti, default=missing
CHARACTER(len=12),INTENT(IN),OPTIONAL :: chardate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmm where any doubled char should be // for missing.

integer :: lyear,lmonth,lday,lhour,lminute,ios


TYPE(cyclicdatetime) :: this !< oggetto da inizializzare

if (present(chardate)) then
!  AAAAMMGGhhmm
  read(chardate(1:4),*,iostat=ios)lyear
  if (ios /= 0)lyear=imiss

  read(chardate(5:6),*,iostat=ios)lmonth
  if (ios /= 0)lmonth=imiss

  read(chardate(7:8),*,iostat=ios)lday
  if (ios /= 0)lday=imiss

  read(chardate(9:10),*,iostat=ios)lhour
  if (ios /= 0)lhour=imiss

  read(chardate(11:12),*,iostat=ios)lminute
  if (ios /= 0)lminute=imiss

  this%year=lyear
  this%month=lmonth
  this%day=lday
  this%hour=lhour
  this%minute=lminute
else
  this%year=optio_l(year)
  this%month=optio_l(month)
  this%day=optio_l(day)
  this%hour=optio_l(hour)
  this%minute=optio_l(minute)
end if

END FUNCTION cyclicdatetime_new



#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE TYPE(datetime)
#define VOL7D_POLY_TYPES _datetime
#define ENABLE_SORT
#include "array_utilities_inc.F90"
#undef ENABLE_SORT

END MODULE datetime_class

