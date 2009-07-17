!> \brief Classi per la gestione delle coordinate temporali.
!! 
!! Questo module definisce un paio di classi per la gestione di
!! date assolute e di intervalli temporali.
!! Entrambe le classi hanno le componenti di tipo \c PRIVATE, per
!! cui non possono essere manipolate direttamente ma solo tramite i
!! relativi metodi.  Attualmente la precisione massima consentita è di un
!! minuto, mentre l'estensione delle date rappresentabili va dall'anno 1
!! all'anno 4074 d.C. circa, ipotizzando un calendario gregoriano per
!! tutto il periodo.  Questo fatto implica che le date precedenti
!! all'introduzione del calendario gregoriano avranno discrepanze di uno
!! o più giorni rispetto alle date storiche "vere", ammesso che
!! qualcuno conosca queste ultime.
!! \ingroup base
MODULE datetime_class
USE kinds
USE log4fortran
USE err_handling
USE missing_values
IMPLICIT NONE

INTEGER, PARAMETER :: dateint=SELECTED_INT_KIND(13)

!> Classe che indica un istante temporale assoluto.
TYPE datetime
  PRIVATE
  INTEGER(KIND=int_ll) :: iminuti
END TYPE  datetime

!> Classe che indica un intervallo temporale relativo.
!! Può assumere anche valori <0. Può assumere  valori "puri", cioè
!! intervalli temporali di durata fissa, e valori "popolari", cioè
!! intervalli di durata variabile in unità di mesi e/o anni, o anche
!! valori misti, cioè mesi/anni + un intervallo fisso; tuttavia negli
!! ultimi 2 casi le operazioni che si possono effettuare con
!! oggetti di questa classe sono limitate.
TYPE timedelta
  PRIVATE
  INTEGER(KIND=int_ll) :: iminuti
  INTEGER :: month
END TYPE timedelta

!> valore mancante per datetime
TYPE(datetime), PARAMETER :: datetime_miss=datetime(imiss)
!> valore mancante per timedelta
TYPE(timedelta), PARAMETER :: timedelta_miss=timedelta(imiss, 0)
!> intervallo timedelta di durata nulla
TYPE(timedelta), PARAMETER :: timedelta_0=timedelta(0, 0)
!> inizializza con l'ora UTC
INTEGER, PARAMETER :: datetime_utc=1
!> inizializza con l'ora locale
INTEGER, PARAMETER :: datetime_local=2

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
 unmin=1035593280, & ! differenza tra 01/01/1970 e 01/01/0001 (min, per unixtime)
 ianno(13,2)=RESHAPE((/ &
 0,31,59,90,120,151,181,212,243,273,304,334,365, &
 0,31,60,91,121,152,182,213,244,274,305,335,366/),(/13,2/))

INTEGER(KIND=int_ll),PARAMETER :: &
 unsec=62135596800_int_ll ! differenza tra 01/01/1970 e 01/01/0001 (sec, per unixtime)

PRIVATE
PUBLIC datetime, datetime_miss, datetime_utc, datetime_local, &
 datetime_new, init, delete, getval, to_char, &
 read_unit, write_unit, &
 OPERATOR(==), OPERATOR(/=), OPERATOR(>), OPERATOR(<), &
 OPERATOR(>=), OPERATOR(<=), OPERATOR(+), OPERATOR(-), &
 OPERATOR(*), OPERATOR(/), mod, &
 timedelta, timedelta_miss, timedelta_new, timedelta_0, timedelta_getamsec,&
 display

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

!> Operatore logico di uguaglianza tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE datetime_eq, datetime_eqsv, timedelta_eq, timedelta_eqsv
END INTERFACE

!> Operatore logico di disuguaglianza tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE datetime_ne, datetime_nesv, timedelta_ne, timedelta_nesv
END INTERFACE

!> Operatore logico maggiore tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari). Nel caso di valori mancanti il risultato
!! non è specificato. Il risultato non è altresì specificato nel caso di oggetti
!! \a timedelta "popolari" o misti.
INTERFACE OPERATOR (>)
  MODULE PROCEDURE datetime_gt, datetime_gtsv, timedelta_gt, timedelta_gtsv
END INTERFACE

!> Operatore logico minore tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari). Nel caso di valori mancanti il risultato
!! non è specificato. Il risultato non è altresì specificato nel caso di oggetti
!! \a timedelta "popolari" o misti.
INTERFACE OPERATOR (<)
  MODULE PROCEDURE datetime_lt, datetime_ltsv, timedelta_lt, timedelta_ltsv
END INTERFACE

!> Operatore logico maggiore-uguale tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari). Nel caso di valori mancanti il risultato
!! non è specificato. Il risultato non è altresì specificato nel caso di oggetti
!! \a timedelta "popolari" o misti.
INTERFACE OPERATOR (>=)
  MODULE PROCEDURE datetime_ge, datetime_gesv, timedelta_ge, timedelta_gesv
END INTERFACE

!> Operatore logico minore-uguale tra oggetti della stessa classe.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari). Nel caso di valori mancanti il risultato
!! non è specificato. Il risultato non è altresì specificato nel caso di oggetti
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
!! La prima combinazione è valida per tutti i tipi di intervallo, mentre la
!! seconda è definita solo per intervalli "puri".
INTERFACE OPERATOR (/)
  MODULE PROCEDURE timedelta_divint, timedelta_divtd
END INTERFACE

!> Operatore di resto della divisione. Sono definite le combinazioni:
!! - \a MOD(\a timedelta, \a timedelta) = \a timedelta
!! - \a MOD(\a datetime, \a timedelta) = \a timedelta
!! .
!! Sono definite solo per intervalli "puri"
!! La seconda combinazione ha senso principalmente con intervalli di
!! 1 minuto, 1 ora o
!! 1 giorno, per calcolare di quanto l'oggetto \a datetime indicato dista
!! dal minuto, ora o giorno tondo precedente più vicino.
INTERFACE mod
  MODULE PROCEDURE timedelta_mod, datetime_timedelta_mod
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
  MODULE PROCEDURE display_datetime
END INTERFACE


CONTAINS

! ==============
! == datetime ==
! ==============

!> Costruisce un oggetto \a datetime con i parametri opzionali forniti.
!! Se non viene passato nulla lo inizializza a 1/1/1.
!! Questa è la versione \c FUNCTION, in stile F2003, del costruttore, da preferire
!! rispetto alla versione \c SUBROUTINE \c init.
!! Notare che i gruppi di parametri opzionali (\a year, \a month, \a hour,
!! \a minute, \a msec), (\a unixtime), (\a isodate), (\a simpledate),
!! (\a oraclesimdate) sono mutualmente escludentesi; \a oraclesimedate è
!! obsoleto, usare piuttosto \a simpledate.
FUNCTION datetime_new(year, month, day, hour, minute, msec, &
 unixtime, isodate, simpledate, oraclesimdate, now) RESULT(this)
INTEGER,INTENT(IN),OPTIONAL :: year !< anno d.C., se è specificato, tutti gli eventuali parametri tranne \a month, \a day, \a hour, \a minute e \a msec sono ignorati; per un problema non risolto, sono ammessi solo anni >0 (d.C.)
INTEGER,INTENT(IN),OPTIONAL :: month !< mese, default=1 se è specificato \a year; può assumere anche valori <1 o >12, l'oggetto finale si aggiusta coerentemente
INTEGER,INTENT(IN),OPTIONAL :: day !< mese, default=1 se è specificato \a year; può anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: hour !< ore, default=0 se è specificato \a year; può anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: minute !< minuti, default=0 se è specificato \a year; può anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: msec !< millisecondi, default=0 se è specificato \a year; può anch'esso assumere valori fuori dai limiti canonici
INTEGER(kind=int_ll),INTENT(IN),OPTIONAL :: unixtime !< inizializza l'oggetto a \a unixtime secondi dopo il 1/1/1970 (convenzione UNIX, notare che il parametro deve essere un intero a 8 byte), se è presente tutto il resto è ignorato
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate !< inizializza l'oggetto ad una data espressa nel formato \c AAAA-MM-GG \c hh:mm:ss.msc, un sottoinsieme del formato noto come \a ISO, la parte iniziale \c AAAA-MM-GG è obbligatoria, il resto è opzionale
CHARACTER(len=*),INTENT(IN),OPTIONAL :: simpledate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmmssmsc, la parte iniziale \c AAAAMMGG è obbligatoria, il resto è opzionale, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmm, come nelle routine per l'accesso al db Oracle del SIM.
INTEGER,INTENT(IN),OPTIONAL :: now !< inizializza l'oggetto all'istante corrente, se \a è \a datetime_utc inizializza con l'ora UTC (preferibile), se è \a datetime_local usa l'ora locale

TYPE(datetime) :: this !< oggetto da inizializzare

CALL datetime_init(this, year, month, day, hour, minute, msec, &
 unixtime, isodate, simpledate, oraclesimdate, now)

END FUNCTION datetime_new

!> Costruisce un oggetto \a datetime con i parametri opzionali forniti.
!! Se non viene passato nulla lo inizializza a 1/1/1.
!! Notare che i gruppi di parametri opzionali (\a year, \a month, \a hour,
!! \a minute, \a msec), (\a unixtime), (\a isodate), (\a simpledate),
!! (\a oraclesimdate) sono mutualmente escludentesi; \a oraclesimedate è
!! obsoleto, usare piuttosto \a simpledate.
RECURSIVE SUBROUTINE datetime_init(this, year, month, day, hour, minute, msec, &
 unixtime, isodate, simpledate, oraclesimdate, now)
TYPE(datetime),INTENT(INOUT) :: this !< oggetto da inizializzare
INTEGER,INTENT(IN),OPTIONAL :: year !< anno d.C., se è specificato, tutti gli eventuali parametri tranne \a month, \a day, \a hour e \a minute sono ignorati; per un problema non risolto, sono ammessi solo anni >0 (d.C.)
INTEGER,INTENT(IN),OPTIONAL :: month !< mese, default=1 se è specificato \a year; può assumere anche valori <1 o >12, l'oggetto finale si aggiusta coerentemente
INTEGER,INTENT(IN),OPTIONAL :: day !< mese, default=1 se è specificato \a year; può anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: hour !< ore, default=0 se è specificato \a year; può anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: minute !< minuti, default=0 se è specificato \a year; può anch'esso assumere valori fuori dai limiti canonici
INTEGER,INTENT(IN),OPTIONAL :: msec !< millisecondi, default=0 se è specificato \a year; può anch'esso assumere valori fuori dai limiti canonici
INTEGER(kind=int_ll),INTENT(IN),OPTIONAL :: unixtime !< inizializza l'oggetto a \a unixtime secondi dopo il 1/1/1970 (convenzione UNIX, notare che il parametro deve essere un intero a 8 byte)
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate !< inizializza l'oggetto ad una data espressa nel formato \c AAAA-MM-GG \c hh:mm:ss.msc, un sottoinsieme del formato noto come \a ISO, la parte iniziale \c AAAA-MM-GG è obbligatoria, il resto è opzionale
CHARACTER(len=*),INTENT(IN),OPTIONAL :: simpledate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmmssmsc, la parte iniziale \c AAAAMMGG è obbligatoria, il resto è opzionale, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate !< inizializza l'oggetto ad una data espressa nel formato \c AAAAMMGGhhmm, come nelle routine per l'accesso al db Oracle del SIM.
INTEGER,INTENT(IN),OPTIONAL :: now !< inizializza l'oggetto all'istante corrente, se \a è \a datetime_utc inizializza con l'ora UTC (preferibile), se è \a datetime_local usa l'ora locale

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
  CALL jeladata5_1(lday, lmonth, lyear, lhour, lminute, lmsec, this%iminuti)
ELSE IF (PRESENT(unixtime)) THEN ! secondi dal 01/01/1970 (unix)
  this%iminuti = (unixtime + unsec)*1000
ELSE IF (PRESENT(isodate)) THEN ! formato iso YYYY-MM-DD hh:mm:ss.msc
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

ELSE IF (PRESENT(simpledate)) THEN ! formato YYYYMMDDhhmmssmsc
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

ELSE IF (PRESENT(oraclesimdate)) THEN ! formato YYYYMMDDhhmm
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
ELSE IF (PRESENT(now)) THEN
  CALL DATE_AND_TIME(values=dt)
  IF (now /= datetime_local) dt(6) = dt(6) - dt(4) ! back to UTC
  CALL init(this, year=dt(1), month=dt(2), day=dt(3), hour=dt(5), minute=dt(6), &
   msec=dt(7)*1000+dt(8))
ELSE
  this%iminuti = 0
ENDIF

END SUBROUTINE datetime_init


SUBROUTINE datetime_delete(this)
TYPE(datetime),INTENT(INOUT) :: this

this%iminuti = imiss

END SUBROUTINE datetime_delete


!> Restituisce il valore di un oggetto \a datetime in una o più
!! modalità desiderate. Qualsiasi combinazione dei parametri
!! opzionali è consentita. \a oraclesimedate è
!! obsoleto, usare piuttosto \a simpledate.
SUBROUTINE datetime_getval(this, year, month, day, hour, minute, msec, &
 unixtime, isodate, simpledate, oraclesimdate)
TYPE(datetime),INTENT(IN) :: this !< oggetto di cui restituire il valore
INTEGER,INTENT(OUT),OPTIONAL :: year !< anno
INTEGER,INTENT(OUT),OPTIONAL :: month !< mese
INTEGER,INTENT(OUT),OPTIONAL :: day !< giorno
INTEGER,INTENT(OUT),OPTIONAL :: hour !< ore
INTEGER,INTENT(OUT),OPTIONAL :: minute !< minuti
INTEGER,INTENT(OUT),OPTIONAL :: msec !< millisecondi
INTEGER(kind=int_ll),INTENT(OUT),OPTIONAL :: unixtime !< secondi a partire dal 1/1/1970
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: isodate !< data completa nel formato \c AAAA-MM-GG \c hh:mm:ss.msc (simil-ISO), la variabile può essere più corta di 23 caratteri, in tal caso conterrà solo ciò che vi cape
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: simpledate !< data completa nel formato \c AAAAMMGGhhmmssmsc , la variabile può essere più corta di 17 caratteri, in tal caso conterrà solo ciò che vi cape, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(OUT),OPTIONAL :: oraclesimdate !< data parziale nel formato \c AAAAMMGGhhmm

INTEGER :: lyear, lmonth, lday, lhour, lminute, lmsec, ier
CHARACTER(len=23) :: datebuf

IF (PRESENT(year) .OR. PRESENT(month) .OR. PRESENT(day) .OR. PRESENT(hour) &
 .OR. PRESENT(minute) .OR. PRESENT(msec) .OR. PRESENT(isodate) &
 .OR. PRESENT(simpledate) .OR. PRESENT(oraclesimdate)) THEN
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
    CALL l4f_log(L4F_WARN, 'in datetime_getval, parametro oraclesimdate '// &
     'obsoleto, usare piuttosto simpledate')
    WRITE(oraclesimdate, '(I4.4,4I2.2)') lyear, lmonth, lday, lhour, lminute
  ENDIF
ENDIF
IF (PRESENT(unixtime)) THEN
  unixtime = this%iminuti/1000_int_ll-unsec
ENDIF

END SUBROUTINE datetime_getval


!> Restituisce una rappresentazione carattere stampabile di un oggetto
!! \a datetime.
FUNCTION datetime_to_char(this) RESULT(char)
TYPE(datetime),INTENT(IN) :: this

CHARACTER(len=23) :: char

CALL getval(this, isodate=char)

END FUNCTION datetime_to_char


SUBROUTINE display_datetime(this)
TYPE(datetime),INTENT(in) :: this
character(len=17)         :: date_time

call getval (this,simpledate=date_time)

print*,"TIME: ",date_time

end subroutine display_datetime


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
  CALL delete(res)
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


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! e, nel caso \a this sia un vettore, la lunghezza del record e quella
!! del vettore devono essere accordate. Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE datetime_read_unit(this, unit)
TYPE(datetime),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CALL datetime_vect_read_unit((/this/), unit)

END SUBROUTINE datetime_read_unit


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! e, nel caso \a this sia un vettore, la lunghezza del record e quella
!! del vettore devono essere accordate. Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE datetime_vect_read_unit(this, unit)
TYPE(datetime) :: this(:) !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

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


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE datetime_write_unit(this, unit)
TYPE(datetime),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CALL datetime_vect_write_unit((/this/), unit)

END SUBROUTINE datetime_write_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE datetime_vect_write_unit(this, unit)
TYPE(datetime),INTENT(in) :: this(:) !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

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


! ===============
! == timedelta ==
! ===============
!> Costruisce un oggetto \a timedelta con i parametri opzionali forniti.
!! Se non viene passato nulla lo inizializza a intervallo di durata nulla.
!! L'intervallo ottenuto è pari alla somma dei valori di tutti i parametri
!! forniti, ovviamente non fornire un parametro equivale a fornirlo =0.
!! Questa è la versione \c FUNCTION, in stile F2003, del costruttore, da preferire
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
!! L'intervallo ottenuto è pari alla somma dei valori di tutti i parametri
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
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate !< inizializza l'oggetto ad un intervallo nel formato \c GGGGGGGGGG \c hh:mm:ss.msc, ignorando tutti gli altri parametri
CHARACTER(len=*),INTENT(IN),OPTIONAL :: simpledate !< inizializza l'oggetto ad un intervallo nel formato \c GGGGGGGGhhmmmsc, ignorando tutti gli altri parametri, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate !< inizializza l'oggetto ad un intervallo nel formato \c GGGGGGGGhhmm, ignorando tutti gli altri parametri

INTEGER :: d, h, m, s, ms
CHARACTER(len=23) :: datebuf

this%month = 0
IF (PRESENT(isodate)) THEN
  datebuf(1:23) = '0000000000 00:00:00.000'
  datebuf(1:MIN(LEN(isodate),23)) = isodate(1:MIN(LEN(isodate),23))
  READ(datebuf,'(I10,1X,I2,1X,I2,1X,I2,1X,I3)', err=200) d, h, m, s, ms
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
ELSE
  this%iminuti = 0
  IF (PRESENT(year)) THEN
    this%month = this%month + year*12
  ENDIF
  IF (PRESENT(month)) THEN
    this%month = this%month + month
  ENDIF
  IF (PRESENT(day)) THEN
    this%iminuti = this%iminuti + 86400000_int_ll*INT(day, KIND=int_ll)
  ENDIF
  IF (PRESENT(hour)) THEN
    this%iminuti = this%iminuti + 3600000_int_ll*INT(hour, KIND=int_ll)
  ENDIF
  IF (PRESENT(minute)) THEN
    this%iminuti = this%iminuti + 60000_int_ll*INT(minute, KIND=int_ll)
  ENDIF
  IF (PRESENT(msec)) THEN
    this%iminuti = this%iminuti + msec
  ENDIF
ENDIF

END SUBROUTINE timedelta_init


SUBROUTINE timedelta_delete(this)
TYPE(timedelta),INTENT(INOUT) :: this

this%iminuti = imiss
this%month = 0

END SUBROUTINE timedelta_delete


!> Restituisce il valore di un oggetto \a timedelta in una o più
!! modalità desiderate. Qualsiasi combinazione dei parametri
!! opzionali è consentita. \a oraclesimedate è
!! obsoleto, usare piuttosto \a simpledate.
SUBROUTINE timedelta_getval(this, year, month, amonth, day, hour, minute, msec, &
 ahour, aminute, amsec, isodate, simpledate, oraclesimdate)
TYPE(timedelta),INTENT(IN) :: this !< oggetto di cui restituire il valore
INTEGER,INTENT(OUT),OPTIONAL :: year !< anni, /=0 solo per intervalli "popolari"
INTEGER,INTENT(OUT),OPTIONAL :: month !< mesi modulo 12, /=0 solo per intervalli "popolari"
INTEGER,INTENT(OUT),OPTIONAL :: amonth !< mesi totali, /=0 solo per intervalli "popolari"
INTEGER,INTENT(OUT),OPTIONAL :: day !< giorni totali
INTEGER,INTENT(OUT),OPTIONAL :: hour !< ore modulo 24
INTEGER,INTENT(OUT),OPTIONAL :: minute !< minuti modulo 60
INTEGER,INTENT(OUT),OPTIONAL :: msec !< millisecondi modulo 1000
INTEGER,INTENT(OUT),OPTIONAL :: ahour !< ore totali
INTEGER,INTENT(OUT),OPTIONAL :: aminute !< minuti totali
INTEGER(kind=int_ll),INTENT(OUT),OPTIONAL :: amsec !< millisecondi totali
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: isodate !< intervallo totale nel formato \c GGGGGGGGGG \c hh:mm:ss.msc  (simil-ISO), la variabile può essere più corta di 23 caratteri, in tal caso conterrà solo ciò che vi cape
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: simpledate  !< intervallo totale nel formato \c GGGGGGGGhhmmssmsc , la variabile può essere più corta di 17 caratteri, in tal caso conterrà solo ciò che vi cape, da preferire rispetto a \a oraclesimdate
CHARACTER(len=12),INTENT(OUT),OPTIONAL :: oraclesimdate !< intervallo totale nel formato \c GGGGGGGGhhmm

INTEGER :: lyear, lmonth, lday, lhour, lminute, ier
CHARACTER(len=23) :: datebuf

IF (PRESENT(amsec)) THEN 
  amsec = this%iminuti
ENDIF
IF (PRESENT(aminute)) THEN 
  aminute = this%iminuti/60000_int_ll
ENDIF
IF (PRESENT(ahour)) THEN
  ahour = this%iminuti/3600000_int_ll
ENDIF
IF (PRESENT(msec)) THEN 
  msec = MOD(this%iminuti, 60000)
ENDIF
IF (PRESENT(minute)) THEN 
  minute = MOD(this%iminuti/60000_int_ll, 60)
ENDIF
IF (PRESENT(hour)) THEN
  hour = MOD(this%iminuti/3600000_int_ll, 24)
ENDIF
IF (PRESENT(day)) THEN
  day = this%iminuti/86400000_int_ll
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
   this%iminuti/86400000_int_ll, MOD(this%iminuti/3600000_int_ll, 24), ':', &
   MOD(this%iminuti/60000_int_ll, 60), ':', MOD(this%iminuti/1000_int_ll, 60), &
   '.', MOD(this%iminuti, 1000)
  isodate = datebuf(1:MIN(LEN(isodate),23))

ENDIF
IF (PRESENT(simpledate)) THEN
  WRITE(datebuf(1:17), '(I8.8,3I2.2,I3.3)') &
   this%iminuti/86400000_int_ll, MOD(this%iminuti/3600000_int_ll, 24), &
   MOD(this%iminuti/60000_int_ll, 60), MOD(this%iminuti/1000_int_ll, 60), &
   MOD(this%iminuti, 1000)
  simpledate = datebuf(1:MIN(LEN(simpledate),17))
ENDIF
IF (PRESENT(oraclesimdate)) THEN
  CALL l4f_log(L4F_WARN, 'in timedelta_getval, parametro oraclesimdate '// &
   'obsoleto, usare piuttosto simpledate')
  WRITE(oraclesimdate, '(I8.8,2I2.2)') this%iminuti/86400000_int_ll, &
   MOD(this%iminuti/3600000_int_ll, 24), MOD(this%iminuti/60000_int_ll, 60)
ENDIF

END SUBROUTINE timedelta_getval


!> Restituisce una rappresentazione carattere stampabile di un oggetto
!! \a timedelta.
FUNCTION timedelta_to_char(this) RESULT(char)
TYPE(timedelta),INTENT(IN) :: this

CHARACTER(len=23) :: char

CALL getval(this, isodate=char)

END FUNCTION timedelta_to_char


!> Restituisce il valore in millisecondi totali di un oggetto \a timedelta.
elemental FUNCTION timedelta_getamsec(this)
TYPE(timedelta),INTENT(IN) :: this !< oggetto di cui restituire il valore
INTEGER(kind=int_ll) :: timedelta_getamsec !< millisecondi totali

timedelta_getamsec = this%iminuti

END FUNCTION timedelta_getamsec


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

res%iminuti = this%iminuti*n
res%month = this%month*n

END FUNCTION timedelta_divint


FUNCTION timedelta_divtd(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
INTEGER :: res

res = this%iminuti/that%iminuti

END FUNCTION timedelta_divtd


FUNCTION timedelta_mod(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
TYPE(timedelta) :: res

res%iminuti = MOD(this%iminuti, that%iminuti)
res%month = 0

END FUNCTION timedelta_mod


FUNCTION datetime_timedelta_mod(this, that) RESULT(res)
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


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! e, nel caso \a this sia un vettore, la lunghezza del record e quella
!! del vettore devono essere accordate. Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE timedelta_read_unit(this, unit)
TYPE(timedelta),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CALL timedelta_vect_read_unit((/this/), unit)

END SUBROUTINE timedelta_read_unit


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! e, nel caso \a this sia un vettore, la lunghezza del record e quella
!! del vettore devono essere accordate. Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE timedelta_vect_read_unit(this, unit)
TYPE(timedelta) :: this(:) !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

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


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE timedelta_write_unit(this, unit)
TYPE(timedelta),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CALL timedelta_vect_write_unit((/this/), unit)

END SUBROUTINE timedelta_write_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE timedelta_vect_write_unit(this, unit)
TYPE(timedelta),INTENT(in) :: this(:) !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

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


SUBROUTINE jeladata5(iday,imonth,iyear,ihour,imin,iminuti)

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

INTEGER :: iday, imonth, iyear, ihour, imin, iminuti

iminuti = ndays(iday,imonth,iyear)*1440+(ihour*60)+imin

END SUBROUTINE jeladata5


SUBROUTINE jeladata5_1(iday,imonth,iyear,ihour,imin,imsec,imillisec)
INTEGER :: iday, imonth, iyear, ihour, imin, imsec
INTEGER(KIND=int_ll) :: imillisec

imillisec = INT(ndays(iday,imonth,iyear)*1440+(ihour*60)+imin, KIND=int_ll)*60000 &
 + imsec

END SUBROUTINE jeladata5_1


SUBROUTINE seconds_until_date(year, month, day, hour, minute, second, asecond)
INTEGER, INTENT(in) :: year, month, day, hour, minute, second
INTEGER(kind=dateint), INTENT(out) :: asecond

asecond = ndays(day,month,year)*sec_in_day + hour*sec_in_hour + &
 minute*sec_in_min + second

END SUBROUTINE seconds_until_date


SUBROUTINE date_until_seconds(asecond, year, month, day, hour, minute, second)
INTEGER(kind=dateint), INTENT(in) :: asecond
INTEGER, INTENT(out) :: year, month, day, hour, minute, second

INTEGER :: aday

second = MOD(asecond, sec_in_min)
minute = MOD(asecond, sec_in_hour)/sec_in_min
hour = MOD(asecond, sec_in_day)/sec_in_hour
aday = asecond/sec_in_day
! IF (MOD(iminuti,1440) < 0) igiorno = igiorno-1 !?
CALL ndyin(aday, day, month, year)

END SUBROUTINE date_until_seconds


SUBROUTINE jeladata6(iday, imonth, iyear, ihour, imin, iminuti)

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


INTEGER :: iday, imonth, iyear, ihour, imin, iminuti, igiorno

imin = MOD(iminuti,60)
ihour = MOD(iminuti,1440)/60
igiorno = iminuti/1440
IF (MOD(iminuti,1440) < 0) igiorno = igiorno-1
CALL ndyin(igiorno,iday,imonth,iyear)

END SUBROUTINE jeladata6


SUBROUTINE jeladata6_1(iday, imonth, iyear, ihour, imin, imsec, imillisec)
INTEGER(KIND=int_ll), INTENT(IN) :: imillisec
INTEGER, INTENT(OUT) :: iday, imonth, iyear, ihour, imin, imsec

INTEGER :: igiorno

imsec = MOD(imillisec, 60000_int_ll) ! partial msec
!imin = MOD(imillisec/60000_int_ll, 60)
!ihour = MOD(imillisec/3600000_int_ll, 24)
imin = MOD(imillisec, 3600000_int_ll)/60000_int_ll
ihour = MOD(imillisec, 86400000_int_ll)/3600000_int_ll
igiorno = imillisec/86400000_int_ll
!IF (MOD(imillisec,1440) < 0) igiorno = igiorno-1 !?!?!?
CALL ndyin(igiorno,iday,imonth,iyear)

END SUBROUTINE jeladata6_1


SUBROUTINE ndyin(ndays,igg,imm,iaa)

!!OMSTART NDYIN
!     SUBROUTINE NDYIN(NDAYS,IGG,IMM,IAA)
!     restituisce la data fornendo in input il numero di
!     giorni dal 1/1/1
!
!!omend

INTEGER :: ndays, igg, imm, iaa, n

n = ndays/d400
ndays = ndays - n*d400
iaa = year0 + n*400
n = MIN(ndays/d100, 3)
ndays = ndays - n*d100
iaa = iaa + n*100
n = ndays/d4
ndays = ndays - n*d4
iaa = iaa + n*4
n = MIN(ndays/d1, 3)
ndays = ndays - n*d1
iaa = iaa + n
n = bisextilis(iaa)
DO imm = 1, 12
  IF (ndays < ianno(imm+1,n)) EXIT
ENDDO
igg = ndays+1-ianno(imm,n) ! +1 perche' il mese parte da 1

END SUBROUTINE ndyin


FUNCTION ndays(igg,imm,iaa)

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

INTEGER :: ndays, igg, imm, iaa

INTEGER :: lmonth, lyear

! Limito il mese a [1-12] e correggo l'anno coerentemente
lmonth = MODULO(imm-1, 12) + 1 ! uso MODULO e non MOD per gestire bene i valori <0
lyear = iaa + (imm - lmonth)/12
ndays = igg+ianno(lmonth, bisextilis(lyear))
ndays = ndays-1 + 365*(lyear-year0) + (lyear-year0)/4 - (lyear-year0)/100 + &
 (lyear-year0)/400

END FUNCTION ndays


FUNCTION bisextilis(annum)
INTEGER,INTENT(in) :: annum
INTEGER :: bisextilis

IF (MOD(annum,4) == 0 .AND. (MOD(annum,400) == 0 .EQV. MOD(annum,100) == 0)) THEN
  bisextilis = 2
ELSE
 bisextilis = 1
ENDIF
END FUNCTION bisextilis

END MODULE datetime_class

