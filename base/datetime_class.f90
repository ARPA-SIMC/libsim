MODULE datetime_class
USE kinds
USE err_handling
USE missing_values
IMPLICIT NONE

TYPE datetime
  PRIVATE
  INTEGER :: iminuti
END TYPE  datetime

TYPE timedelta
  PRIVATE
  INTEGER :: iminuti, month, year
END TYPE timedelta


TYPE(datetime), PARAMETER :: datetime_miss=datetime(imiss)
TYPE(timedelta), PARAMETER :: timedelta_miss=timedelta(imiss, 0, 0)
TYPE(timedelta), PARAMETER :: timedelta_0=timedelta(0, 0, 0)

INTEGER,PARAMETER :: &
 year0=1, & ! anno di origine per iminuti
 d1=365, & ! giorni/1 anno nel calendario gregoriano
 d4=d1*4+1, & ! giorni/4 anni nel calendario gregoriano
 d100=d1*100+25-1, & ! giorni/100 anni nel calendario gregoriano
 d400=d1*400+100-3, & ! giorni/400 anni nel calendario gregoriano
 unmim=1035593280, & ! differenza tra 01/01/1970 e 01/01/0001 (per unixtime)
 ianno(13,2)=RESHAPE((/ &
 0,31,59,90,120,151,181,212,243,273,304,334,365, &
 0,31,60,91,121,152,182,213,244,274,305,335,366/),(/13,2/))

PRIVATE
PUBLIC datetime, datetime_miss, init, delete, getval, &
 datetime_eq, datetime_eqsv, datetime_ne, datetime_nesv, &
 datetime_gt, datetime_gtsv, datetime_lt, datetime_ltsv, &
 datetime_ge, datetime_gesv, datetime_le, datetime_lesv, &
 datetime_add, datetime_subdt, datetime_subtd, &
 timedelta, timedelta_miss, timedelta_0, &
 timedelta_eq, timedelta_eqsv, timedelta_ne, timedelta_nesv, &
 timedelta_gt, timedelta_gtsv, timedelta_lt, timedelta_ltsv, &
 timedelta_ge, timedelta_gesv, timedelta_le, timedelta_lesv, &
 timedelta_add, timedelta_sub, timedelta_mod, mod

INTERFACE init
  MODULE PROCEDURE datetime_init, timedelta_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE datetime_delete, timedelta_delete
END INTERFACE

INTERFACE getval
  MODULE PROCEDURE datetime_getval, timedelta_getval
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE datetime_eq, datetime_eqsv, timedelta_eq, timedelta_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE datetime_ne, datetime_nesv, timedelta_ne, timedelta_nesv
END INTERFACE

INTERFACE OPERATOR (>)
  MODULE PROCEDURE datetime_gt, datetime_gtsv, timedelta_gt, timedelta_gtsv
END INTERFACE

INTERFACE OPERATOR (<)
  MODULE PROCEDURE datetime_lt, datetime_ltsv, timedelta_lt, timedelta_ltsv
END INTERFACE

INTERFACE OPERATOR (>=)
  MODULE PROCEDURE datetime_ge, datetime_gesv, timedelta_ge, timedelta_gesv
END INTERFACE

INTERFACE OPERATOR (<=)
  MODULE PROCEDURE datetime_le, datetime_lesv, timedelta_le, timedelta_lesv
END INTERFACE

INTERFACE OPERATOR (+)
  MODULE PROCEDURE datetime_add, timedelta_add
END INTERFACE

INTERFACE OPERATOR (-)
  MODULE PROCEDURE datetime_subdt, datetime_subtd, timedelta_sub
END INTERFACE

INTERFACE OPERATOR (*)
  MODULE PROCEDURE timedelta_mult, timedelta_tlum
END INTERFACE

INTERFACE OPERATOR (/)
  MODULE PROCEDURE timedelta_divint, timedelta_divtd
END INTERFACE

INTERFACE mod
  MODULE PROCEDURE timedelta_mod
END INTERFACE

CONTAINS

! ==============
! == datetime ==
! ==============
SUBROUTINE datetime_init(this, iminuti, year, month, day, hour, minute, &
 unixtime, isodate, oraclesimdate)
TYPE(datetime),INTENT(INOUT) :: this
INTEGER,INTENT(IN),OPTIONAL :: iminuti, year, month, day, hour, minute
INTEGER(kind=int_ll),INTENT(IN),OPTIONAL ::  unixtime
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate

INTEGER :: lyear, lmonth, lday, lhour, lminute, ier

IF (PRESENT(iminuti)) THEN ! minuti dal 01/01/0001 (libmega)
  this%iminuti = iminuti
ELSE IF (PRESENT(year)) THEN ! anno/mese/giorno, ecc.
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
  CALL jeladata5(lday, lmonth, lyear, lhour, lminute, this%iminuti)
ELSE IF (PRESENT(isodate)) THEN ! formato iso YYYY-MM-DD hh:mm
  READ(isodate,'(I4,1X,I2,1X,I2,1X,I2,1X,I2)', iostat=ier) &
   lyear, lmonth, lday, lhour, lminute
  IF (ier /= 0) THEN
    CALL delete(this)
    CALL raise_error('isodate '//TRIM(isodate)//' non valida')
    RETURN
  ENDIF
  CALL jeladata5(lday,lmonth,lyear,lhour,lminute,this%iminuti)
ELSE IF (PRESENT(oraclesimdate)) THEN ! formato YYYYMMDDhhmm
  READ(oraclesimdate,'(I4,4I2)', iostat=ier) lyear, lmonth, lday, lhour, lminute
  IF (ier /= 0) THEN
    CALL delete(this)
    CALL raise_error('oraclesimdate '//TRIM(oraclesimdate)//' non valida')
    RETURN
  ENDIF
  CALL jeladata5(lday,lmonth,lyear,lhour,lminute,this%iminuti)
ELSE IF (PRESENT(unixtime)) THEN ! secondi dal 01/01/1970 (unix)
  this%iminuti = unixtime/60_int_ll + unmim
ENDIF

END SUBROUTINE datetime_init


SUBROUTINE datetime_delete(this)
TYPE(datetime),INTENT(INOUT) :: this

this%iminuti = imiss

END SUBROUTINE datetime_delete


SUBROUTINE datetime_getval(this, iminuti, year, month, day, hour, minute, &
 unixtime, isodate, oraclesimdate)
TYPE(datetime),INTENT(IN) :: this
INTEGER,INTENT(OUT),OPTIONAL :: iminuti, year, month, day, hour, minute
INTEGER(kind=int_ll),INTENT(OUT),OPTIONAL ::  unixtime
CHARACTER(len=16),INTENT(OUT),OPTIONAL :: isodate
CHARACTER(len=12),INTENT(OUT),OPTIONAL :: oraclesimdate

INTEGER :: lyear, lmonth, lday, lhour, lminute, ier

IF (PRESENT(iminuti)) THEN
  iminuti = this%iminuti
ENDIF
IF (PRESENT(year) .OR. PRESENT(month) .OR. PRESENT(day) .OR. PRESENT(hour) &
 .OR. PRESENT(minute) .OR. PRESENT(unixtime) .OR. PRESENT(isodate) &
 .OR. PRESENT(oraclesimdate)) THEN
  CALL jeladata6(lday, lmonth, lyear, lhour, lminute, this%iminuti)
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
    WRITE(isodate, '(I4.4,A1,I2.2,A1,I2.2,1X,I2.2,A1,I2.2)') &
     lyear, '-', lmonth, '-', lday, lhour, ':', lminute
  ENDIF
  IF (PRESENT(oraclesimdate)) THEN
    WRITE(oraclesimdate, '(I4.4,4I2.2)') lyear, lmonth, lday, lhour, lminute
  ENDIF
  IF (PRESENT(unixtime)) THEN
    unixtime = (this%iminuti-unmim)*60_int_ll
  ENDIF
ENDIF

END SUBROUTINE datetime_getval


elemental FUNCTION datetime_eq(this, that) RESULT(res)
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

INTEGER :: lyear, lmonth, lday, lhour, lminute

IF (this == datetime_miss .OR. that == timedelta_miss) THEN
  CALL delete(res)
ELSE
  IF (that%month == 0 .AND. that%year == 0) THEN
    CALL init(res, iminuti=this%iminuti+that%iminuti)
  ELSE
    CALL init(res, iminuti=this%iminuti+that%iminuti)
    CALL getval(res, year=lyear, month=lmonth, day=lday, hour=lhour, &
     minute=lminute)
    CALL init(res, year=lyear+that%year, month=lmonth+that%month, day=lday, &
     hour=lhour, minute=lminute)
  ENDIF
ENDIF

END FUNCTION datetime_add


FUNCTION datetime_subdt(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
TYPE(timedelta) :: res

IF (this == datetime_miss .OR. that == datetime_miss) THEN
  CALL delete(res)
ELSE
  CALL init(res, minute=this%iminuti-that%iminuti)
ENDIF

END FUNCTION datetime_subdt


FUNCTION datetime_subtd(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this
TYPE(timedelta),INTENT(IN) :: that
TYPE(datetime) :: res

INTEGER :: lyear, lmonth, lday, lhour, lminute

IF (this == datetime_miss .OR. that == timedelta_miss) THEN
  CALL delete(res)
ELSE
  IF (that%month == 0 .AND. that%year == 0) THEN
    CALL init(res, iminuti=this%iminuti-that%iminuti)
  ELSE
    CALL init(res, iminuti=this%iminuti-that%iminuti)
    CALL getval(res, year=lyear, month=lmonth, day=lday, hour=lhour, &
     minute=lminute)
    CALL init(res, year=lyear-that%year, month=lmonth-that%month, day=lday, &
     hour=lhour, minute=lminute)
  ENDIF
ENDIF

END FUNCTION datetime_subtd


! ===============
! == timedelta ==
! ===============
SUBROUTINE timedelta_init(this, year, month, day, hour, minute)
TYPE(timedelta),INTENT(INOUT) :: this
INTEGER,INTENT(IN),OPTIONAL :: year, month, day, hour, minute

this%iminuti = 0
IF (PRESENT(year)) THEN
  this%year = year
ELSE
  this%year = 0
ENDIF
IF (PRESENT(month)) THEN
  this%month = month
ELSE
  this%month = 0
ENDIF
IF (PRESENT(day)) THEN
  this%iminuti = this%iminuti + 1440*day
ENDIF
IF (PRESENT(hour)) THEN
  this%iminuti = this%iminuti + 60*hour
ENDIF
IF (PRESENT(minute)) THEN
  this%iminuti = this%iminuti + minute
ENDIF

END SUBROUTINE timedelta_init


SUBROUTINE timedelta_delete(this)
TYPE(timedelta),INTENT(INOUT) :: this

this%iminuti = imiss
this%year = 0
this%month = 0

END SUBROUTINE timedelta_delete


SUBROUTINE timedelta_getval(this, year, month, day, hour, minute, &
 ahour, aminute, isodate, oraclesimdate)
TYPE(timedelta),INTENT(IN) :: this
INTEGER,INTENT(OUT),OPTIONAL :: year, month, day, hour, minute, &
 ahour, aminute
CHARACTER(len=16),INTENT(OUT),OPTIONAL :: isodate
CHARACTER(len=12),INTENT(OUT),OPTIONAL :: oraclesimdate

INTEGER :: lyear, lmonth, lday, lhour, lminute, ier

IF (PRESENT(aminute)) THEN 
  aminute = this%iminuti
ENDIF
IF (PRESENT(ahour)) THEN
  ahour = this%iminuti/60
ENDIF
IF (PRESENT(minute)) THEN 
  minute = MOD(this%iminuti,60)
ENDIF
IF (PRESENT(hour)) THEN
  hour = MOD(this%iminuti,1440)/60
ENDIF
IF (PRESENT(day)) THEN
  day = this%iminuti/1440
ENDIF
IF (PRESENT(month)) THEN
  month = this%month
ENDIF
IF (PRESENT(year)) THEN
  year = this%year
ENDIF
IF (PRESENT(isodate)) THEN ! Non standard, inventato!
  WRITE(isodate, '(I10.10,1X,I2.2,A1,I2.2)') this%iminuti/1440, &
   MOD(this%iminuti,1440)/60, ':', MOD(this%iminuti,60)
ENDIF
IF (PRESENT(oraclesimdate)) THEN
  WRITE(oraclesimdate, '(I8.8,2I2.2)') this%iminuti/1440, &
   MOD(this%iminuti,1440)/60, MOD(this%iminuti,60)
ENDIF

END SUBROUTINE timedelta_getval


elemental FUNCTION timedelta_eq(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
LOGICAL :: res

res = (this%iminuti == that%iminuti &
 .AND. this%month == that%month .AND. this%year == that%year)

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

CALL init(res, minute=this%iminuti+that%iminuti, &
 month=this%month+that%month, year=this%year+that%year)

END FUNCTION timedelta_add


FUNCTION timedelta_sub(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
TYPE(timedelta) :: res

CALL init(res, minute=this%iminuti-that%iminuti, &
 month=this%month-that%month, year=this%year-that%year)

END FUNCTION timedelta_sub


FUNCTION timedelta_mult(this, n) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this
INTEGER,INTENT(IN) :: n
TYPE(timedelta) :: res

CALL init(res, minute=this%iminuti*n, month=this%month*n, year=this%year*n)

END FUNCTION timedelta_mult


FUNCTION timedelta_tlum(n, this) RESULT(res)
INTEGER,INTENT(IN) :: n
TYPE(timedelta),INTENT(IN) :: this
TYPE(timedelta) :: res

CALL init(res, minute=this%iminuti*n, month=this%month*n, year=this%year*n)

END FUNCTION timedelta_tlum


FUNCTION timedelta_divint(this, n) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this
INTEGER,INTENT(IN) :: n
TYPE(timedelta) :: res

CALL init(res, minute=this%iminuti/n, month=this%month/n, year=this%year/n)

END FUNCTION timedelta_divint


FUNCTION timedelta_divtd(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
INTEGER :: res

res = this%iminuti/that%iminuti

END FUNCTION timedelta_divtd


FUNCTION timedelta_mod(this, that) RESULT(res)
TYPE(timedelta),INTENT(IN) :: this, that
TYPE(timedelta) :: res

CALL init(res, minute=MOD(this%iminuti, that%iminuti))

END FUNCTION timedelta_mod



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
lmonth = MOD(MOD(imm-1, 12)+12, 12) + 1 ! Problema con mod(1,12) per i < 0
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

