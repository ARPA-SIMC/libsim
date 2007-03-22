MODULE datetime_class
USE kinds
USE err_handling
USE missing_values
IMPLICIT NONE

TYPE datetime
  PRIVATE
  INTEGER :: iminuti
  LOGICAL :: interval
END TYPE  datetime

TYPE(datetime), PARAMETER :: datetime_miss=datetime(imiss, .FALSE.)
INTEGER,PARAMETER :: &
 year0=1, & ! anno di origine per iminuti
 d400=365*400+366*100-3, & ! giorni/400 anni nel calendario gregoriano
 d100=365*100+366*25-1, & ! giorni/100 anni nel calendario gregoriano
 d4=365*4+366, & ! giorni/4 anni nel calendario gregoriano
 unmim=1035593280, & ! differenza tra 01/01/1970 e 01/01/0001 (per unixtime)
 ianno(13,2)=RESHAPE((/ &
 0,31,59,90,120,151,181,212,243,273,304,334,365, &
 0,31,60,91,121,152,182,213,244,274,305,335,366/),(/13,2/))

PRIVATE
PUBLIC datetime, datetime_miss, init, delete, getval, &
 datetime_eq, datetime_eqsv, datetime_ne, datetime_nesv, &
 datetime_gt, datetime_gtsv, datetime_lt, datetime_ltsv, &
 datetime_ge, datetime_gesv, datetime_le, datetime_lesv, &
 datetime_add, datetime_sub

INTERFACE init
  MODULE PROCEDURE datetime_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE datetime_delete
END INTERFACE

INTERFACE getval
  MODULE PROCEDURE datetime_getval
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE datetime_eq, datetime_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE datetime_ne, datetime_nesv
END INTERFACE

INTERFACE OPERATOR (>)
  MODULE PROCEDURE datetime_gt, datetime_gtsv
END INTERFACE

INTERFACE OPERATOR (<)
  MODULE PROCEDURE datetime_lt, datetime_ltsv
END INTERFACE

INTERFACE OPERATOR (>=)
  MODULE PROCEDURE datetime_ge, datetime_gesv
END INTERFACE

INTERFACE OPERATOR (<=)
  MODULE PROCEDURE datetime_le, datetime_lesv
END INTERFACE

INTERFACE OPERATOR (+)
  MODULE PROCEDURE datetime_add
END INTERFACE

INTERFACE OPERATOR (-)
  MODULE PROCEDURE datetime_sub
END INTERFACE

CONTAINS

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
  this%interval = .TRUE.
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
  this%interval = .FALSE.
  CALL jeladata5(lday, lmonth, lyear, lhour, lminute, this%iminuti)
ELSE IF (PRESENT(day) .OR. PRESENT(hour) .OR. PRESENT(minute)) THEN ! intervallo
  this%iminuti = 0
  IF (PRESENT(day)) THEN
    this%iminuti = this%iminuti + 1440
  ENDIF
  IF (PRESENT(hour)) THEN
    this%iminuti = this%iminuti + 60*hour
  ENDIF
  IF (PRESENT(minute)) THEN
    this%iminuti = this%iminuti + minute
  ENDIF
ELSE IF (PRESENT(isodate)) THEN ! formato iso YYYY-MM-DD hh:mm
  READ(isodate,'(I4,1X,I2,1X,I2,1X,I2,1X,I2)', iostat=ier) &
   lyear, lmonth, lday, lhour, lminute
  IF (ier /= 0) THEN
    CALL delete(this)
    CALL raise_error('isodate '//TRIM(isodate)//' non valida')
    RETURN
  ENDIF
  CALL jeladata5(lday,lmonth,lyear,lhour,lminute,this%iminuti)
  this%interval = .FALSE.
ELSE IF (PRESENT(oraclesimdate)) THEN ! formato YYYYMMDDhhmm
  READ(oraclesimdate,'(I4,4I2)', iostat=ier) lyear, lmonth, lday, lhour, lminute
  IF (ier /= 0) THEN
    CALL delete(this)
    CALL raise_error('oraclesimdate '//TRIM(oraclesimdate)//' non valida')
    RETURN
  ENDIF
  CALL jeladata5(lday,lmonth,lyear,lhour,lminute,this%iminuti)
  this%interval = .FALSE.
ELSE IF (PRESENT(unixtime)) THEN ! secondi dal 01/01/1970 (unix)
  this%iminuti = unixtime/60_int_ll + unmim
  this%interval = .FALSE.
ENDIF

END SUBROUTINE datetime_init


SUBROUTINE datetime_delete(this)
TYPE(datetime),INTENT(INOUT) :: this

this%iminuti = imiss
this%interval = .FALSE.

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
  IF (this%interval) THEN
    lminute = MOD(this%iminuti,60)
    lhour = MOD(this%iminuti,1440)/60
    lday = this%iminuti/1440
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
      month = 0
    ENDIF
    IF (PRESENT(year)) THEN
      year = 0
    ENDIF
    IF (PRESENT(isodate)) THEN ! Non standard, inventato!
      WRITE(isodate, '(I10.10,1X,I2.2,A1,I2.2)') lday, lhour, ':', lminute
    ENDIF
    IF (PRESENT(oraclesimdate)) THEN
      WRITE(oraclesimdate, '(I8.8,2I2.2)') lday, lhour, lminute
    ENDIF
    IF (PRESENT(unixtime)) THEN
      unixtime = this%iminuti*60_int_ll
    ENDIF
  ELSE 
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
ENDIF

END SUBROUTINE datetime_getval


elemental FUNCTION datetime_eq(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%iminuti == that%iminuti .AND. (this%interval .EQV. that%interval)) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

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

IF ((this%iminuti > that%iminuti .AND. (this%interval .EQV. that%interval)) .OR. &
 (.NOT.this%interval .AND. that%interval)) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

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

IF ((this%iminuti < that%iminuti .AND. this%interval .EQV. that%interval) .OR. &
 (this%interval .AND. .NOT.that%interval)) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

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
TYPE(datetime),INTENT(IN) :: this, that
TYPE(datetime) :: res

IF (.NOT.that%interval) THEN
  CALL raise_error('tentativo di sommare 2 datetime assoluti')
  CALL delete(res)
ELSE
  IF (this == datetime_miss .OR. that == datetime_miss) THEN
    CALL delete(res)
  ELSE
    CALL init(res, iminuti=this%iminuti+that%iminuti)
    res%interval = this%interval .AND. that%interval
  ENDIF
ENDIF

END FUNCTION datetime_add


FUNCTION datetime_sub(this, that) RESULT(res)
TYPE(datetime),INTENT(IN) :: this, that
TYPE(datetime) :: res

IF (this%interval .AND. .NOT.that%interval) THEN
  CALL raise_error('tentativo di sottrarre un datetime assoluto da uno relativo')
  CALL delete(res)
ELSE
  IF (this == datetime_miss .OR. that == datetime_miss) THEN
    CALL delete(res)
  ELSE
    CALL init(res, iminuti=this%iminuti-that%iminuti)
    res%interval = this%interval .EQV. that%interval
  ENDIF
ENDIF

END FUNCTION datetime_sub


SUBROUTINE datetime_print(this)
TYPE(datetime),INTENT(IN) :: this

INTEGER :: lday, lmonth, lyear, lhour, lminute

PRINT*,this%iminuti
CALL jeladata6(lday, lmonth, lyear, lhour, lminute, this%iminuti)
PRINT*,lday, lmonth, lyear, lhour, lminute

END SUBROUTINE datetime_print


SUBROUTINE jeladata5(iday,imonth,iyear,ihour,imin,iminuti)

!omstart JELADATA5
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
!OMEND

!     IMINUTI=NDAYS(IDAY,IMONTH,IYEAR)*24*60+(IHOUR*60)+IMIN+
!     1 998779680

INTEGER :: iday, imonth, iyear, ihour, imin, iminuti

iminuti = ndays(iday,imonth,iyear)*1440+(ihour*60)+imin

END SUBROUTINE jeladata5


SUBROUTINE jeladata6(iday, imonth, iyear, ihour, imin, iminuti)

!omstart JELADATA6
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
!OMEND


INTEGER :: iday, imonth, iyear, ihour, imin, iminuti, igiorno

!     IGIORNO=IMINUTI/(60*24)
!     IHOUR=(IMINUTI-IGIORNO*(60*24))/60
!     IMIN=IMINUTI-IGIORNO*(60*24)-IHOUR*60

imin = MOD(iminuti,60)
ihour = MOD(iminuti,1440)/60
igiorno = iminuti/1440
IF (MOD(iminuti,1440) < 0) igiorno = igiorno-1

CALL ndyin(igiorno,iday,imonth,iyear)

END SUBROUTINE jeladata6


SUBROUTINE ndyin(ndays,igg,imm,iaa)

!OMSTART NDYIN
!     SUBROUTINE NDYIN(NDAYS,IGG,IMM,IAA)
!     restituisce la data fornendo in input il numero di
!     giorni dal 1/1/1
!
!omend

INTEGER :: ndays, igg, imm, iaa, n

n = MOD(ndays, d400)
ndays = ndays - n*d400
iaa = n*400
n = MOD(ndays, d100)
ndays = ndays - n*d100
iaa = iaa + n*100
n = MOD(ndays, d4)
ndays = ndays - n*d4
iaa = iaa + n*4
n = bisextilis(iaa)
DO imm = 1, 12
  IF (ndays <= ianno(imm+1,n)) EXIT
ENDDO
igg = ndays-ianno(imm,n)

END SUBROUTINE ndyin


FUNCTION ndays(igg,imm,iaa)

!OMSTART NDAYS
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
!omend

INTEGER :: ndays, igg, imm, iaa


ndays = igg+ianno(imm,bisextilis(iaa))
ndays = ndays-1 + 365*(iaa-year0) + (iaa-year0)/4 - (iaa-year0)/100 + &
 (iaa-year0)/400

END FUNCTION ndays


FUNCTION bisextilis(annum)
INTEGER,intent(in) :: annum
INTEGER :: bisextilis

IF (MOD(annum,4) == 0 .AND. (MOD(annum,400) == 0 .EQV. MOD(annum,100) == 0)) THEN
  bisextilis = 2
ELSE
 bisextilis = 1
ENDIF
END FUNCTION bisextilis

END MODULE datetime_class


