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

PRIVATE jeladata5, ndays

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

CONTAINS

SUBROUTINE datetime_init(this, iminuti, year, month, day, hour, minute, &
 unixtime, isodate, oraclesimdate)
TYPE(datetime),INTENT(INOUT) :: this
INTEGER,INTENT(IN),OPTIONAL :: iminuti, year, month, day, hour, minute
INTEGER(kind=int_ll),INTENT(IN),OPTIONAL ::  unixtime
CHARACTER(len=*),INTENT(IN),OPTIONAL :: isodate
CHARACTER(len=12),INTENT(IN),OPTIONAL :: oraclesimdate

INTEGER :: lyear, lmonth, lday, lhour, lminute, ier

IF (PRESENT(iminuti)) THEN ! minuti dal 01/01/1900 (libmega)
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
  READ(isodate,'(I4,4I2)', iostat=ier) lyear, lmonth, lday, lhour, lminute
  IF (ier /= 0) THEN
    CALL delete(this)
    CALL raise_error('oraclesimdate '//TRIM(oraclesimdate)//' non valida')
    RETURN
  ENDIF
  CALL jeladata5(lday,lmonth,lyear,lhour,lminute,this%iminuti)
  this%interval = .FALSE.
ELSE IF (PRESENT(unixtime)) THEN ! secondi dal 01/01/1970 (unix)
  CALL jeladata5(1, 1, 1970, 0, 0, this%iminuti)
  this%iminuti = this%iminuti + unixtime/60_int_ll
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
    lhour = MOD((this%iminuti-lminute)/60,24)
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
      WRITE(oraclesimdate, '(I4,4I2)') lyear, lmonth, lday, lhour, lminute
    ENDIF
!    unixtime = this%iminuti*60_int_ll
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

IF (.NOT. that%interval) THEN
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

iminuti = ndays(iday,imonth,iyear)*24*60+(ihour*60)+imin

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
ihour = MOD(iminuti,(60*24))/60
!     iminu=iminuti-998779680
!     iminu=iminuti
igiorno=iminuti/(60*24)
IF (MOD(iminuti,(60*24)) < 0) igiorno=igiorno-1

CALL ndyin(igiorno,iday,imonth,iyear)

END SUBROUTINE jeladata6


SUBROUTINE ndyin(ndays,igg,imm,iaa)

!OMSTART NDYIN
!     SUBROUTINE NDYIN(NDAYS,IGG,IMM,IAA)
!     restituisce la data fornendo in input il numero di
!     giorni dal 1/1/1
!     ATTENZIONE
!     non tiene conto del riaggiustamento dell'anno non
!     bisestile ogni 100 anni
!
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     nota bene                   E' SICURO !!!
!     un anno e' bisestile se divisibile per 4
!     un anno rimane bisestile se divisibile per 400
!     un anno NON e' bisestile se divisibile per 100
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!     INPUT:
!     NDAYS     I*4     numero giorni dal 1/1/1
!
!     OUTPUT:
!     IGG       I*4     giorno
!     IMM       I*4     mese
!     IAA       I*4     anno
!omend

INTEGER :: ndays, igg, imm, iaa, ndayy, nday, ires, j
INTEGER,PARAMETER :: &
 ianno(13)=(/0,31,59,90,120,151,181,212,243,273,304,334,365/), &
 ianno_b(13)=(/0,31,60,91,121,152,182,213,244,274,305,335,366/)

ndayy=MOD(ndays,(366+365*3)) !    resto di gruppi di 4 anni
iaa=(ndays/(366+365*3))*4+(ndayy/365)+1
IF (ndayy/365 == 4) iaa=iaa-1
nday=MOD(ndayy,365)+1
IF (ndayy/365 == 4) nday=nday+365

ires=MOD(iaa,4)
IF (ires == 0) THEN
  DO j=1,12
    IF (nday > ianno_b(j) .AND. nday <= ianno_b(j+1)) imm=j
  ENDDO
  igg=nday-ianno_b(imm)
ELSE
  DO j=1,12
    IF (nday > ianno(j) .AND. nday <= ianno(j+1)) imm=j
  ENDDO
  igg=nday-ianno(imm)
ENDIF

END SUBROUTINE ndyin


FUNCTION ndays(igg,imm,iaa)

!OMSTART NDAYS
!     FUNCTION NDAYS(IGG,IMM,IAA)
!     restituisce  il numero di giorni dal 1/1/1
!     fornendo in input la data
!     ATTENZIONE
!     non tiene conto del riaggiustamento dell'anno non
!     bisestile ogni 100 anni
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     nota bene                   E' SICURO !!!
!     un anno e' bisestile se divisibile per 4
!     un anno rimane bisestile se divisibile per 400
!     un anno NON e' bisestile se divisibile per 100
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!     INPUT:
!     IGG       I*4     giorno
!     IMM       I*4     mese
!     IAA       I*4     anno
!
!     OUTPUT:
!     NDAYS     I*4     numero giorni dal 1/1/1
!omend

INTEGER :: ndays, igg, imm, iaa, ibis, nday
INTEGER,PARAMETER :: ianno(12) =(/0,31,59,90,120,151,181,212,243,273,304,334/), &
 ianno_b(12)=(/0,31,60,91,121,152,182,213,244,274,305,335/)

nday = 0
ibis = MOD(iaa,4)
IF (ibis.EQ.0) THEN
  nday=igg+ianno_b(imm)
ELSE
  nday=igg+ianno(imm)
ENDIF

ndays=(nday-1)+(365*(iaa-1))+((iaa-1)/4) ! -((iaa-1)/100) vero ogni 100

END FUNCTION ndays


END MODULE datetime_class


