PROGRAM esempio_file_utilities
USE file_utilities
USE err_handling
IMPLICIT NONE

INTEGER :: un, iost, clen, nrec, nfield
CHARACTER(LEN=1024) :: line
CHARACTER(LEN=16) :: field
TYPE(csv_record) :: csv
INTEGER :: i1, i2
REAL :: r1, r2

CALL eh_setval(fatal=.FALSE.)
un=getunit() ! ottengo un'unita` libera

OPEN(un, FILE='tmp.csv') ! creo il file csv al volo
WRITE(un,'(A)')'simple, 10,20, ciao bello'
WRITE(un,'(A)')'more complex, " 10",, "ciao, ""bello"""'
WRITE(un,'(A)')'wrong behavior , " 10" ,20 , "ciao, ""bello""'
WRITE(un,'(A)')'end of wrong behavior'
WRITE(un,'(A)')'20, 10.5, 9.81, non numeric'
CLOSE(un)

OPEN(un, FILE='tmp.csv') ! lo riapro
nrec = 0
DO WHILE(.TRUE.) ! ciclo sui record
  READ(un,iostat=iost,'(A)') line
  IF (iost /= 0) EXIT
  nrec = nrec + 1

  CALL init(csv, line) ! inizializzo l'oggetto csv_record col record letto
  PRINT'(''record'',I3,'': |'',A,''|'')',nrec,TRIM(line)
  IF (nrec < 5) THEN
    nfield = 0
    DO WHILE(.NOT.csv_record_end(csv)) ! ciclo sui campi
      nfield = nfield  + 1
      CALL csv_record_getfield(csv, field, clen) ! ottengo un campo
      PRINT'(''field:'',I3,'' length: '',I4,'' contents: |'',A,''|'')', &
       nfield,clen,field(:MIN(clen,LEN(field)))
    ENDDO
  ELSE
    CALL csv_record_getfield(csv, i1)
    CALL csv_record_getfield(csv, i2)
    CALL csv_record_getfield(csv, r1)
    CALL csv_record_getfield(csv, r2)
    PRINT*,i1,i2,r1,r2
  ENDIF
  CALL delete(csv) ! non dimenticare di distruggerlo altrimenti perdo memoria
ENDDO

CLOSE(un)

END PROGRAM esempio_file_utilities
