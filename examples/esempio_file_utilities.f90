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
CALL init(csv)
CALL csv_record_addfield(csv, 'simple')
CALL csv_record_addfield(csv, ' 10')
CALL csv_record_addfield(csv, '20')
CALL csv_record_addfield(csv, ' ciao bello')
WRITE(un, '(A)') TRIM(csv_record_getrecord(csv))
CALL delete(csv)

CALL init(csv)
CALL csv_record_addfield(csv, 'more complex ')
CALL csv_record_addfield(csv, '" 10"')
CALL csv_record_addfield(csv, '')
CALL csv_record_addfield(csv, ' ciao, "bello"')
WRITE(un, '(A)') TRIM(csv_record_getrecord(csv))
CALL delete(csv)

WRITE(un,'(A)')'wrong behavior , " 10" ,20 , "ciao, ""bello""'
WRITE(un,'(A)')'end of wrong behavior'

CALL init(csv)
CALL csv_record_addfield(csv, 20)
CALL csv_record_addfield(csv, 10.5)
CALL csv_record_addfield(csv, 9.81, '(F4.2)')
CALL csv_record_addfield(csv, 'non numeric')
WRITE(un, '(A)') TRIM(csv_record_getrecord(csv))
CALL delete(csv)

CLOSE(un)

OPEN(un, FILE='tmp.csv') ! lo riapro
nrec = 0
DO WHILE(.TRUE.) ! ciclo sui record
  READ(un,"(A)",iostat=iost) line
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
