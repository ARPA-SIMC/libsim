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
MODULE vol7d_serialize_csvdba_class
USE,INTRINSIC :: iso_c_binding
USE vol7d_serialize_class
USE optionparser_class
IMPLICIT NONE

TYPE,EXTENDS(vol7d_serialize) :: vol7d_serialize_csvdba
  CONTAINS
  PROCEDURE :: vol7d_serialize_optionparser
  PROCEDURE :: vol7d_serialize_parse
  PROCEDURE :: vol7d_serialize_export
END TYPE vol7d_serialize_csvdba

PRIVATE
PUBLIC vol7d_serialize_csvdba, vol7d_serialize_csvdba_new

CONTAINS

FUNCTION vol7d_serialize_csvdba_new() RESULT(this)
TYPE(vol7d_serialize_csvdba) :: this

this%vol7d_serialize = vol7d_serialize_new()

END FUNCTION vol7d_serialize_csvdba_new


SUBROUTINE vol7d_serialize_optionparser(this, opt, ext)
CLASS(vol7d_serialize_csvdba),INTENT(inout) :: this
TYPE(optionparser),INTENT(inout),OPTIONAL :: opt
CHARACTER(len=*),INTENT(in),OPTIONAL :: ext

IF (PRESENT(ext)) THEN
  this%ext = ext
ELSE
  this%ext = 'csv'
ENDIF

! reset unconditionally some parameters
this%cachedesc = .TRUE.
this%column = 'ana,network,time,level,timerange,var,value'
this%loop = 'time,timerange,level,ana,network,var'
! no options, the rest all set to default

END SUBROUTINE vol7d_serialize_optionparser


SUBROUTINE vol7d_serialize_parse(this, category)
CLASS(vol7d_serialize_csvdba),INTENT(inout) :: this !< object having undorgone the vol7d_serialize_optionparser method
INTEGER,INTENT(in),OPTIONAL :: category !< log4fortran category for logging error messages

! call the original method
CALL this%vol7d_serialize%vol7d_serialize_parse(category)

END SUBROUTINE vol7d_serialize_parse


SUBROUTINE vol7d_serialize_export(this, iun)
CLASS(vol7d_serialize_csvdba),INTENT(inout) :: this
INTEGER,INTENT(in) :: iun

CHARACTER(len=64),TARGET,ALLOCATABLE :: col(:)
CHARACTER(len=2048),TARGET :: line
CHARACTER(len=1),POINTER :: pline
INTEGER :: i
TYPE(vol7d_serialize_iterline) :: linei
TYPE(vol7d_serialize_itercol) :: coli

! make csv header and count columns
i = 0; line = ''
coli = this%vol7d_serialize_itercol_new()
DO WHILE(coli%next())
  i = i + 1
  IF (i /= 1) THEN
    line(LEN_TRIM(line)+1:) = ','
  ENDIF
! use double pointer to avoid ICE on IBM xlf compiler
  pline => line(LEN_TRIM(line)+1:LEN_TRIM(line)+1)
  CALL coli%export(C_LOC(pline))
END DO

WRITE(iun,'(A)')TRIM(line)
ALLOCATE(col(i))

! loop over lines
linei = this%vol7d_serialize_iterline_new()
DO WHILE(linei%next())
  i = 0; line = ''
  coli = linei%vol7d_serialize_itercol_new()
  DO WHILE(coli%next())
    i = i + 1
    IF (i /= 1) THEN
      line(LEN_TRIM(line)+1:) = ','
    ENDIF
    CALL coli%export(C_LOC(col(i)(1:1)))
    line(LEN_TRIM(line)+1:) = col(i)
  END DO
  WRITE(iun,'(A)')TRIM(line)
END DO

END SUBROUTINE vol7d_serialize_export

END MODULE vol7d_serialize_csvdba_class
