MODULE vol7d_serialize_csv_class
USE,INTRINSIC :: iso_c_binding
USE vol7d_serialize_class
USE optionparser_class
IMPLICIT NONE

TYPE,EXTENDS(vol7d_serialize) :: vol7d_serialize_csv
  INTEGER :: csv_header=2
  CONTAINS
  PROCEDURE :: vol7d_serialize_optionparser
  PROCEDURE :: vol7d_serialize_export
END TYPE vol7d_serialize_csv

PRIVATE
PUBLIC vol7d_serialize_csv

CONTAINS

SUBROUTINE vol7d_serialize_optionparser(this, opt, ext)
CLASS(vol7d_serialize_csv),INTENT(inout) :: this
TYPE(optionparser),INTENT(inout) :: opt
CHARACTER(len=*) :: ext

CALL this%vol7d_serialize%vol7d_serialize_optionparser(opt, ext)
CALL optionparser_add(opt, ' ', 'csv-header', this%csv_header, 2, help= &
 'write 0 to 2 header lines at the beginning of csv output')

this%vol7d_Serialize%variable = 'all'
this%vol7d_Serialize%cachedesc = .TRUE.

END SUBROUTINE vol7d_serialize_optionparser


SUBROUTINE vol7d_serialize_export(this, iun)
CLASS(vol7d_serialize_csv),INTENT(inout) :: this
INTEGER,INTENT(in) :: iun

CHARACTER(len=64),TARGET,ALLOCATABLE :: col(:)
CHARACTER(len=512),TARGET :: line
INTEGER :: i
TYPE(vol7d_serialize_iterline) :: linei
TYPE(vol7d_serialize_itercol) :: coli

i = 0
line = ''
coli = this%vol7d_serialize_itercol_new()
DO WHILE(coli%next())
  i = i + 1
  IF (i /= 1) THEN
    line(LEN_TRIM(line)+1:) = ','
  ENDIF
  CALL coli%export(C_LOC(line(LEN_TRIM(line)+1:LEN_TRIM(line)+1)))
END DO
WRITE(iun,'(A)')TRIM(line)
ALLOCATE(col(i))

linei = this%vol7d_serialize_iterline_new()
DO WHILE(linei%next())
  i = 0
  line = ''
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

END MODULE vol7d_serialize_csv_class
