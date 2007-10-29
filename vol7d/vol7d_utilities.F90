MODULE vol7d_utilities
USE file_utilities
USE err_handling
USE datetime_class
USE kinds

IMPLICIT NONE

INTEGER, PARAMETER, PRIVATE :: nftype = 2
CHARACTER(len=16), PARAMETER, PRIVATE :: &
 pathlist(2,nftype) = RESHAPE((/ &
 '/usr/share      ', '/usr/local/share', &
 '/etc            ', '/usr/local/etc  ' /), &
 (/2,nftype/))
CHARACTER(len=6), PARAMETER, PRIVATE :: &
 filetypename(nftype) = (/ 'DATA  ', 'CONFIG' /)
INTEGER, PARAMETER :: filetype_data = 1, filetype_config = 2

CHARACTER(len=20) :: program_name='vol7d', program_name_env='VOL7D'


! la routine per i char non puo' essere sviluppata in macro perche` si deve scrivere diversa
!cosi' esiste la function count_distinctc (senza _ ) e la subroutine pack_distinctc qui ivi scritte

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_i, count_distinct_r, count_distinct_d, &
   count_distinct_datetime, count_distinct_c
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_i, pack_distinct_r, pack_distinct_d, &
   pack_distinct_datetime !, pack_distinct_c
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_i, map_distinct_r, map_distinct_d, &
   map_distinct_datetime, map_distinct_c
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_i, map_inv_distinct_r, map_inv_distinct_d, &
   map_inv_distinct_datetime, map_inv_distinct_c
END INTERFACE

CONTAINS


FUNCTION get_package_filepath(filename, filetype) RESULT(path)
CHARACTER(len=*), INTENT(in) :: filename
INTEGER, INTENT(in) :: filetype

INTEGER :: i, j
CHARACTER(len=512) :: path
LOGICAL :: exist

IF (program_name == ' ') THEN
  CALL getarg(0, program_name)
  ! program_name_env?
ENDIF

IF (filetype < 1 .OR. filetype > nftype) THEN
  path = ""
  CALL raise_error('File type not valid')
  RETURN
ENDIF

! try with environment variable
CALL getenv(TRIM(program_name_env)//'_'//TRIM(filetypename(filetype)), path)
IF (path /= ' ') THEN

  path=TRIM(path)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL print_info('Ho trovato il file '//path)
    RETURN
  ENDIF
ENDIF
! try with pathlist
DO j = 1, SIZE(pathlist,1)
  IF (pathlist(j,filetype) == ' ') EXIT
  path=TRIM(pathlist(j,filetype))//'/'//TRIM(program_name)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL print_info('Ho trovato il file '//path)
    RETURN
  ENDIF
ENDDO
CALL raise_error('File '//TRIM(filename)//' not found')
path = ""

END FUNCTION get_package_filepath


FUNCTION open_package_file(filename, filetype) RESULT(unit)
CHARACTER(len=*), INTENT(in) :: filename
INTEGER, INTENT(in) :: filetype
INTEGER :: unit,i

CHARACTER(len=512) :: path

IF (filetype < 1 .OR. filetype > nftype) THEN
  unit = -1
  CALL raise_error('File type not valid')
  RETURN
ENDIF

unit = getunit()

path=get_package_filepath(filename, filetype)

IF (path /= ' ') THEN
  OPEN(unit, file=path, status='old', iostat = i)
  IF (i == 0) THEN
    CALL print_info('Ho aperto il file '//TRIM(path))
    RETURN
  ENDIF
ENDIF

CALL raise_error('File '//TRIM(filename)//' not found')
unit = -1

END FUNCTION open_package_file


! Ritorna in vdelim gli indici dei separatori (cdelim, default ',')
! trovati nella stringa line (tipicamente la riga di un file csv)
! per definizione: vdelim(1)=0, vdelim(size(vdelim))=LEN_TRIM(line)+1
FUNCTION delim_csv(line, vdelim, cdelim) RESULT(status)
CHARACTER(len=*),INTENT(in) :: line
INTEGER,INTENT(out) :: vdelim(:)
CHARACTER(len=1),INTENT(in),OPTIONAL :: cdelim
INTEGER :: status

CHARACTER (len=1) :: cldelim
INTEGER :: j

IF (line(1:1) == '#') THEN ! Commento
  status = -1
  RETURN
ENDIF
IF (PRESENT(cdelim)) THEN
  cldelim = cdelim
ELSE
  cldelim = ','
ENDIF

vdelim(1) = 0
DO j = 2, SIZE(vdelim)-1
  vdelim(j) = INDEX(line(vdelim(j-1)+1:), cldelim) + vdelim(j-1)
  IF (vdelim(j) == vdelim(j-1)) THEN ! N. di record insufficiente
    vdelim(j+1:) = vdelim(j)
    status = -2
    RETURN
  ENDIF
ENDDO
status = 0
vdelim(j) = LEN_TRIM(line) + 1

END FUNCTION delim_csv


! Ritorna l'indice del primo elemento vero del vettore logico v
FUNCTION firsttrue(v) RESULT(i)
LOGICAL,INTENT(in) :: v(:)
INTEGER :: i

DO i = 1, SIZE(v)
  IF (v(i)) RETURN
ENDDO
i = 0

END FUNCTION firsttrue


! Definisce le funzioni count_distinct e pack_distinct
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE INTEGER
#define VOL7D_POLY_TYPES _i
#include "vol7d_distinct.F90"

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE REAL
#define VOL7D_POLY_TYPES _r
#include "vol7d_distinct.F90"

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE REAL(kind=fp_d)
#define VOL7D_POLY_TYPES _d
#include "vol7d_distinct.F90"

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE TYPE(datetime)
#define VOL7D_POLY_TYPES _datetime
#include "vol7d_distinct.F90"

#define VOL7D_NO_PACK
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE CHARACTER(len=*)
#define VOL7D_POLY_TYPES _c
#include "vol7d_distinct.F90"


SUBROUTINE pack_distinct_c(vect, pack_distinct, mask, back) !RESULT(pack_distinct)
CHARACTER(len=*),INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
CHARACTER(len=LEN(vect)) :: pack_distinct(SIZE(vect))

INTEGER :: count_distinct
INTEGER :: i, j
LOGICAL :: lback

IF (PRESENT(back)) THEN
  lback = back
ELSE
  lback = .FALSE.
ENDIF
count_distinct = 0

IF (PRESENT (mask)) THEN
  IF (lback) THEN
    vectm1: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm1
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) CYCLE vectm1
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
      DO j = 1, i-1
        IF (vect(j) == vect(i)) CYCLE vectm2
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = 1, SIZE(vect)
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) CYCLE vect1
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
      DO j = 1, i-1
        IF (vect(j) == vect(i)) CYCLE vect2
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vect2
  ENDIF
ENDIF

END SUBROUTINE pack_distinct_c


END MODULE vol7d_utilities
