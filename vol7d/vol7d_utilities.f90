MODULE vol7d_utilities
USE file_utilities
USE err_handling
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

INTERFACE count_distinct
  MODULE PROCEDURE count_distincti, count_distinctc
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distincti, pack_distinctc
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distincti, map_distinctc
END INTERFACE

CONTAINS


FUNCTION open_package_file(filename, filetype) RESULT(unit)
CHARACTER(len=*), INTENT(in) :: filename
INTEGER, INTENT(in) :: filetype
INTEGER :: unit

INTEGER :: i, j
CHARACTER(len=512) :: path

IF (program_name == ' ') THEN
  CALL getarg(0, program_name)
  ! program_name_env?
ENDIF

IF (filetype < 1 .OR. filetype > nftype) THEN
  unit = -1
  CALL raise_error('File type not valid')
  RETURN
ENDIF

unit = getunit()
! try with environment variable
CALL getenv(TRIM(program_name_env)//'_'//TRIM(filetypename(filetype)), path)
IF (path /= ' ') THEN
  OPEN(unit, file=TRIM(path)//'/'//filename, status='old', iostat = i)
  IF (i == 0) THEN
    CALL print_info('Ho trovato il file '//TRIM(path)//'/'//filename)
    RETURN
  ENDIF
ENDIF
! try with pathlist
DO j = 1, SIZE(pathlist,1)
  IF (pathlist(j,filetype) == ' ') EXIT
  OPEN(unit, file=TRIM(pathlist(j,filetype))//'/'//TRIM(program_name)//'/' &
   //filename, status='old', iostat = i)
  IF (i == 0) THEN
    CALL print_info('Ho trovato il file '//TRIM(pathlist(j,filetype))//'/' &
     //TRIM(program_name)//'/'//filename)
    RETURN
  ENDIF
ENDDO
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


! conta gli elementi distinti in vect
FUNCTION count_distincti(vect, mask, back) RESULT(count_distinct)
INTEGER,INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
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
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
      DO j = 1, i-1
        IF (vect(j) == vect(i)) CYCLE vectm2
      ENDDO
      count_distinct = count_distinct + 1
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = 1, SIZE(vect)
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) CYCLE vect1
      ENDDO
      count_distinct = count_distinct + 1
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
      DO j = 1, i-1
        IF (vect(j) == vect(i)) CYCLE vect2
      ENDDO
      count_distinct = count_distinct + 1
    ENDDO vect2
  ENDIF
ENDIF

END FUNCTION count_distincti


FUNCTION count_distinctc(vect, mask, back) RESULT(count_distinct)
CHARACTER(len=*),INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
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
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
      DO j = 1, i-1
        IF (vect(j) == vect(i)) CYCLE vectm2
      ENDDO
      count_distinct = count_distinct + 1
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = 1, SIZE(vect)
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) CYCLE vect1
      ENDDO
      count_distinct = count_distinct + 1
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
      DO j = 1, i-1
        IF (vect(j) == vect(i)) CYCLE vect2
      ENDDO
      count_distinct = count_distinct + 1
    ENDDO vect2
  ENDIF
ENDIF

END FUNCTION count_distinctc


! compatta gli elementi distinti di vect in un array
FUNCTION pack_distincti(vect, mask, back) RESULT(pack_distinct)
INTEGER,INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
INTEGER :: pack_distinct(SIZE(vect))

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

END FUNCTION pack_distincti


! vlen provvisorio, altrimenti internal compiler error con pgf90!!!
FUNCTION pack_distinctc(vect, vlen, mask, back) RESULT(pack_distinct)
CHARACTER(len=vlen),INTENT(in) :: vect(:)
INTEGER :: vlen
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
CHARACTER(len=vlen) :: pack_distinct(SIZE(vect))
!CHARACTER(len=len(vect(1))) :: pack_distinct(SIZE(vect))

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

END FUNCTION pack_distinctc


! restituisce gli indici degli elementi distinti di vect impacchettati
! con la pack_distinct
FUNCTION map_distincti(vect, mask, back) RESULT(map_distinct)
INTEGER,INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
INTEGER :: map_distinct(SIZE(vect))

INTEGER :: count_distinct
INTEGER :: i, j
LOGICAL :: lback

IF (PRESENT(back)) THEN
  lback = back
ELSE
  lback = .FALSE.
ENDIF
count_distinct = 0
map_distinct(:) = 0

IF (PRESENT (mask)) THEN
  IF (lback) THEN
    vectm1: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm1
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) THEN
          map_distinct(i) = map_distinct(j)
          CYCLE vectm1
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      map_distinct(i) = count_distinct
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
      DO j = 1, i-1
        IF (vect(j) == vect(i)) THEN
          map_distinct(i) = map_distinct(j)
          CYCLE vectm2
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      map_distinct(i) = count_distinct
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = 1, SIZE(vect)
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) THEN
          map_distinct(i) = map_distinct(j)
          CYCLE vect1
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      map_distinct(i) = count_distinct
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
      DO j = 1, i-1
        IF (vect(j) == vect(i)) THEN
          map_distinct(i) = map_distinct(j)
          CYCLE vect2
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      map_distinct(i) = count_distinct
    ENDDO vect2
  ENDIF
ENDIF

END FUNCTION map_distincti


FUNCTION map_distinctc(vect, mask, back) RESULT(map_distinct)
CHARACTER(len=*),INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
INTEGER :: map_distinct(SIZE(vect))

INTEGER :: count_distinct
INTEGER :: i, j
LOGICAL :: lback

IF (PRESENT(back)) THEN
  lback = back
ELSE
  lback = .FALSE.
ENDIF
count_distinct = 0
map_distinct(:) = 0

IF (PRESENT (mask)) THEN
  IF (lback) THEN
    vectm1: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm1
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) THEN
          map_distinct(i) = map_distinct(j)
          CYCLE vectm1
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      map_distinct(i) = count_distinct
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
      DO j = 1, i-1
        IF (vect(j) == vect(i)) THEN
          map_distinct(i) = map_distinct(j)
          CYCLE vectm2
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      map_distinct(i) = count_distinct
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = 1, SIZE(vect)
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) THEN
          map_distinct(i) = map_distinct(j)
          CYCLE vect1
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      map_distinct(i) = count_distinct
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
      DO j = 1, i-1
        IF (vect(j) == vect(i)) THEN
          map_distinct(i) = map_distinct(j)
          CYCLE vect2
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      map_distinct(i) = count_distinct
    ENDDO vect2
  ENDIF
ENDIF

END FUNCTION map_distinctc


END MODULE vol7d_utilities
