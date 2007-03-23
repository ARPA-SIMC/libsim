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

END MODULE vol7d_utilities
