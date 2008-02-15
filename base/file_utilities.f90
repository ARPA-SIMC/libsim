!> \brief Utilità per i file.
!!
!! Questo modulo raccoglie utilità di uso generale legate alla gestione dei file.
!! \ingroup base
MODULE file_utilities
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
INTEGER, PARAMETER :: filetype_data = 1 !< Il file richiesto è un file di dati
INTEGER, PARAMETER :: filetype_config = 2 !< Il file richiesto è un file di configurazione

CHARACTER(len=20) :: program_name='libsim', program_name_env='LIBSIM'


CONTAINS

!> Restituisce il numero di un'unità I/O Fortran attualmente non
!! utilizzata. Restituisce -1 in caso di errore. Da inserire in un'istruzione
!! fortran \c OPEN. Esempio di utilizzo:
!! \code
!! USE file_utilities
!! ...
!! INTEGER :: n
!! ...
!! n=getunit()
!! OPEN(n, FILE='ostregheta.txt')
!! \endcode
FUNCTION getunit() RESULT(unit)
INTEGER :: unit

LOGICAL :: op

DO unit = 100, 32767
  INQUIRE(unit, opened=op)
  IF (.NOT. op) RETURN
ENDDO

CALL raise_error('Too many open files')
unit = -1

END FUNCTION getunit


!> Trova un file specifico per il pacchetto libsim. Esegue una ricerca
!! in varie directory nell'ordine seguente:
!!  - directory specificata dalla variabile d'ambiente \c LIBSIM_DATA per i file di dati o \c LIBSIM_CONFIG per i file di configurazione, se definite
!!  - directory \c /usr/share/libsim per i file di dati o \c /usr/etc/libsim per i file di configurazione
!!  - directory \c /usr/local/share/libsim per i file di dati o \c /usr/local/etc/libsim per i file di configurazione
!! restituisce il path completo del file eventualmente trovato o una stringa vuota
!! se il file non è stato trovato.
FUNCTION get_package_filepath(filename, filetype) RESULT(path)
CHARACTER(len=*), INTENT(in) :: filename !< nome del file da cercare, deve essere un path relativo
INTEGER, INTENT(in) :: filetype !< tipo di file, usare una delle constanti \a ::filetype_data o \a ::filetype_config

INTEGER :: i, j
CHARACTER(len=512) :: path
LOGICAL :: exist

IF (program_name == ' ') THEN
  CALL getarg(0, program_name)
  ! program_name_env?
ENDIF

IF (filetype < 1 .OR. filetype > nftype) THEN
  path = ''
  CALL raise_error('File type not valid')
  RETURN
ENDIF

! try with environment variable
CALL getenv(TRIM(program_name_env)//'_'//TRIM(filetypename(filetype)), path)
IF (path /= ' ') THEN

  path=TRIM(path)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL print_info('Ho trovato il file '//TRIM(path))
    RETURN
  ENDIF
ENDIF
! try with pathlist
DO j = 1, SIZE(pathlist,1)
  IF (pathlist(j,filetype) == ' ') EXIT
  path=TRIM(pathlist(j,filetype))//'/'//TRIM(program_name)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL print_info('Ho trovato il file '//TRIM(path))
    RETURN
  ENDIF
ENDDO
CALL raise_error('File '//TRIM(filename)//' not found')
path = ''

END FUNCTION get_package_filepath


!> Apre un file specifico per il pacchetto libsim. Esegue una ricerca
!! in varie directory nell'ordine seguente:
!!  - directory specificata dalla variabile d'ambiente \c LIBSIM_DATA per i file di dati o \c LIBSIM_CONFIG per i file di configurazione, se definite
!!  - directory \c /usr/share/libsim per i file di dati o \c /usr/etc/libsim per i file di configurazione
!!  - directory \c /usr/local/share/libsim per i file di dati o \c /usr/local/etc/libsim per i file di configurazione
!! restituisce il numero dell'unità associata al file eventualmente trovato
!! e aperto con successo o -1 se il file non è stato trovato o si è verificato
!! un errore in fase di apertura.
FUNCTION open_package_file(filename, filetype) RESULT(unit)
CHARACTER(len=*), INTENT(in) :: filename !< nome del file da cercare, deve essere un path relativo
INTEGER, INTENT(in) :: filetype !< tipo di file, usare una delle constanti \a ::filetype_data o \a ::filetype_config
INTEGER :: unit, i

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


!> Interpreta una riga di un file csv.
!! Restituisce in vdelim gli indici dei separatori (cdelim, default ',')
!! trovati nella stringa line (tipicamente la riga di un file csv)
!! per definizione: vdelim(1)=0, vdelim(size(vdelim))=LEN_TRIM(line)+1 .
!! Ignora le righe che iniziano con il carattere \c # .
!! Il valore restituito dalla funzione è:
!!  - 0 se tutto è andato bene
!!  - -1 se si tratta di una riga di commento
!!  - -2 se la riga contiene un numero di separatori maggiore della dimensione di
!!    \a vdelim, in tal caso sono assegnati solo quelli che ci stanno in \a vdelim.
!!
!! Esempio di utilizzo:
!! \code
!! INTEGER,PARAMETER :: nf=4 ! formato file
!! INTEGER :: i, sep(nf), n1, n2, un, i1, i2, i3
!! CHARACTER(len=512) :: line
!!
!! un = open_package_file('varmap.csv', filetype_data)
!! IF (un < 0) CALL raise_fatal_error('non trovo il file delle variabili')
!!
!! ...
!! i = 0
!! readline: DO WHILE(.TRUE.)
!!   READ(un,'(A)',END=120)line
!!   IF (delim_csv(line, sep) < 0) CYCLE readline
!!   i = i + 1
!!   READ(line(sep(1)+1:sep(2)-1),'(I8)')vartable(i)%varora
!!   READ(line(sep(2)+1:sep(3)-1),'(A)')vartable(i)%varbt
!!   READ(line(sep(3)+1:sep(4)-1),'(A)')vartable(i)%unit
!!  ENDDO readline
!! 120 CONTINUE
!! CALL print_info('Ho letto '//TRIM(to_char(i))//' variabili dalla tabella')
!! CLOSE(un)
!! \endcode
!! Per leggere un file tipo:
!! \code
!! 160,B13011,mm->KG/M**2
!! 220,B13011,mm/10->KG/M**2
!! 159,B13011,mm->KG/M**2
!! 159,B13011,mm->KG/M**2
!! \endcode
FUNCTION delim_csv(line, vdelim, cdelim) RESULT(status)
CHARACTER(len=*),INTENT(in) :: line !< riga in formato csv da interpretare
INTEGER,INTENT(out) :: vdelim(:) !< vettore degli indici dei separatori trovati in \a line, per definizione vdelim(1)=0, vdelim(size(vdelim))=LEN_TRIM(line)+1, deve essere dimansionato al numero di campi previsti in \a line +1
CHARACTER(len=1),INTENT(in),OPTIONAL :: cdelim !< carattere da interpretare come delimitatore, default \c ','
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


FUNCTION delim_csv_q(line, vdelim, cdelim, cquot) RESULT(status)
CHARACTER(len=*),INTENT(in) :: line !< riga in formato csv da interpretare
INTEGER,INTENT(out) :: vdelim(:) !< vettore degli indici dei separatori trovati in \a line, per definizione vdelim(1)=0, vdelim(size(vdelim))=LEN_TRIM(line)+1, deve essere dimansionato al numero di campi previsti in \a line +1
CHARACTER(len=1),INTENT(in),OPTIONAL :: cdelim !< carattere da interpretare come delimitatore di campi, default \c ','
CHARACTER(len=1),INTENT(in),OPTIONAL :: cquot !< carattere da interpretare come delimitatore, default \c ','
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

END FUNCTION delim_csv_q


END MODULE file_utilities
