!> \brief Utilità per i file.
!!
!! Questo modulo raccoglie utilità di uso generale legate alla gestione dei file.
!! Un primo gruppo di utilità gestisce la localizzazioen e l'apertura di file di
!! configurazione in directory standard o specificate da una variabile d'ambiente.
!! Esso definisce inoltre la classe \a csv_record ed una serie di metodi associati
!! che permette di interpretare i record di un file formato csv.
!! \ingroup base
MODULE file_utilities
USE kinds
USE char_utilities
USE missing_values
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

!> Classe che permette di interpretare i record di un file formato csv.
TYPE csv_record
  PRIVATE
  INTEGER :: cursor !, nfield, ntotal
!  CHARACTER(LEN=1) :: csep, cquote
  INTEGER(KIND=int_b) :: csep, cquote
  INTEGER(KIND=int_b), POINTER :: record(:)
END TYPE csv_record

!> Costruttore per la classe \a csv_record. Deve essere richiamato
!! per ogni record (riga) csv da interpretare.
INTERFACE init
  MODULE PROCEDURE csv_record_init
END INTERFACE

!> Distruttore per la classe \a csv_record. È importante richiamarlo prima
!! di riutilizzare l'oggetto per un record successivo altrimenti si
!! perde memoria allocata.
INTERFACE delete
  MODULE PROCEDURE csv_record_delete
END INTERFACE

!> Metodi per ottenere successivamente i campi di un oggetto \a csv_record.
!! Usare il nome generico \c csv_record_getfield con i parametri opportuni,
!! ci penserà il compiltore a scegliere il metodo giusto.
INTERFACE csv_record_getfield
  MODULE PROCEDURE csv_record_getfield_char, csv_record_getfield_int, &
   csv_record_getfield_real
END INTERFACE

PRIVATE csv_record_init, csv_record_delete, csv_record_getfield_char, &
 csv_record_getfield_int, csv_record_getfield_real

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
!! Resa obsoleta dalla classe ::csv_record.
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

!> Inizializza un oggetto \a csv_record con il record fornito in ingresso.
!! È possibile specificare opzionalmente i caratteri da usare come
!! delimitatore e come raggruppatore di campo, ed è possibile ottenere
!! in uscita il numero di campi presenti. È in generale sconsigliato richiedere
!! esplicitamente il numero di campi se non necessario, perché richiede una
!! quantità aggiuntiva di calcoli, si consiglia di usare il metodo ::csv_record_end.!! Attenzione, la classe \a csv_record non gestisce i record csv che si estendono
!! su più righe.
SUBROUTINE csv_record_init(this, record, csep, cquote, nfield)
TYPE(csv_record),INTENT(INOUT) :: this !< oggetto da inizializzare
CHARACTER(LEN=*),INTENT(IN) :: record !< record csv da interpretare
CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: csep !< carattere separatore di campo, default \c , (virgola)
CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: cquote !< carattere raggruppatore di campo, default \c " (doppio apice); è usato tipicamente quando un campo contiene virgole o spazi iniali o finali
INTEGER,INTENT(OUT),OPTIONAL :: nfield !< numero di campi contenuti nel record

INTEGER :: l

IF (PRESENT(csep)) THEN
  this%csep = TRANSFER(csep, this%csep)
ELSE
  this%csep = TRANSFER(',', this%csep)
ENDIF
IF (PRESENT(cquote)) THEN
  this%cquote = TRANSFER(cquote, this%cquote)
ELSE
  this%cquote = TRANSFER('"', this%cquote)
ENDIF
l = LEN_TRIM(record)
ALLOCATE(this%record(l))
this%record(:) = TRANSFER(record, this%record, l) ! ice in pgf90 with TRIM(record)
this%cursor = 1

IF (PRESENT(nfield)) THEN
  nfield = 0
  DO WHILE(.NOT.csv_record_end(this)) ! faccio un giro a vuoto sul record
    nfield = nfield + 1
    CALL csv_record_getfield(this)
  ENDDO
  this%cursor = 1 ! riazzero il cursore
ENDIF

END SUBROUTINE csv_record_init


!> Distrugge l'oggetto \a csv_record, liberando la memoria allocata.
SUBROUTINE csv_record_delete(this)
TYPE(csv_record), INTENT(INOUT) :: this !< oggetto da distruggere

DEALLOCATE(this%record)

END SUBROUTINE csv_record_delete


!> Riporta il puntatore del record all'inizio per permettere di scorrere
!! nuovamente lo stesso oggetto.
SUBROUTINE csv_record_rewind(this)
TYPE(csv_record),INTENT(INOUT) :: this !< oggetto da reinizializzare

this%cursor = 1

END SUBROUTINE csv_record_rewind


!> Restituisce il campo successivo del record \a this in formato \c CHARACTER.
!! Fa avanzare il puntatore di campo, per cui non è più possibile tornare
!! indietro.
!! Se tutti i campi sono stati interpretati restituisce comunque una stringa
!! nulla (cioè una stringa di spazi e una lunghezza zero), per verificare la
!! fine del record usare il metodo ::csv_record_end .
SUBROUTINE csv_record_getfield_char(this, field, flen, ier)
TYPE(csv_record),INTENT(INOUT) :: this !< oggetto di cui restituire i campi
CHARACTER(LEN=*),INTENT(OUT),OPTIONAL :: field !< contenuto del campo, se non è fornito si limita a far avanzare il puntatore di campo; se la variabile fornita non è lunga a sufficienza viene stampato un warning e viene assegnato solo il possibile;
!< la stringa è comunque teminata da spazi, per cui è necessario usare il parametro \a flen per non perdere eventuali spazi significativi al termine del campo
INTEGER,INTENT(OUT),OPTIONAL :: flen !< lunghezza effettiva del campo, è calcolata correttamente anche nei casi in cui \a field non è fornita o non è sufficientemente lunga per contenere il campo
INTEGER,INTENT(OUT),OPTIONAL :: ier !< codice di errore, 0 = tutto bene, 1 = \a field troppo corta per contenere il campo (ha senso solo se \a field è fornita), 2 = fine record

LOGICAL :: inquote, inpre, inpost, firstquote
INTEGER :: i, ocursor, ofcursor, lier

IF (PRESENT(field)) field = ''
IF (PRESENT(ier)) ier = 0
IF (csv_record_end(this)) THEN
  IF (PRESENT(ier)) ier = 1
  CALL raise_error('in csv_record_getfield, superata la fine record')
  RETURN
ENDIF
lier = 0
ocursor = 0
ofcursor = 0
inquote = .FALSE.
inpre = .TRUE.
inpost = .FALSE.
firstquote = .FALSE.

DO i = this%cursor, SIZE(this%record)
  IF (inpre) THEN ! sono nel preludio, butto via gli spazi
    IF (is_space(this%record(i))) THEN
      CYCLE
    ELSE
      inpre = .FALSE.
    ENDIF
  ENDIF

  IF (.NOT.inquote) THEN ! fuori da " "
    IF (this%record(i) == this%cquote) THEN ! ": inizia " "
      inquote = .TRUE.
      CYCLE
    ELSE IF (this%record(i) == this%csep) THEN ! ,: fine campo
      EXIT
    ELSE ! carattere normale, elimina "trailing blanks"
      CALL add_char(this%record(i), .TRUE., field, ier)
      CYCLE
    ENDIF
  ELSE ! dentro " "
    IF (.NOT.firstquote) THEN ! il precedente non e` "
      IF (this%record(i) == this%cquote) THEN ! ": fine " " oppure ""
        firstquote = .TRUE.
        CYCLE
      ELSE ! carattere normale
        CALL add_char(this%record(i), .FALSE., field, ier)
        CYCLE
      ENDIF
    ELSE ! il precedente e` "
      firstquote = .FALSE.
      IF (this%record(i) == this%cquote) THEN ! ": sequenza ""
        CALL add_char(this%cquote, .FALSE., field, ier)
        CYCLE
      ELSE ! carattere normale: e` terminata " "
        inquote = .FALSE.
        IF (this%record(i) == this%csep) THEN ! , fine campo
          EXIT
        ELSE ! carattere normale, elimina "trailing blanks"
          CALL add_char(this%record(i), .TRUE., field, ier)
          CYCLE
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDDO

this%cursor = MIN(i + 1, SIZE(this%record) + 1)
IF (PRESENT(flen)) flen = ofcursor ! restituisco la lunghezza
IF (PRESENT(field)) THEN ! controllo overflow di field
  IF (ofcursor > LEN(field)) THEN
    IF (PRESENT(ier)) ier = 1
    CALL raise_warning('in csv_record_getfield, stringa troppo corta: '// &
     TRIM(to_char(LEN(field)))//'/'//TRIM(to_char(ocursor)))
  ENDIF
ENDIF

CONTAINS

SUBROUTINE add_char(char, check_space, field, ier)
INTEGER(kind=int_b) :: char
LOGICAL,INTENT(IN) :: check_space
CHARACTER(LEN=*),INTENT(OUT),OPTIONAL :: field
INTEGER,INTENT(OUT),OPTIONAL :: ier

ocursor = ocursor + 1
IF (PRESENT(field)) THEN
  IF (ocursor <= LEN(field)) THEN
    field(ocursor:ocursor) = TRANSFER(char, field)
  ENDIF
ENDIF
IF (check_space) THEN
  IF (.NOT.is_space(char)) ofcursor = ocursor
ELSE
  ofcursor = ocursor
ENDIF

END SUBROUTINE add_char

FUNCTION is_space(char)
INTEGER(kind=int_b) :: char
LOGICAL :: is_space

is_space = (char == 32 .OR. char == 9) ! improve

END FUNCTION is_space

END SUBROUTINE csv_record_getfield_char


!> Restituisce il campo successivo del record \a this in formato \c INTEGER.
!! Fa avanzare il puntatore di campo, per cui non è più possibile tornare
!! indietro.
!! Se il campo non è adatto ad essere convertito in intero (compreso il caso di
!! fine record), oppure se il campo è più lungo di 32 caratteri, viene restituito
!! un valore mancante (vedi missing_values).
SUBROUTINE csv_record_getfield_int(this, field, ier)
TYPE(csv_record),INTENT(INOUT) :: this !< oggetto di cui restituire i campi
INTEGER,INTENT(OUT) :: field !< valore del campo, = \a imiss se la conversione fallisce
INTEGER,INTENT(OUT),OPTIONAL :: ier ! codice di errore, 0 = tutto bene, 1 = impossibile convertire il campo a numero intero, 2 = fine record

CHARACTER(LEN=32) :: cfield
INTEGER :: lier

CALL csv_record_getfield(this, field=cfield, ier=lier)
IF (lier == 0) READ(cfield, '(I32)', iostat=lier) field
IF (lier /= 0) THEN
  field = imiss
  CALL raise_error('in csv_record_getfield_int, campo errato: '//TRIM(cfield))
ENDIF
IF (PRESENT(ier)) ier = lier

END SUBROUTINE csv_record_getfield_int


SUBROUTINE csv_record_getfield_real(this, field, ier)
TYPE(csv_record),INTENT(INOUT) :: this !< oggetto di cui restituire i campi
REAL,INTENT(OUT) :: field !< valore del campo, = \a rmiss se la conversione fallisce
INTEGER,INTENT(OUT),OPTIONAL :: ier ! codice di errore, 0 = tutto bene, 1 = impossibile convertire il campo a numero intero, 2 = fine record

CHARACTER(LEN=32) :: cfield
INTEGER :: lier

CALL csv_record_getfield(this, field=cfield, ier=lier)
IF (lier == 0) READ(cfield, '(F32.0)', iostat=lier) field
IF (lier /= 0) THEN
  field = imiss
  CALL raise_error('in csv_record_getfield_real, campo errato: '//TRIM(cfield))
ENDIF
IF (PRESENT(ier)) ier = lier

END SUBROUTINE csv_record_getfield_real


!> Informa se l'interpretazione del record è giunta al termine (\c .TRUE.)
!! o se ci sono ancora dei campi da interpretare (\c .FALSE.).
FUNCTION csv_record_end(this)
TYPE(csv_record), INTENT(IN) :: this !< oggetto su cui operare
LOGICAL :: csv_record_end

csv_record_end = this%cursor > SIZE(this%record)

END FUNCTION csv_record_end



END MODULE file_utilities
