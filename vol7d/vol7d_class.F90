!> \defgroup vol7d Pacchetto libsim, libreria vol7d.
!! La libreria vol7d di libsim contiene classi per la gestione in
!! Fortran 90 di dati puntuali, tipicamente osservazioni da stazioni meteo,
!! e per la loro importazione da Db-Alle e dal database Oracle del SIM.
!! Per compilare e linkare programmi che fanno uso di questa libreria si
!! dovranno inserire gli appositi comandi \c USE nelle unità di
!! programma coinvolte e usare, in fase di compilazione, l'opzione
!! \c -I/usr/include e, in fase di linking, l'opzione
!! \c -lsim_vol7d, presupponendo che l'installazione sia stata
!! fatta a livello di sistema.

!> Classe per la gestione di un volume completo di dati osservati.
!! Questo modulo definisce gli oggetti e i metodi per gestire
!! volumi di dati meteorologici sparsi.
!! I volumi definiti sono principalmente di 4 categorie:
!!  - volumi di anagrafica (vol7d::volanar & c.), hanno 3 dimensioni:
!!    - anagrafica
!!    - variabile di anagrafica
!!    - rete
!!  - volumi di attributi di anagrafica (vol7d::volanaattrr & c.), hanno 4 dimensioni:
!!    - anagrafica
!!    - variabile di anagrafica
!!    - rete
!!    - variabile di attributi delle variabili di anagrafica
!!  - volumi di dati (vol7d::voldatir & c.), hanno 6 dimensioni:
!!    - anagrafica
!!    - tempo
!!    - livello verticale
!!    - intervallo temporale (timerange)
!!    - variabile di dati
!!    - rete
!!  - volumi di attributi di dati (vol7d::voldatiattrr & c.), hanno 7 dimensioni:
!!    - anagrafica
!!    - tempo
!!    - livello verticale
!!    - intervallo temporale (timerange)
!!    - variabile di dati
!!    - rete
!!    - variabile di attributi delle variabili di dati
!!
!! Tutte le variabili sono inoltre disponibil1 in 5 tipi diversi:
!!  - reale (abbreviato r)
!!  - doppia precisione (abbreviato d)
!!  - intero  (abbreviato i)
!!  - byte (abbreviato b)
!!  - carattere (abbreviato c)
!!
!! Per ognuna delle dimensioni possibili, incluse le variabili e gli
!! attributi con i loro diversi tipi,
!! è definito un cosiddetto "vettore di descrittori", con un
!! numero di elementi pari all'estensione della dimensione stessa,
!! che contiene le informazioni necessarie a descrivere
!! gli elementi di quella dimensione.
!! In realtà l'utente non dovrà generalmente occuparsi di costruire
!! un oggetto vol7d con le proprie mani ma utilizzerà nella maggior parte
!! dei casi i metodi di importazione preconfezionati che importano dati da
!! DB-All.e (vol7d_dballe_class) o dal DB Oracle del SIM (vol7d_oraclesim_class).
!! 
!!
!! Il programma esempio_v7d.f90 contiene un esempio elementare di uso
!! della classe vol7d:
!! \include esempio_v7d.f90
!!
!! \ingroup vol7d
MODULE vol7d_class
USE kinds
USE err_handling
USE datetime_class
USE optional_values
USE vol7d_utilities
USE vol7d_ana_class
USE vol7d_timerange_class
USE vol7d_level_class
USE vol7d_network_class
USE vol7d_varvect_class
IMPLICIT NONE


INTEGER, PARAMETER :: vol7d_maxdim_a = 3, vol7d_maxdim_aa = 4, &
 vol7d_maxdim_d = 6, vol7d_maxdim_ad = 7

INTEGER, PARAMETER :: vol7d_ana_a=1 !< indice della dimensione "anagrafica" nei volumi di anagrafica, da usare nei metodi vol7d_get_volana*
INTEGER, PARAMETER :: vol7d_var_a=2 !< indice della dimensione "variabile" nei volumi di anagrafica, da usare nei metodi vol7d_get_volana*
INTEGER, PARAMETER :: vol7d_network_a=3 !< indice della dimensione "rete" nei volumi di anagrafica, da usare nei metodi vol7d_get_volana*
INTEGER, PARAMETER :: vol7d_attr_a=4 !< indice della dimensione "attributo" nei volumi di anagrafica, da usare nei metodi vol7d_get_volana*
INTEGER, PARAMETER :: vol7d_ana_d=1 !< indice della dimensione "anagrafica" nei volumi di dati, da usare nei metodi vol7d_get_voldati*
INTEGER, PARAMETER :: vol7d_time_d=2 !< indice della dimensione "tempo" nei volumi di dati, da usare nei metodi vol7d_get_voldati*
INTEGER, PARAMETER :: vol7d_level_d=3 !< indice della dimensione "livello verticale" nei volumi di dati, da usare nei metodi vol7d_get_voldati*
INTEGER, PARAMETER :: vol7d_timerange_d=4 !< indice della dimensione "intervallo temporale" nei volumi di dati, da usare nei metodi vol7d_get_voldati*
INTEGER, PARAMETER :: vol7d_var_d=5 !< indice della dimensione "variabile" nei volumi di dati, da usare nei metodi vol7d_get_voldati*
INTEGER, PARAMETER :: vol7d_network_d=6 !< indice della dimensione "rete" nei volumi di dati, da usare nei metodi vol7d_get_voldati*
INTEGER, PARAMETER :: vol7d_attr_d=7 !< indice della dimensione "attributo" nei volumi di dati, da usare nei metodi vol7d_get_voldati*
INTEGER, PARAMETER :: vol7d_cdatalen=20

TYPE vol7d_varmap
  INTEGER :: r, d, i, b, c
END TYPE vol7d_varmap

!> Definisce un oggetto contenente i volumi anagrafica e dati e tutti
!! i descrittori delle loro dimensioni.
TYPE vol7d
!> vettore descrittore della dimensione anagrafica
  TYPE(vol7d_ana),POINTER :: ana(:)
!> vettore descrittore della dimensione tempo
  TYPE(datetime),POINTER :: time(:)
!> vettore descrittore della dimensione livello verticale
  TYPE(vol7d_level),POINTER :: level(:)
!> vettore descrittore della dimensione intervallo temporale (timerange)
  TYPE(vol7d_timerange),POINTER :: timerange(:)
!> vettore descrittore della dimensione rete
  TYPE(vol7d_network),POINTER :: network(:)
!> vettore descrittore della dimensione variabile di anagrafica
  TYPE(vol7d_varvect) :: anavar
!> vettore descrittore della dimensione attributo delle variabili di anagrafica
    TYPE(vol7d_varvect) :: anaattr
!> vettore descrittore della dimensione variabile di anagrafica che ha tali attributi
    TYPE(vol7d_varvect) :: anavarattr
!> vettore descrittore della dimensione variabile di dati
    TYPE(vol7d_varvect) :: dativar
!> vettore descrittore della dimensione attributo delle variabili di dati
    TYPE(vol7d_varvect) :: datiattr
!> vettore descrittore della dimensione variabile di dati che ha tali attributi
    TYPE(vol7d_varvect) :: dativarattr

!> volume di anagrafica a valori reali
  REAL,POINTER :: volanar(:,:,:)
!> volume di anagrafica a valori a doppia precisione
  REAL(kind=fp_d),POINTER :: volanad(:,:,:)
!> volume di anagrafica a valori interi
  INTEGER,POINTER :: volanai(:,:,:)
!> volume di anagrafica a valori byte
  INTEGER(kind=int_b),POINTER :: volanab(:,:,:)
!> volume di anagrafica a valori carattere
  CHARACTER(len=vol7d_cdatalen),POINTER :: volanac(:,:,:)

!> volume di attributi di anagrafica a valori reali
  REAL,POINTER :: volanaattrr(:,:,:,:)
!> volume di attributi di anagrafica a valori a doppia precisione
  REAL(kind=fp_d),POINTER :: volanaattrd(:,:,:,:)
!> volume di attributi di anagrafica a valori interi
  INTEGER,POINTER :: volanaattri(:,:,:,:)
!> volume di attributi di anagrafica a valori byte
  INTEGER(kind=int_b),POINTER :: volanaattrb(:,:,:,:)
!> volume di attributi di anagrafica a valori carattere
  CHARACTER(len=vol7d_cdatalen),POINTER :: volanaattrc(:,:,:,:)

!> volume di dati a valori reali
  REAL,POINTER :: voldatir(:,:,:,:,:,:) ! sono i dati
!> volume di dati a valori a doppia precisione
  REAL(kind=fp_d),POINTER :: voldatid(:,:,:,:,:,:)
!> volume di dati a valori interi
  INTEGER,POINTER :: voldatii(:,:,:,:,:,:)
!> volume di dati a valori byte
  INTEGER(kind=int_b),POINTER :: voldatib(:,:,:,:,:,:)
!> volume di dati a valori carattere
  CHARACTER(len=vol7d_cdatalen),POINTER :: voldatic(:,:,:,:,:,:)

!> volume di attributi di dati a valori reali
  REAL,POINTER :: voldatiattrr(:,:,:,:,:,:,:)
!> volume di attributi di dati a valori a doppia precisione
  REAL(kind=fp_d),POINTER :: voldatiattrd(:,:,:,:,:,:,:)
!> volume di attributi di dati a valori interi
  INTEGER,POINTER :: voldatiattri(:,:,:,:,:,:,:)
!> volume di attributi di dati a valori byte
  INTEGER(kind=int_b),POINTER :: voldatiattrb(:,:,:,:,:,:,:)
!> volume di attributi di dati a valori carattere
  CHARACTER(len=vol7d_cdatalen),POINTER :: voldatiattrc(:,:,:,:,:,:,:)

END TYPE vol7d

!> Costruttore per la classe vol7d.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE vol7d_init
END INTERFACE

!> Distruttore per la classe vol7d.
INTERFACE delete
  MODULE PROCEDURE vol7d_delete
END INTERFACE

!> Scrittura su file.
INTERFACE export
  MODULE PROCEDURE vol7d_write_on_file
END INTERFACE

!> Lettura da file.
INTERFACE import
  MODULE PROCEDURE vol7d_read_from_file
END INTERFACE

!>Print object
INTERFACE display
  MODULE PROCEDURE vol7d_display
END INTERFACE

!>doubleprecision data conversion
INTERFACE doubledat
  MODULE PROCEDURE doubledatd,doubledatr,doubledati,doubledatb,doubledatc
END INTERFACE

!>real data conversion
INTERFACE realdat
  MODULE PROCEDURE realdatd,realdatr,realdati,realdatb,realdatc
END INTERFACE



!!$INTERFACE get_volana
!!$  MODULE PROCEDURE vol7d_get_volanar, vol7d_get_volanad, vol7d_get_volanai, &
!!$   vol7d_get_volanab, vol7d_get_volanac
!!$END INTERFACE
!!$
!!$INTERFACE get_voldati
!!$  MODULE PROCEDURE vol7d_get_voldatir, vol7d_get_voldatid, vol7d_get_voldatii, &
!!$   vol7d_get_voldatib, vol7d_get_voldatic
!!$END INTERFACE
!!$
!!$INTERFACE get_volanaattr
!!$  MODULE PROCEDURE vol7d_get_volanaattrr, vol7d_get_volanaattrd, &
!!$   vol7d_get_volanaattri, vol7d_get_volanaattrb, vol7d_get_volanaattrc
!!$END INTERFACE
!!$
!!$INTERFACE get_voldatiattr
!!$  MODULE PROCEDURE vol7d_get_voldatiattrr, vol7d_get_voldatiattrd, &
!!$   vol7d_get_voldatiattri, vol7d_get_voldatiattrb, vol7d_get_voldatiattrc
!!$END INTERFACE

PRIVATE vol7d_get_volr, vol7d_get_vold, vol7d_get_voli, vol7d_get_volb, &
 vol7d_get_volc, &
 volptr1dr, volptr2dr, volptr3dr, volptr4dr, volptr5dr, volptr6dr, volptr7dr, &
 volptr1dd, volptr2dd, volptr3dd, volptr4dd, volptr5dd, volptr6dd, volptr7dd, &
 volptr1di, volptr2di, volptr3di, volptr4di, volptr5di, volptr6di, volptr7di, &
 volptr1db, volptr2db, volptr3db, volptr4db, volptr5db, volptr6db, volptr7db, &
 volptr1dc, volptr2dc, volptr3dc, volptr4dc, volptr5dc, volptr6dc, volptr7dc, &
 vol7d_nullifyr, vol7d_nullifyd, vol7d_nullifyi, vol7d_nullifyb, vol7d_nullifyc, &
 vol7d_init, vol7d_delete, vol7d_write_on_file, vol7d_read_from_file, &
 vol7d_check_alloc_ana,  vol7d_check_alloc_dati, vol7d_display

PRIVATE doubledatd,doubledatr,doubledati,doubledatb,doubledatc

CONTAINS


!> Inizializza un oggetto di tipo vol7d.
!! Non riceve alcun parametro tranne l'oggetto stesso. Attenzione, è necessario
!! comunque chiamare sempre il costruttore per evitare di avere dei puntatori in
!! uno stato indefinito.
SUBROUTINE vol7d_init(this)
TYPE(vol7d),intent(out) :: this !< oggetto da inizializzare

CALL init(this%anavar)
CALL init(this%anaattr)
CALL init(this%anavarattr)
CALL init(this%dativar)
CALL init(this%datiattr)
CALL init(this%dativarattr)

NULLIFY(this%ana, this%time, this%level, this%timerange, this%network)

NULLIFY(this%volanar, this%volanaattrr, this%voldatir, this%voldatiattrr)
NULLIFY(this%volanad, this%volanaattrd, this%voldatid, this%voldatiattrd)
NULLIFY(this%volanai, this%volanaattri, this%voldatii, this%voldatiattri)
NULLIFY(this%volanab, this%volanaattrb, this%voldatib, this%voldatiattrb)
NULLIFY(this%volanac, this%volanaattrc, this%voldatic, this%voldatiattrc)

END SUBROUTINE vol7d_init


!> Distrugge l'oggetto in maniera pulita, liberando l'eventuale memoria
!! dinamicamente allocata. Permette di distruggere la sola parte di dati
!! mantenendo l'anagrafica.
ELEMENTAL SUBROUTINE vol7d_delete(this, dataonly)
TYPE(vol7d),intent(inout) :: this !< oggetto da distruggere
LOGICAL, INTENT(in), OPTIONAL :: dataonly !< dealloca solo i dati, tenendo l'anagrafica, (default \c .FALSE.)


IF (.NOT. optio_log(dataonly)) THEN
  IF (ASSOCIATED(this%volanar)) DEALLOCATE(this%volanar)
  IF (ASSOCIATED(this%volanad)) DEALLOCATE(this%volanad)
  IF (ASSOCIATED(this%volanai)) DEALLOCATE(this%volanai)
  IF (ASSOCIATED(this%volanab)) DEALLOCATE(this%volanab)
  IF (ASSOCIATED(this%volanac)) DEALLOCATE(this%volanac)
  IF (ASSOCIATED(this%volanaattrr)) DEALLOCATE(this%volanaattrr)
  IF (ASSOCIATED(this%volanaattrd)) DEALLOCATE(this%volanaattrd)
  IF (ASSOCIATED(this%volanaattri)) DEALLOCATE(this%volanaattri)
  IF (ASSOCIATED(this%volanaattrb)) DEALLOCATE(this%volanaattrb)
  IF (ASSOCIATED(this%volanaattrc)) DEALLOCATE(this%volanaattrc)
ENDIF
IF (ASSOCIATED(this%voldatir)) DEALLOCATE(this%voldatir)
IF (ASSOCIATED(this%voldatid)) DEALLOCATE(this%voldatid)
IF (ASSOCIATED(this%voldatii)) DEALLOCATE(this%voldatii)
IF (ASSOCIATED(this%voldatib)) DEALLOCATE(this%voldatib)
IF (ASSOCIATED(this%voldatic)) DEALLOCATE(this%voldatic)
IF (ASSOCIATED(this%voldatiattrr)) DEALLOCATE(this%voldatiattrr)
IF (ASSOCIATED(this%voldatiattrd)) DEALLOCATE(this%voldatiattrd)
IF (ASSOCIATED(this%voldatiattri)) DEALLOCATE(this%voldatiattri)
IF (ASSOCIATED(this%voldatiattrb)) DEALLOCATE(this%voldatiattrb)
IF (ASSOCIATED(this%voldatiattrc)) DEALLOCATE(this%voldatiattrc)

IF (.NOT. optio_log(dataonly)) THEN
  IF (ASSOCIATED(this%ana)) DEALLOCATE(this%ana)
  IF (ASSOCIATED(this%network)) DEALLOCATE(this%network)
ENDIF
IF (ASSOCIATED(this%time)) DEALLOCATE(this%time)
IF (ASSOCIATED(this%level)) DEALLOCATE(this%level)
IF (ASSOCIATED(this%timerange)) DEALLOCATE(this%timerange)

IF (.NOT. optio_log(dataonly)) THEN
  CALL delete(this%anavar)
  CALL delete(this%anaattr)
  CALL delete(this%anavarattr)
ENDIF
CALL delete(this%dativar)
CALL delete(this%datiattr)
CALL delete(this%dativarattr)

END SUBROUTINE vol7d_delete


!TODO da completare !
!> stampa a video una sintesi del contenuto
SUBROUTINE vol7d_display(this)
TYPE(vol7d),intent(in) :: this !< oggetto da visualizzare
integer :: i


print*,"<<<<<<<<<<<<<<<<<<< vol7d object >>>>>>>>>>>>>>>>>>>>"

IF (ASSOCIATED(this%network))then
  print*,"---- network vector ----"
  print*,"elements=",size(this%network)
  do i=1, size(this%network)
    call display(this%network(i))
  end do
end IF

IF (ASSOCIATED(this%ana))then
  print*,"---- ana vector ----"
  print*,"elements=",size(this%ana)
  do i=1, size(this%ana)
    call display(this%ana(i))
  end do
end IF

IF (ASSOCIATED(this%time))then        
  print*,"---- time vector ----"
  print*,"elements=",size(this%time)
  do i=1, size(this%time)
    call display(this%time(i))
  end do
end if

IF (ASSOCIATED(this%level)) then
  print*,"---- level vector ----"
  print*,"elements=",size(this%level)
  do i =1,size(this%level)
    call display(this%level(i))
  end do
end if

IF (ASSOCIATED(this%timerange))then
  print*,"---- timerange vector ----"
  print*,"elements=",size(this%timerange)
  do i =1,size(this%timerange)
    call display(this%timerange(i))
  end do
end if


print*,"---- ana vector ----"

print*,"- anavar -"
call display(this%anavar)
print*,"- anaattr -"
call display(this%anaattr)
print*,"- anavarattr -"
call display(this%anavarattr)

print*,"---- data vector ----"

print*,"- dativar -"
call display(this%dativar)
print*,"- datiattr -"
call display(this%datiattr)
print*,"- dativarattr -"
call display(this%dativarattr)

print*,"<<<<<<<<<<<<<<<<<<< END vol7d object >>>>>>>>>>>>>>>>>>>>"

END SUBROUTINE vol7d_display


!> Metodo per allocare i descrittori delle 7 dimensioni.
!! Riceve un grande numero di parametri opzionali che
!! indicano quali descrittori allocare e con quale estensione;
!! i descrittori non specificati non vengono toccati.
!! Può essere quindi chiamato più volte allocando via via
!! descrittori relativi a dimensioni diverse.
!! Se un descrittore richiesto è già allocato, viene deallocato
!! (perdendone l'eventuale contenuto) e riallocato con l'estensione
!! richiesta.
!! Per i descrittori relativi a dimensioni che non siano variabili o attributi,
!! è possibile specificare l'estensione di una dimensione a 0,
!! in tal caso il descrittore viene comunque allocato con lunghezza nulla,
!! che è diverso da non allocarlo. Per i descrittori di variabili e attributi
!! passare un'estensione 0 equivale a non fornire il parametro.
!! Avere uno o più descrittori dimensionati con estensione nulla fa sì
!! che anche il volume dati successivamente allocato abbia estensione nulla;
!! sebbene ciò appaia inutile, un volume del genere può in realtà servire,
!! in associazione ai metodi ::vol7d_merge o ::vol7d_append per estendere
!! un volume esistente aggiungendo elementi in alcune dimensioni (quelle
!! a estensione non nulla, ovviamente) e mantenendo invariato tutto il resto.
!! Per quanto riguarda i descrittori delle dimensioni relative alle
!! variabili, la relativa estesnsione è specificata con la nomenclatura
!! \a n<x><y><z> dove <x> può valere:
!!  - \a ana per variabili relative a voumi di anagrafica
!!  - \a dati per variabili relative a voumi di dati
!!
!! <y> può valere:
!!  - \a var per variabili
!!  - \a attr per attributi
!!  - \a varattr variabili aventi attributi nei volumi di attributi
!!
!! <z> può valere:
!!  - \a r per variabili o attributi a valori reali
!!  - \a d per variabili o attributi a valori a doppia precisione
!!  - \a i per variabili o attributi a valori interi
!!  - \a b per variabili o attributi a valori byte
!!  - \a c per variabili o attributi a valori carattere
!!
SUBROUTINE vol7d_alloc(this, nana, ntime, nlevel, ntimerange, nnetwork, &
 nanavarr, nanavard, nanavari, nanavarb, nanavarc, &
 nanaattrr, nanaattrd, nanaattri, nanaattrb, nanaattrc, &
 nanavarattrr, nanavarattrd, nanavarattri, nanavarattrb, nanavarattrc, &
 ndativarr, ndativard, ndativari, ndativarb, ndativarc, &
 ndatiattrr, ndatiattrd, ndatiattri, ndatiattrb, ndatiattrc, &
 ndativarattrr, ndativarattrd, ndativarattri, ndativarattrb, ndativarattrc, &
 ini)
TYPE(vol7d),INTENT(inout) :: this !< oggetto di cui allocare i descrittori
INTEGER,INTENT(in),OPTIONAL :: nana !< estensione della dimensione anagrafica
INTEGER,INTENT(in),OPTIONAL :: ntime !< estensione della dimensione tempo
INTEGER,INTENT(in),OPTIONAL :: nlevel !< estensione della dimensione livello varticale
INTEGER,INTENT(in),OPTIONAL :: ntimerange !< estensione della dimensione intervallo temporale (timerange)
INTEGER,INTENT(in),OPTIONAL :: nnetwork !< estensione della dimensione rete
!> estensione delle possibili dimensioni variabile
INTEGER,INTENT(in),OPTIONAL :: &
 nanavarr, nanavard, nanavari, nanavarb, nanavarc, &
 nanaattrr, nanaattrd, nanaattri, nanaattrb, nanaattrc, &
 nanavarattrr, nanavarattrd, nanavarattri, nanavarattrb, nanavarattrc, &
 ndativarr, ndativard, ndativari, ndativarb, ndativarc, &
 ndatiattrr, ndatiattrd, ndatiattri, ndatiattrb, ndatiattrc, &
 ndativarattrr, ndativarattrd, ndativarattri, ndativarattrb, ndativarattrc
LOGICAL,INTENT(in),OPTIONAL :: ini !< se fornito e vale \c .TRUE., viene chiamato il costruttore, senza parametri opzionali, per ogni elemento di tutti i descrittori allocati, inizializzandolo quindi a valore mancante

INTEGER :: i
LOGICAL :: linit

IF (PRESENT(ini)) THEN
  linit = ini
ELSE
  linit = .FALSE.
ENDIF

! Dimensioni principali
IF (PRESENT(nana)) THEN
  IF (nana >= 0) THEN
    IF (ASSOCIATED(this%ana)) DEALLOCATE(this%ana)
    ALLOCATE(this%ana(nana))
    IF (linit) THEN
      DO i = 1, nana
        CALL init(this%ana(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(ntime)) THEN
  IF (ntime >= 0) THEN
    IF (ASSOCIATED(this%time)) DEALLOCATE(this%time)
    ALLOCATE(this%time(ntime))
    IF (linit) THEN
      DO i = 1, ntime
        CALL init(this%time(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nlevel)) THEN
  IF (nlevel >= 0) THEN
    IF (ASSOCIATED(this%level)) DEALLOCATE(this%level)
    ALLOCATE(this%level(nlevel))
    IF (linit) THEN
      DO i = 1, nlevel
        CALL init(this%level(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(ntimerange)) THEN
  IF (ntimerange >= 0) THEN
    IF (ASSOCIATED(this%timerange)) DEALLOCATE(this%timerange)
    ALLOCATE(this%timerange(ntimerange))
    IF (linit) THEN
      DO i = 1, ntimerange
        CALL init(this%timerange(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nnetwork)) THEN
  IF (nnetwork >= 0) THEN
    IF (ASSOCIATED(this%network)) DEALLOCATE(this%network)
    ALLOCATE(this%network(nnetwork))
    IF (linit) THEN
      DO i = 1, nnetwork
        CALL init(this%network(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
! Dimensioni dei tipi delle variabili
CALL vol7d_varvect_alloc(this%anavar, nanavarr, nanavard, &
 nanavari, nanavarb, nanavarc, ini)
CALL vol7d_varvect_alloc(this%anaattr, nanaattrr, nanaattrd, &
 nanaattri, nanaattrb, nanaattrc, ini)
CALL vol7d_varvect_alloc(this%anavarattr, nanavarattrr, nanavarattrd, &
 nanavarattri, nanavarattrb, nanavarattrc, ini)
CALL vol7d_varvect_alloc(this%dativar, ndativarr, ndativard, &
 ndativari, ndativarb, ndativarc, ini)
CALL vol7d_varvect_alloc(this%datiattr, ndatiattrr, ndatiattrd, &
 ndatiattri, ndatiattrb, ndatiattrc, ini)
CALL vol7d_varvect_alloc(this%dativarattr, ndativarattrr, ndativarattrd, &
 ndativarattri, ndativarattrb, ndativarattrc, ini)

END SUBROUTINE vol7d_alloc


SUBROUTINE vol7d_check_alloc_ana(this, ini)
TYPE(vol7d),INTENT(inout) :: this
LOGICAL,INTENT(in),OPTIONAL :: ini

! Alloco i descrittori minimi per avere un volume di anagrafica
IF (.NOT. ASSOCIATED(this%ana)) CALL vol7d_alloc(this, nana=1, ini=ini)
IF (.NOT. ASSOCIATED(this%network)) CALL vol7d_alloc(this, nnetwork=1, ini=ini)

END SUBROUTINE vol7d_check_alloc_ana


SUBROUTINE vol7d_check_alloc_dati(this, ini)
TYPE(vol7d),INTENT(inout) :: this
LOGICAL,INTENT(in),OPTIONAL :: ini

! Alloco i descrittori minimi per avere un volume di dati
CALL vol7d_check_alloc_ana(this, ini)
IF (.NOT. ASSOCIATED(this%time)) CALL vol7d_alloc(this, ntime=1, ini=ini)
IF (.NOT. ASSOCIATED(this%level)) CALL vol7d_alloc(this, nlevel=1, ini=ini)
IF (.NOT. ASSOCIATED(this%timerange)) CALL vol7d_alloc(this, ntimerange=1, ini=ini)

END SUBROUTINE vol7d_check_alloc_dati


!> Metodo per allocare i volumi richiesti di variabili e attributi per
!! anagrafica e dati.
!! Se alcuni dei descrittori relativi alle dimensioni anagrafica,
!! livello verticale, tempo, intervallo temporale (timerange), rete non sono
!! stati richiesti preventivamente con la ::vol7d_alloc, essi vengono allocati
!! automaticamente da questo metodo
!! con estensione di default pari a 1 (non 0!), questo significa, ad esempio,
!! che se prevedo di avere soli dati superficiali, cioè ad un solo livello
!! verticale, o una sola rete di stazioni, non devo preoccuparmi di
!! specificare questa informazione.
!! Tra i 20 possibili volumi allocabili
!! ((variabili,attributi)*(anagrafica,dati)*(r,d,i,b,c)=20)
!! saranno allocati solo quelli per cui è stato precedentemente richiesto il
!! corrispondente descrittore variabili/attributi con la ::vol7d_alloc.
SUBROUTINE vol7d_alloc_vol(this, ini, inivol)
TYPE(vol7d),INTENT(inout) :: this !< oggetto di cui allocare i volumi
LOGICAL,INTENT(in),OPTIONAL :: ini !< se fornito e vale \c .TRUE., viene chiamato il costruttore, senza parametri opzionali, per ogni elemento di tutti i descrittori allocati
LOGICAL,INTENT(in),OPTIONAL :: inivol !< se fornito e vale \c .TRUE., i volumi allocati saranno inizializzati a valore mancante

LOGICAL :: linivol

IF (PRESENT(inivol)) THEN
  linivol = inivol
ELSE
  linivol = .TRUE.
ENDIF

! Anagrafica
IF (ASSOCIATED(this%anavar%r) .AND. .NOT.ASSOCIATED(this%volanar)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanar(SIZE(this%ana), SIZE(this%anavar%r), SIZE(this%network)))
  IF (linivol) this%volanar(:,:,:) = rmiss
ENDIF

IF (ASSOCIATED(this%anavar%d) .AND. .NOT.ASSOCIATED(this%volanad)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanad(SIZE(this%ana), SIZE(this%anavar%d), SIZE(this%network)))
  IF (linivol) this%volanad(:,:,:) = rdmiss
ENDIF

IF (ASSOCIATED(this%anavar%i) .AND. .NOT.ASSOCIATED(this%volanai)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanai(SIZE(this%ana), SIZE(this%anavar%i), SIZE(this%network)))
  IF (linivol) this%volanai(:,:,:) = imiss
ENDIF

IF (ASSOCIATED(this%anavar%b) .AND. .NOT.ASSOCIATED(this%volanab)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanab(SIZE(this%ana), SIZE(this%anavar%b), SIZE(this%network)))
  IF (linivol) this%volanab(:,:,:) = ibmiss
ENDIF

IF (ASSOCIATED(this%anavar%c) .AND. .NOT.ASSOCIATED(this%volanac)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanac(SIZE(this%ana), SIZE(this%anavar%c), SIZE(this%network)))
  IF (linivol) this%volanac(:,:,:) = cmiss
ENDIF

! Attributi dell'anagrafica
IF (ASSOCIATED(this%anaattr%r) .AND. ASSOCIATED(this%anavarattr%r) .AND. &
 .NOT.ASSOCIATED(this%volanaattrr)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanaattrr(SIZE(this%ana), SIZE(this%anavarattr%r), &
   SIZE(this%network), SIZE(this%anaattr%r)))
  IF (linivol) this%volanaattrr(:,:,:,:) = rmiss
ENDIF

IF (ASSOCIATED(this%anaattr%d) .AND. ASSOCIATED(this%anavarattr%d) .AND. &
 .NOT.ASSOCIATED(this%volanaattrd)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanaattrd(SIZE(this%ana), SIZE(this%anavarattr%d), &
   SIZE(this%network), SIZE(this%anaattr%d)))
  IF (linivol) this%volanaattrd(:,:,:,:) = rdmiss
ENDIF

IF (ASSOCIATED(this%anaattr%i) .AND. ASSOCIATED(this%anavarattr%i) .AND. &
 .NOT.ASSOCIATED(this%volanaattri)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanaattri(SIZE(this%ana), SIZE(this%anavarattr%i), &
   SIZE(this%network), SIZE(this%anaattr%i)))
  IF (linivol) this%volanaattri(:,:,:,:) = imiss
ENDIF

IF (ASSOCIATED(this%anaattr%b) .AND. ASSOCIATED(this%anavarattr%b) .AND. &
 .NOT.ASSOCIATED(this%volanaattrb)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanaattrb(SIZE(this%ana), SIZE(this%anavarattr%b), &
   SIZE(this%network), SIZE(this%anaattr%b)))
  IF (linivol) this%volanaattrb(:,:,:,:) = ibmiss
ENDIF

IF (ASSOCIATED(this%anaattr%c) .AND. ASSOCIATED(this%anavarattr%c) .AND. &
 .NOT.ASSOCIATED(this%volanaattrc)) THEN
  CALL vol7d_check_alloc_ana(this, ini)
  ALLOCATE(this%volanaattrc(SIZE(this%ana), SIZE(this%anavarattr%c), &
   SIZE(this%network), SIZE(this%anaattr%c)))
  IF (linivol) this%volanaattrc(:,:,:,:) = cmiss
ENDIF

! Dati
IF (ASSOCIATED(this%dativar%r) .AND. .NOT.ASSOCIATED(this%voldatir)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatir(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativar%r), SIZE(this%network)))
  IF (linivol) this%voldatir(:,:,:,:,:,:) = rmiss
ENDIF

IF (ASSOCIATED(this%dativar%d) .AND. .NOT.ASSOCIATED(this%voldatid)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatid(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativar%d), SIZE(this%network)))
  IF (linivol) this%voldatid(:,:,:,:,:,:) = rdmiss
ENDIF

IF (ASSOCIATED(this%dativar%i) .AND. .NOT.ASSOCIATED(this%voldatii)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatii(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativar%i), SIZE(this%network)))
  IF (linivol) this%voldatii(:,:,:,:,:,:) = imiss
ENDIF

IF (ASSOCIATED(this%dativar%b) .AND. .NOT.ASSOCIATED(this%voldatib)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatib(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativar%b), SIZE(this%network)))
  IF (linivol) this%voldatib(:,:,:,:,:,:) = ibmiss
ENDIF

IF (ASSOCIATED(this%dativar%c) .AND. .NOT.ASSOCIATED(this%voldatic)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatic(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativar%c), SIZE(this%network)))
  IF (linivol) this%voldatic(:,:,:,:,:,:) = cmiss
ENDIF

! Attributi dei dati
IF (ASSOCIATED(this%datiattr%r) .AND. ASSOCIATED(this%dativarattr%r) .AND. &
 .NOT.ASSOCIATED(this%voldatiattrr)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatiattrr(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativarattr%r), SIZE(this%network), &
   SIZE(this%datiattr%r)))
  IF (linivol) this%voldatiattrr(:,:,:,:,:,:,:) = rmiss
ENDIF

IF (ASSOCIATED(this%datiattr%d) .AND. ASSOCIATED(this%dativarattr%d) .AND. &
 .NOT.ASSOCIATED(this%voldatiattrd)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatiattrd(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativarattr%d), SIZE(this%network), &
   SIZE(this%datiattr%d)))
  IF (linivol) this%voldatiattrd(:,:,:,:,:,:,:) = rdmiss
ENDIF

IF (ASSOCIATED(this%datiattr%i) .AND. ASSOCIATED(this%dativarattr%i) .AND. &
 .NOT.ASSOCIATED(this%voldatiattri)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatiattri(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativarattr%i), SIZE(this%network), &
   SIZE(this%datiattr%i)))
  IF (linivol) this%voldatiattri(:,:,:,:,:,:,:) = imiss
ENDIF

IF (ASSOCIATED(this%datiattr%b) .AND. ASSOCIATED(this%dativarattr%b) .AND. &
 .NOT.ASSOCIATED(this%voldatiattrb)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatiattrb(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativarattr%b), SIZE(this%network), &
   SIZE(this%datiattr%b)))
  IF (linivol) this%voldatiattrb(:,:,:,:,:,:,:) = ibmiss
ENDIF

IF (ASSOCIATED(this%datiattr%c) .AND. ASSOCIATED(this%dativarattr%c) .AND. &
 .NOT.ASSOCIATED(this%voldatiattrc)) THEN
  CALL vol7d_check_alloc_dati(this, ini)
  ALLOCATE(this%voldatiattrc(SIZE(this%ana), SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%dativarattr%c), SIZE(this%network), &
   SIZE(this%datiattr%c)))
  IF (linivol) this%voldatiattrc(:,:,:,:,:,:,:) = cmiss
ENDIF

! Creo gli indici var-attr
CALL vol7d_set_attr_ind(this)

END SUBROUTINE vol7d_alloc_vol


!> Metodo per creare gli indici che associano le variabili aventi attributo
!! alle variabili nei relativi descrittori.
!! Ha senso chiamare questo metodo solo dopo che i descrittori delle variabili
!! e degli attributi desiderati sono stati allocati ed è stato assegnato un
!! valore ai relativi membri btable (vedi vol7d_var_class::vol7d_var), se
!! i descrittori non sono stati allocati o assegnati, il metodo non fa niente.
SUBROUTINE vol7d_set_attr_ind(this)
TYPE(vol7d),INTENT(inout) :: this !< oggetto in cui creare gli indici

INTEGER :: i

! real
IF (ASSOCIATED(this%dativar%r)) THEN
  IF (ASSOCIATED(this%dativarattr%r)) THEN
    DO i = 1, SIZE(this%dativar%r)
      this%dativar%r(i)%r = &
       firsttrue(this%dativar%r(i)%btable == this%dativarattr%r(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%d)) THEN
    DO i = 1, SIZE(this%dativar%r)
      this%dativar%r(i)%d = &
       firsttrue(this%dativar%r(i)%btable == this%dativarattr%d(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%i)) THEN
    DO i = 1, SIZE(this%dativar%r)
      this%dativar%r(i)%i = &
       firsttrue(this%dativar%r(i)%btable == this%dativarattr%i(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%b)) THEN
    DO i = 1, SIZE(this%dativar%r)
      this%dativar%r(i)%b = &
       firsttrue(this%dativar%r(i)%btable == this%dativarattr%b(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%c)) THEN
    DO i = 1, SIZE(this%dativar%r)
      this%dativar%r(i)%c = &
       firsttrue(this%dativar%r(i)%btable == this%dativarattr%c(:)%btable)
    ENDDO
  ENDIF
ENDIF
! double
IF (ASSOCIATED(this%dativar%d)) THEN
  IF (ASSOCIATED(this%dativarattr%r)) THEN
    DO i = 1, SIZE(this%dativar%d)
      this%dativar%d(i)%r = &
       firsttrue(this%dativar%d(i)%btable == this%dativarattr%r(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%d)) THEN
    DO i = 1, SIZE(this%dativar%d)
      this%dativar%d(i)%d = &
       firsttrue(this%dativar%d(i)%btable == this%dativarattr%d(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%i)) THEN
    DO i = 1, SIZE(this%dativar%d)
      this%dativar%d(i)%i = &
       firsttrue(this%dativar%d(i)%btable == this%dativarattr%i(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%b)) THEN
    DO i = 1, SIZE(this%dativar%d)
      this%dativar%d(i)%b = &
       firsttrue(this%dativar%d(i)%btable == this%dativarattr%b(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%c)) THEN
    DO i = 1, SIZE(this%dativar%d)
      this%dativar%d(i)%c = &
       firsttrue(this%dativar%d(i)%btable == this%dativarattr%c(:)%btable)
    ENDDO
  ENDIF
ENDIF
! integer
IF (ASSOCIATED(this%dativar%i)) THEN
  IF (ASSOCIATED(this%dativarattr%r)) THEN
    DO i = 1, SIZE(this%dativar%i)
      this%dativar%i(i)%r = &
       firsttrue(this%dativar%i(i)%btable == this%dativarattr%r(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%d)) THEN
    DO i = 1, SIZE(this%dativar%i)
      this%dativar%i(i)%d = &
       firsttrue(this%dativar%i(i)%btable == this%dativarattr%d(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%i)) THEN
    DO i = 1, SIZE(this%dativar%i)
      this%dativar%i(i)%i = &
       firsttrue(this%dativar%i(i)%btable == this%dativarattr%i(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%b)) THEN
    DO i = 1, SIZE(this%dativar%i)
      this%dativar%i(i)%b = &
       firsttrue(this%dativar%i(i)%btable == this%dativarattr%b(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%c)) THEN
    DO i = 1, SIZE(this%dativar%i)
      this%dativar%i(i)%c = &
       firsttrue(this%dativar%i(i)%btable == this%dativarattr%c(:)%btable)
    ENDDO
  ENDIF
ENDIF
! byte
IF (ASSOCIATED(this%dativar%b)) THEN
  IF (ASSOCIATED(this%dativarattr%r)) THEN
    DO i = 1, SIZE(this%dativar%b)
      this%dativar%b(i)%r = &
       firsttrue(this%dativar%b(i)%btable == this%dativarattr%r(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%d)) THEN
    DO i = 1, SIZE(this%dativar%b)
      this%dativar%b(i)%d = &
       firsttrue(this%dativar%b(i)%btable == this%dativarattr%d(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%i)) THEN
    DO i = 1, SIZE(this%dativar%b)
      this%dativar%b(i)%i = &
       firsttrue(this%dativar%b(i)%btable == this%dativarattr%i(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%b)) THEN
    DO i = 1, SIZE(this%dativar%b)
      this%dativar%b(i)%b = &
       firsttrue(this%dativar%b(i)%btable == this%dativarattr%b(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%c)) THEN
    DO i = 1, SIZE(this%dativar%b)
      this%dativar%b(i)%c = &
       firsttrue(this%dativar%b(i)%btable == this%dativarattr%c(:)%btable)
    ENDDO
  ENDIF
ENDIF
! character
IF (ASSOCIATED(this%dativar%c)) THEN
  IF (ASSOCIATED(this%dativarattr%r)) THEN
    DO i = 1, SIZE(this%dativar%c)
      this%dativar%c(i)%r = &
       firsttrue(this%dativar%c(i)%btable == this%dativarattr%r(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%d)) THEN
    DO i = 1, SIZE(this%dativar%c)
      this%dativar%c(i)%d = &
       firsttrue(this%dativar%c(i)%btable == this%dativarattr%d(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%i)) THEN
    DO i = 1, SIZE(this%dativar%c)
      this%dativar%c(i)%i = &
       firsttrue(this%dativar%c(i)%btable == this%dativarattr%i(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%b)) THEN
    DO i = 1, SIZE(this%dativar%c)
      this%dativar%c(i)%b = &
       firsttrue(this%dativar%c(i)%btable == this%dativarattr%b(:)%btable)
    ENDDO
  ENDIF

  IF (ASSOCIATED(this%dativarattr%c)) THEN
    DO i = 1, SIZE(this%dativar%c)
      this%dativar%c(i)%c = &
       firsttrue(this%dativar%c(i)%btable == this%dativarattr%c(:)%btable)
    ENDDO
  ENDIF
ENDIF

END SUBROUTINE vol7d_set_attr_ind


!> Metodo per fondere 2 oggetti vol7d.
!! Il secondo volume viene accodato al primo
!! e poi distrutto, si veda quindi la descrizione di ::vol7d_append.
SUBROUTINE vol7d_merge(this, that, sort)
TYPE(vol7d),INTENT(INOUT) :: this !< primo oggetto in ingresso, alla fine conterrà il risultato della fusione
TYPE(vol7d),INTENT(INOUT) :: that !< secondo oggetto in ingresso, alla fine sarà distrutto
LOGICAL,INTENT(IN),OPTIONAL :: sort !< se fornito e uguale a \c .TRUE., i descrittori che supportano un ordinamento (operatori > e/o <) risulteranno ordinati in ordine crescente nell'oggetto finale

! Accodo that a this e distruggo that
CALL vol7d_append(this, that, sort)
CALL delete(that)

END SUBROUTINE vol7d_merge


!> Metodo per accodare un oggetto vol7d ad un altro.
!! Si tratta di un metodo molto potente e versatile;
!! i descrittori delle dimensioni del volume finale conterranno i valori
!! dei corrispondenti descrittori del primo e del secondo volume
!! e i volumi di anagrafica e dati conterranno i valori dei due volumi
!! ai posti giusti, e valori mancanti per le nuove combinazioni che
!! eventualmente si verranno a creare.
!! Se i volumi multidimensionali di anagrafica e/o dati dei 2 oggetti
!! hanno un'intersezione non nulla, negli elementi comuni il volume finale
!! conterrà il corrispondente elemento del \b secondo volume.
!! Attenzione che, durante l'esecuzione del metodo, la memoria richiesta è
!! pari alla memoria complessiva occupata dai 2 volumi iniziali più
!! la memoria complessiva del volume finale, per cui, nel caso di volumi grandi,
!! ci potebbero essere problemi di esaurimento della memoria centrale.
!!
!! \todo nel caso di elementi comuni inserire la possibiità (opzionale per
!! non penalizzare le prestazioni quando ciò non serve) di effettuare una scelta
!! più ragionata dell'elemento da tenere, almeno controllando i dati mancanti
!! se non le flag di qualità
!!
!! \todo "rateizzare" l'allocazione dei volumi per ridurre l'occupazione di
!! memoria nel caso siano allocati contemporaneamente volumi di variabili e
!! di attributi o più volumi di tipi diversi
!!
!! \todo il parametro \a that è dichiarato \a INOUT perché la vol7d_alloc_vol
!! può modificarlo, bisognerebbe implementare una vol7d_check_vol che restituisca
!! errore anziché usare la vol7d_alloc_vol.
SUBROUTINE vol7d_append(this, that, sort)
TYPE(vol7d),INTENT(INOUT) :: this !< primo oggetto in ingresso, a cui sarà accodato il secondo
TYPE(vol7d),INTENT(INOUT) :: that !< secondo oggetto in ingresso, non viene modificato dal metodo
LOGICAL,INTENT(IN),OPTIONAL :: sort !< se fornito e uguale a \c .TRUE., i descrittori che supportano un ordinamento (operatori > e/o <) risulteranno ordinati in ordine crescente nell'oggetto finale

TYPE(vol7d) :: v7dtmp
LOGICAL :: lsort
INTEGER,POINTER :: remapt1(:), remapt2(:), remaptr1(:), remaptr2(:), &
 remapl1(:), remapl2(:), remapa1(:), remapa2(:), remapn1(:), remapn2(:)

! Completo l'allocazione per avere volumi a norma
CALL vol7d_alloc_vol(this)
CALL vol7d_alloc_vol(that)

CALL init(v7dtmp)
lsort = .FALSE.
IF (PRESENT(sort)) lsort = sort

! Calcolo le mappature tra volumi vecchi e volume nuovo
! I puntatori remap* vengono tutti o allocati o nullificati
CALL vol7d_remap2_datetime(this%time, that%time, v7dtmp%time, &
 lsort, remapt1, remapt2)
CALL vol7d_remap2_vol7d_timerange(this%timerange, that%timerange, &
 v7dtmp%timerange, lsort, remaptr1, remaptr2)
CALL vol7d_remap2_vol7d_level(this%level, that%level, v7dtmp%level, &
 lsort, remapl1, remapl2)
CALL vol7d_remap2_vol7d_ana(this%ana, that%ana, v7dtmp%ana, &
 .FALSE., remapa1, remapa2)
CALL vol7d_remap2_vol7d_network(this%network, that%network, v7dtmp%network, &
 .FALSE., remapn1, remapn2)

! Faccio la fusione fisica dei volumi
CALL vol7d_merge_finalr(this, that, v7dtmp, &
 remapa1, remapa2, remapt1, remapt2, remapl1, remapl2, &
 remaptr1, remaptr2, remapn1, remapn2)
CALL vol7d_merge_finald(this, that, v7dtmp, &
 remapa1, remapa2, remapt1, remapt2, remapl1, remapl2, &
 remaptr1, remaptr2, remapn1, remapn2)
CALL vol7d_merge_finali(this, that, v7dtmp, &
 remapa1, remapa2, remapt1, remapt2, remapl1, remapl2, &
 remaptr1, remaptr2, remapn1, remapn2)
CALL vol7d_merge_finalb(this, that, v7dtmp, &
 remapa1, remapa2, remapt1, remapt2, remapl1, remapl2, &
 remaptr1, remaptr2, remapn1, remapn2)
CALL vol7d_merge_finalc(this, that, v7dtmp, &
 remapa1, remapa2, remapt1, remapt2, remapl1, remapl2, &
 remaptr1, remaptr2, remapn1, remapn2)

! Dealloco i vettori di rimappatura
IF (ASSOCIATED(remapt1)) DEALLOCATE(remapt1)
IF (ASSOCIATED(remapt2)) DEALLOCATE(remapt2)
IF (ASSOCIATED(remaptr1)) DEALLOCATE(remaptr1)
IF (ASSOCIATED(remaptr2)) DEALLOCATE(remaptr2)
IF (ASSOCIATED(remapl1)) DEALLOCATE(remapl1)
IF (ASSOCIATED(remapl2)) DEALLOCATE(remapl2)
IF (ASSOCIATED(remapa1)) DEALLOCATE(remapa1)
IF (ASSOCIATED(remapa2)) DEALLOCATE(remapa2)
IF (ASSOCIATED(remapn1)) DEALLOCATE(remapn1)
IF (ASSOCIATED(remapn2)) DEALLOCATE(remapn2)

! Distruggo il vecchio volume e assegno il nuovo a this
CALL delete(this)
this = v7dtmp
! Ricreo gli indici var-attr
CALL vol7d_set_attr_ind(this)

END SUBROUTINE vol7d_append


!> Metodo per creare una copia completa e indipendente di un oggetto vol7d.
!! Questo metodo crea un duplicato di tutti i membri di un oggetto vol7d,
!! con la possibilità di rielaborarlo durante la copia.
!! Attenzione, il codice:
!! \code
!! USE vol7d_class
!! TYPE(vol7d) :: vol1, vol2
!! CALL init(vol1)
!! CALL init(vol2)
!! ... ! riempio vol1
!! vol2 = vol1
!! \endcode
!! fa una cosa diversa rispetto a:
!! \code
!! USE vol7d_class
!! TYPE(vol7d) :: vol1, vol2
!! CALL init(vol1)
!! CALL init(vol2)
!! ... ! riempio vol1
!! CALL vol7d_copy(vol1, vol2)
!! \endcode
!! nel primo caso, infatti, l'operatore di assegnazione copia solo i componenti
!! statici di \a vol1 nei corrispondenti elementi di \a vol2, mentre i componenti che
!! sono allocati dinamicamente (cioè quelli che in ::vol7d hanno l'attributo
!! \c POINTER, in pratica quasi tutti) non vengono duplicati, ma per essi vol2
!! conterrà un puntatore al corrispondente elemento a cui già punta vol1, e quindi 
!! eventuali cambiamenti al contenuto di uno dei due oggetti influenzerà il
!! contenuto dell'altro; nel secondo caso, invece, vol1 e vol2 sono, dopo la
!! vol7d_copy, 2 istanze
!! completamente indipendenti, ma uguali tra loro per contenuto, della classe
!! vol7d, e quindi hanno vita indipendente.
SUBROUTINE vol7d_copy(this, that, sort, unique, miss, ltime, ltimerange, &
 llevel, lana, lnetwork, &
 lanavarr, lanavard, lanavari, lanavarb, lanavarc, &
 lanaattrr, lanaattrd, lanaattri, lanaattrb, lanaattrc, &
 lanavarattrr, lanavarattrd, lanavarattri, lanavarattrb, lanavarattrc, &
 ldativarr, ldativard, ldativari, ldativarb, ldativarc, &
 ldatiattrr, ldatiattrd, ldatiattri, ldatiattrb, ldatiattrc, &
 ldativarattrr, ldativarattrd, ldativarattri, ldativarattrb, ldativarattrc)
TYPE(vol7d),INTENT(INOUT) :: this !< oggetto origine
TYPE(vol7d),INTENT(INOUT) :: that !< oggetto destinazione
LOGICAL,INTENT(IN),OPTIONAL :: sort !< se fornito e uguale a \c .TRUE., i descrittori che supportano un ordinamento (operatori > e/o <) risulteranno ordinati in ordine crescente nell'oggetto destinazione
LOGICAL,INTENT(IN),OPTIONAL :: unique !< se fornito e uguale a \c .TRUE., gli eventuali elementi duplicati nei descrittori dell'oggetto origine verranno collassati in un unico elemento (con eventuale perdita dei dati relativi agli elementi duplicati)
LOGICAL,INTENT(IN),OPTIONAL :: miss !< se fornito e uguale a \c .TRUE., gli eventuali elementi dei descrittori uguali al corrispondente valore mancante verranno eliminati dall'oggetto finale
!> se fornito, deve essere un vettore logico della stessa lunghezza di
!! this%time indicante quali elementi della dimensione \a time
!! mantenere (valori \c .TRUE.) e quali scartare (valori \c .FALSE.)
!! nel volume copiato; in alternativa può essere un vettore di
!! lunghezza 1, in tal caso, se \c .FALSE. , equivale a scartare tutti
!! gli elementi (utile principalmente per le variabili); è compatibile
!! col parametro \a miss
LOGICAL,INTENT(IN),OPTIONAL :: ltime(:)
LOGICAL,INTENT(IN),OPTIONAL :: ltimerange(:) !< come il precedente per la dimensione \a timerange
LOGICAL,INTENT(IN),OPTIONAL :: llevel(:) !< come il precedente per la dimensione \a level
LOGICAL,INTENT(IN),OPTIONAL :: lana(:) !< come il precedente per la dimensione \a ana
LOGICAL,INTENT(IN),OPTIONAL :: lnetwork(:) !< come il precedente per la dimensione \a network
!> come il precedente per tutte le possibili dimensioni variabile
LOGICAL,INTENT(in),OPTIONAL :: &
 lanavarr(:), lanavard(:), lanavari(:), lanavarb(:), lanavarc(:), &
 lanaattrr(:), lanaattrd(:), lanaattri(:), lanaattrb(:), lanaattrc(:), &
 lanavarattrr(:), lanavarattrd(:), lanavarattri(:), lanavarattrb(:), lanavarattrc(:), &
 ldativarr(:), ldativard(:), ldativari(:), ldativarb(:), ldativarc(:), &
 ldatiattrr(:), ldatiattrd(:), ldatiattri(:), ldatiattrb(:), ldatiattrc(:), &
 ldativarattrr(:), ldativarattrd(:), ldativarattri(:), ldativarattrb(:), ldativarattrc(:)

LOGICAL :: lsort, lunique, lmiss
INTEGER,POINTER :: remapt(:), remaptr(:), remapl(:), remapa(:), remapn(:)

! Completo l'allocazione per avere un volume a norma
CALL vol7d_alloc_vol(this)
CALL init(that)
IF (PRESENT(sort)) THEN
  lsort = sort
ELSE
  lsort = .FALSE.
ENDIF
IF (PRESENT(unique)) THEN
  lunique = unique
ELSE
  lunique = .TRUE.
ENDIF
IF (PRESENT(miss)) THEN
  lmiss = miss
ELSE
  lmiss = .FALSE.
ENDIF

! Calcolo le mappature tra volume vecchio e volume nuovo
! I puntatori remap* vengono tutti o allocati o nullificati
CALL vol7d_remap1_datetime(this%time, that%time, lsort, lunique, lmiss, &
 remapt, ltime)
CALL vol7d_remap1_vol7d_timerange(this%timerange, that%timerange, &
 lsort, lunique, lmiss, remaptr, ltimerange)
CALL vol7d_remap1_vol7d_level(this%level, that%level, lsort, lunique, lmiss, &
 remapl, llevel)
CALL vol7d_remap1_vol7d_ana(this%ana, that%ana, lsort, lunique, lmiss, &
 remapa, lana)
CALL vol7d_remap1_vol7d_network(this%network, that%network, &
 lsort, lunique, lmiss, remapn, lnetwork)

! lanavari, lanavarb, lanavarc, &
! lanaattri, lanaattrb, lanaattrc, &
! lanavarattri, lanavarattrb, lanavarattrc, &
! ldativari, ldativarb, ldativarc, &
! ldatiattri, ldatiattrb, ldatiattrc, &
! ldativarattri, ldativarattrb, ldativarattrc
! Faccio la riforma fisica dei volumi
CALL vol7d_reform_finalr(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss, &
 lanavarr, lanaattrr, lanavarattrr, ldativarr, ldatiattrr, ldativarattrr)
CALL vol7d_reform_finald(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss, &
 lanavard, lanaattrd, lanavarattrd, ldativard, ldatiattrd, ldativarattrd)
CALL vol7d_reform_finali(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss, &
 lanavari, lanaattri, lanavarattri, ldativari, ldatiattri, ldativarattri)
CALL vol7d_reform_finalb(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss, &
 lanavarb, lanaattrb, lanavarattrb, ldativarb, ldatiattrb, ldativarattrb)
CALL vol7d_reform_finalc(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss, &
 lanavarc, lanaattrc, lanavarattrc, ldativarc, ldatiattrc, ldativarattrc)

! Dealloco i vettori di rimappatura
IF (ASSOCIATED(remapt)) DEALLOCATE(remapt)
IF (ASSOCIATED(remaptr)) DEALLOCATE(remaptr)
IF (ASSOCIATED(remapl)) DEALLOCATE(remapl)
IF (ASSOCIATED(remapa)) DEALLOCATE(remapa)
IF (ASSOCIATED(remapn)) DEALLOCATE(remapn)

! Ricreo gli indici var-attr
CALL vol7d_set_attr_ind(that)

END SUBROUTINE vol7d_copy


!> Metodo per riformare in varie maniere un oggetto vol7d.
!! Equivale ad una copia (vedi ::vol7d_copy)
!! seguita dalla distruzione del volume iniziale e alla
!! sua riassegnazione al volume copiato. Ha senso se almeno uno dei parametri
!! \a sort, \a uniq o \a miss è fornito uguale a \c .TRUE., altrimenti
!! è solo una perdita di tempo.
!! Può essere utile, ad esempio, per eliminare stazioni
!! o istanti temporali indesiderati, basta assegnare il loro corrispondente
!! elemento del descrittore a valore mancante e chiamare vol7d_reform
!! con miss=.TRUE. .
SUBROUTINE vol7d_reform(this, sort, unique, miss, ltime, ltimerange, &
 llevel, lana, lnetwork, &
 lanavarr, lanavard, lanavari, lanavarb, lanavarc, &
 lanaattrr, lanaattrd, lanaattri, lanaattrb, lanaattrc, &
 lanavarattrr, lanavarattrd, lanavarattri, lanavarattrb, lanavarattrc, &
 ldativarr, ldativard, ldativari, ldativarb, ldativarc, &
 ldatiattrr, ldatiattrd, ldatiattri, ldatiattrb, ldatiattrc, &
 ldativarattrr, ldativarattrd, ldativarattri, ldativarattrb, ldativarattrc)
TYPE(vol7d),INTENT(INOUT) :: this !< oggetto da riformare
LOGICAL,INTENT(IN),OPTIONAL :: sort !< se fornito e uguale a \c .TRUE., i descrittori che supportano un ordinamento (operatori > e/o <) risulteranno ordinati in ordine crescente nell'oggetto riformato
LOGICAL,INTENT(IN),OPTIONAL :: unique !< se fornito e uguale a \c .TRUE., gli eventuali elementi duplicati nei descrittori dell'oggetto iniziale verranno collassati in un unico elemento (con eventuale perdita dei dati relativi agli elementi duplicati)
LOGICAL,INTENT(IN),OPTIONAL :: miss !< se fornito e uguale a \c .TRUE., gli eventuali elementi dei descrittori uguali al corrispondente valore mancante verranno eliminati dall'oggetto riformato
!> se fornito, deve essere un vettore logico della stessa lunghezza di
!! this%time indicante quali elementi della dimensione \a time
!! mantenere (valori \c .TRUE.) e quali scartare (valori \c .FALSE.)
!! nel volume copiato; in alternativa può essere un vettore di
!! lunghezza 1, in tal caso, se \c .FALSE. , equivale a scartare tutti
!! gli elementi (utile principalmente per le variabili); è compatibile
!! col parametro \a miss
LOGICAL,INTENT(IN),OPTIONAL :: ltime(:)
LOGICAL,INTENT(IN),OPTIONAL :: ltimerange(:) !< come il precedente per la dimensione \a timerange
LOGICAL,INTENT(IN),OPTIONAL :: llevel(:) !< come il precedente per la dimensione \a level
LOGICAL,INTENT(IN),OPTIONAL :: lana(:) !< come il precedente per la dimensione \a ana
LOGICAL,INTENT(IN),OPTIONAL :: lnetwork(:) !< come il precedente per la dimensione \a network
!> come il precedente per tutte le possibili dimensioni variabile
LOGICAL,INTENT(in),OPTIONAL :: &
 lanavarr(:), lanavard(:), lanavari(:), lanavarb(:), lanavarc(:), &
 lanaattrr(:), lanaattrd(:), lanaattri(:), lanaattrb(:), lanaattrc(:), &
 lanavarattrr(:), lanavarattrd(:), lanavarattri(:), lanavarattrb(:), lanavarattrc(:), &
 ldativarr(:), ldativard(:), ldativari(:), ldativarb(:), ldativarc(:), &
 ldatiattrr(:), ldatiattrd(:), ldatiattri(:), ldatiattrb(:), ldatiattrc(:), &
 ldativarattrr(:), ldativarattrd(:), ldativarattri(:), ldativarattrb(:), ldativarattrc(:)

TYPE(vol7d) :: v7dtmp

CALL vol7d_copy(this, v7dtmp, sort, unique, miss, ltime, ltimerange, &
 llevel, lana, lnetwork, &
 lanavarr, lanavard, lanavari, lanavarb, lanavarc, &
 lanaattrr, lanaattrd, lanaattri, lanaattrb, lanaattrc, &
 lanavarattrr, lanavarattrd, lanavarattri, lanavarattrb, lanavarattrc, &
 ldativarr, ldativard, ldativari, ldativarb, ldativarc, &
 ldatiattrr, ldatiattrd, ldatiattri, ldatiattrb, ldatiattrc, &
 ldativarattrr, ldativarattrd, ldativarattri, ldativarattrb, ldativarattrc)
! Distruggo il vecchio volume e assegno il nuovo a this
CALL delete(this)
this = v7dtmp

END SUBROUTINE vol7d_reform

!> Metodo per convertire i volumi di dati di un oggetto vol7d in dati
!! reali dove possibile. L'oggetto convertito è una copia completa
!! dell'originale che può essere quindi distrutto dopo la chiamata.
!! I dati di anagrafica al momento non sono convertiti.
!! Anche gli attributi di anagrafica e dati non sono toccati.
SUBROUTINE vol7d_convr(this, that)
TYPE(vol7d),INTENT(INOUT) :: this !< oggetto origine
TYPE(vol7d),INTENT(INOUT) :: that !< oggetto convertito
LOGICAL :: anaconv ! dovra` diventare un parametro
INTEGER :: i
LOGICAL :: fv(1)=(/.FALSE./), tv(1)=(/.TRUE./), acp(1), acn(1)
TYPE(vol7d) :: v7d_tmp

! richiede modifiche per convertirte i dati di anagrafica
! per ora sempre disabilitato
anaconv = .FALSE.
IF (anaconv) THEN
  acp=fv
  acn=tv
ELSE
  acp=tv
  acn=fv
ENDIF

! Volume con solo i dati reali e tutti gli attributi
! l'anagrafica e` copiata interamente se necessario
CALL vol7d_copy(this, that, &
 lanavarr=tv, lanavard=acp, lanavari=acp, lanavarb=acp, lanavarc=acp, &
 ldativarr=tv, ldativard=fv, ldativari=fv, ldativarb=fv, ldativarc=fv)


! Volume solo di dati double
CALL vol7d_copy(this, v7d_tmp, &
 lanavarr=fv, lanavard=acn, lanavari=fv, lanavarb=fv, lanavarc=fv, &
 lanaattrr=fv, lanaattrd=fv, lanaattri=fv, lanaattrb=fv, lanaattrc=fv, &
 lanavarattrr=fv, lanavarattrd=fv, lanavarattri=fv, lanavarattrb=fv, lanavarattrc=fv, &
 ldativarr=fv, ldativard=tv, ldativari=fv, ldativarb=fv, ldativarc=fv, &
 ldatiattrr=fv, ldatiattrd=fv, ldatiattri=fv, ldatiattrb=fv, ldatiattrc=fv, &
 ldativarattrr=fv, ldativarattrd=fv, ldativarattri=fv, ldativarattrb=fv, ldativarattrc=fv)

! converto a dati reali
IF (ASSOCIATED(v7d_tmp%dativar%d)) THEN ! .and. associated(v7d_tmp%voldatid) ?
! alloco i dati reali e vi trasferisco i double
  ALLOCATE(v7d_tmp%voldatir(SIZE(v7d_tmp%voldatid, 1), SIZE(v7d_tmp%voldatid, 2), &
  SIZE(v7d_tmp%voldatid, 3), SIZE(v7d_tmp%voldatid, 4), SIZE(v7d_tmp%voldatid, 5), &
  SIZE(v7d_tmp%voldatid, 6)))
  DO i = 1, SIZE(v7d_tmp%dativar%d)
    v7d_tmp%voldatir(:,:,:,:,i,:) = &
     realdat(v7d_tmp%voldatid(:,:,:,:,i,:), v7d_tmp%dativar%d(i))
  ENDDO
  DEALLOCATE(v7d_tmp%voldatid)
! trasferisco le variabili
  v7d_tmp%dativar%r => v7d_tmp%dativar%d
  NULLIFY(v7d_tmp%dativar%d)

! fondo con il volume definitivo
  CALL vol7d_merge(that, v7d_tmp)
ELSE
  CALL delete(v7d_tmp)
ENDIF


! Volume solo di dati interi
CALL vol7d_copy(this, v7d_tmp, &
 lanavarr=fv, lanavard=fv, lanavari=acn, lanavarb=fv, lanavarc=fv, &
 lanaattrr=fv, lanaattrd=fv, lanaattri=fv, lanaattrb=fv, lanaattrc=fv, &
 lanavarattrr=fv, lanavarattrd=fv, lanavarattri=fv, lanavarattrb=fv, lanavarattrc=fv, &
 ldativarr=fv, ldativard=fv, ldativari=tv, ldativarb=fv, ldativarc=fv, &
 ldatiattrr=fv, ldatiattrd=fv, ldatiattri=fv, ldatiattrb=fv, ldatiattrc=fv, &
 ldativarattrr=fv, ldativarattrd=fv, ldativarattri=fv, ldativarattrb=fv, ldativarattrc=fv)

! converto a dati reali
IF (ASSOCIATED(v7d_tmp%dativar%i)) THEN
! alloco i dati reali e vi trasferisco gli interi
  ALLOCATE(v7d_tmp%voldatir(SIZE(v7d_tmp%voldatii, 1), SIZE(v7d_tmp%voldatii, 2), &
  SIZE(v7d_tmp%voldatii, 3), SIZE(v7d_tmp%voldatii, 4), SIZE(v7d_tmp%voldatii, 5), &
  SIZE(v7d_tmp%voldatii, 6)))
  DO i = 1, SIZE(v7d_tmp%dativar%i)
    v7d_tmp%voldatir(:,:,:,:,i,:) = &
     realdat(v7d_tmp%voldatii(:,:,:,:,i,:), v7d_tmp%dativar%i(i))
  ENDDO
  DEALLOCATE(v7d_tmp%voldatii)
! trasferisco le variabili
  v7d_tmp%dativar%r => v7d_tmp%dativar%i
  NULLIFY(v7d_tmp%dativar%i)

! fondo con il volume definitivo
  CALL vol7d_merge(that, v7d_tmp)
ELSE
  CALL delete(v7d_tmp)
ENDIF


! Volume solo di dati byte
call vol7d_copy(this, v7d_tmp, &
 lanavarr=fv, lanavard=fv, lanavari=fv, lanavarb=acn, lanavarc=fv, &
 lanaattrr=fv, lanaattrd=fv, lanaattri=fv, lanaattrb=fv, lanaattrc=fv, &
 lanavarattrr=fv, lanavarattrd=fv, lanavarattri=fv, lanavarattrb=fv, lanavarattrc=fv, &
 ldativarr=fv, ldativard=fv, ldativari=fv, ldativarb=tv, ldativarc=fv, &
 ldatiattrr=fv, ldatiattrd=fv, ldatiattri=fv, ldatiattrb=fv, ldatiattrc=fv, &
 ldativarattrr=fv, ldativarattrd=fv, ldativarattri=fv, ldativarattrb=fv, ldativarattrc=fv)

! converto a dati reali
IF (ASSOCIATED(v7d_tmp%dativar%b)) THEN
! alloco i dati reali e vi trasferisco i byte
  ALLOCATE(v7d_tmp%voldatir(SIZE(v7d_tmp%voldatib, 1), SIZE(v7d_tmp%voldatib, 2), &
  SIZE(v7d_tmp%voldatib, 3), SIZE(v7d_tmp%voldatib, 4), SIZE(v7d_tmp%voldatib, 5), &
  SIZE(v7d_tmp%voldatib, 6)))
  DO i = 1, SIZE(v7d_tmp%dativar%b)
    v7d_tmp%voldatir(:,:,:,:,i,:) = &
     realdat(v7d_tmp%voldatib(:,:,:,:,i,:), v7d_tmp%dativar%b(i))
  ENDDO
  DEALLOCATE(v7d_tmp%voldatib)
! trasferisco le variabili
  v7d_tmp%dativar%r => v7d_tmp%dativar%b
  NULLIFY(v7d_tmp%dativar%b)

! fondo con il volume definitivo
  CALL vol7d_merge(that, v7d_tmp)
ELSE
  CALL delete(v7d_tmp)
ENDIF


! Volume solo di dati character
call vol7d_copy(this, v7d_tmp, &
 lanavarr=fv, lanavard=fv, lanavari=fv, lanavarb=fv, lanavarc=acn, &
 lanaattrr=fv, lanaattrd=fv, lanaattri=fv, lanaattrb=fv, lanaattrc=fv, &
 lanavarattrr=fv, lanavarattrd=fv, lanavarattri=fv, lanavarattrb=fv, lanavarattrc=fv, &
 ldativarr=fv, ldativard=fv, ldativari=fv, ldativarb=fv, ldativarc=tv, &
 ldatiattrr=fv, ldatiattrd=fv, ldatiattri=fv, ldatiattrb=fv, ldatiattrc=fv, &
 ldativarattrr=fv, ldativarattrd=fv, ldativarattri=fv, ldativarattrb=fv, ldativarattrc=fv)

! converto a dati reali
IF (ASSOCIATED(v7d_tmp%dativar%c)) THEN
! alloco i dati reali e vi trasferisco i character
  ALLOCATE(v7d_tmp%voldatir(SIZE(v7d_tmp%voldatic, 1), SIZE(v7d_tmp%voldatic, 2), &
  SIZE(v7d_tmp%voldatic, 3), SIZE(v7d_tmp%voldatic, 4), SIZE(v7d_tmp%voldatic, 5), &
  SIZE(v7d_tmp%voldatic, 6)))
  DO i = 1, SIZE(v7d_tmp%dativar%c)
    v7d_tmp%voldatir(:,:,:,:,i,:) = &
     realdat(v7d_tmp%voldatic(:,:,:,:,i,:), v7d_tmp%dativar%c(i))
  ENDDO
  DEALLOCATE(v7d_tmp%voldatic)
! trasferisco le variabili
  v7d_tmp%dativar%r => v7d_tmp%dativar%c
  NULLIFY(v7d_tmp%dativar%c)

! fondo con il volume definitivo
  CALL vol7d_merge(that, v7d_tmp)
ELSE
  CALL delete(v7d_tmp)
ENDIF



END SUBROUTINE vol7d_convr


!> Metodo per ottenere solo le differenze tra due oggetti vol7d.
!! Il primo volume viene confrontato col secondo; nel secondo volume ovunque 
!! i dati confrontati siano coincidenti viene impostato valore mancante.
SUBROUTINE vol7d_diff_only (this, that, data_only,ana)
TYPE(vol7d),INTENT(IN) :: this !< primo volume da confrontare
TYPE(vol7d),INTENT(OUT) :: that !< secondo volume da confrontare in cui eliminare i dati coincidenti
!INTEGER(kind=int_b), PARAMETER :: bmiss = ibmiss 
!INTEGER(kind=fp_d), PARAMETER :: dmiss = rdmiss 
logical , optional, intent(in) :: data_only !< attiva l'elaborazione dei soli dati e non dell'anagrafica (default: .false.)
logical , optional, intent(in) :: ana !< attiva l'elaborazione dell'anagrafica (coordinate e ident) (default: .false.)
logical  :: ldata_only,lana

IF (PRESENT(data_only)) THEN
  ldata_only = data_only
ELSE
  ldata_only = .FALSE.
ENDIF

IF (PRESENT(ana)) THEN
  lana = ana
ELSE
  lana = .FALSE.
ENDIF


#undef VOL7D_POLY_ARRAY
#define VOL7D_POLY_ARRAY voldati
#include "vol7d_class_diff.F90"
#undef VOL7D_POLY_ARRAY
#define VOL7D_POLY_ARRAY voldatiattr
#include "vol7d_class_diff.F90"
#undef VOL7D_POLY_ARRAY

if ( .not. ldata_only) then

#define VOL7D_POLY_ARRAY volana
#include "vol7d_class_diff.F90"
#undef VOL7D_POLY_ARRAY
#define VOL7D_POLY_ARRAY volanaattr
#include "vol7d_class_diff.F90"
#undef VOL7D_POLY_ARRAY

  if(lana)then
    where ( this%ana == that%ana )
      that%ana = vol7d_ana_miss
    end where
  end if

end if



END SUBROUTINE vol7d_diff_only



! Creo le routine da ripetere per i vari tipi di dati di v7d
! tramite un template e il preprocessore
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE REAL
#define VOL7D_POLY_TYPES r
#include "vol7d_class_type_templ.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE REAL(kind=fp_d)
#define VOL7D_POLY_TYPES d
#include "vol7d_class_type_templ.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE INTEGER
#define VOL7D_POLY_TYPES i
#include "vol7d_class_type_templ.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE INTEGER(kind=int_b)
#define VOL7D_POLY_TYPES b
#include "vol7d_class_type_templ.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE CHARACTER(len=vol7d_cdatalen)
#define VOL7D_POLY_TYPES c
#include "vol7d_class_type_templ.F90"

! Creo le routine da ripetere per i vari descrittori di dimensioni di v7d
! tramite un template e il preprocessore
#define VOL7D_SORT
#undef VOL7D_NO_ZERO_ALLOC
#undef VOL7D_POLY_TYPE
#define VOL7D_POLY_TYPE datetime
#include "vol7d_class_desc_templ.F90"
#undef VOL7D_POLY_TYPE
#define VOL7D_POLY_TYPE vol7d_timerange
#include "vol7d_class_desc_templ.F90"
#undef VOL7D_POLY_TYPE
#define VOL7D_POLY_TYPE vol7d_level
#include "vol7d_class_desc_templ.F90"
#undef VOL7D_SORT
#undef VOL7D_POLY_TYPE
#define VOL7D_POLY_TYPE vol7d_network
#include "vol7d_class_desc_templ.F90"
#undef VOL7D_POLY_TYPE
#define VOL7D_POLY_TYPE vol7d_ana
#include "vol7d_class_desc_templ.F90"
#define VOL7D_NO_ZERO_ALLOC
#undef VOL7D_POLY_TYPE
#define VOL7D_POLY_TYPE vol7d_var
#include "vol7d_class_desc_templ.F90"

!>\brief Scrittura su file di un volume Vol7d.
!! Scrittura su file unformatted di un intero volume Vol7d.
!! Il volume viene serializzato e scritto su file.
!! Il file puo' essere aperto opzionalmente dall'utente. Si possono passare
!! opzionalmente unità e nome del file altrimenti assegnati internamente a dei default; se assegnati internamente 
!! tali parametri saranno in output.
!! Se non viene fornito il nome file viene utilizzato un file di default con nome pari al nome del programma in 
!! esecuzione con postfisso ".v7d".
!! Come parametro opzionale c'è la description che insieme alla data corrente viene inserita nell'header del file.


subroutine vol7d_write_on_file (this,unit,description,filename,filename_auto)

TYPE(vol7d),INTENT(IN) :: this !< volume vol7d da scrivere 
integer,optional,intent(inout) :: unit !< unità su cui scrivere; se passata =0 ritorna il valore rielaborato (default =rielaborato internamente con getlun )
character(len=*),intent(in),optional :: filename !< nome del file su cui scrivere
character(len=*),intent(out),optional :: filename_auto !< nome del file generato se "filename" è omesso
character(len=*),INTENT(IN),optional :: description !< descrizione del volume

integer :: lunit
character(len=254) :: ldescription,arg,lfilename
integer :: nana, ntime, ntimerange, nlevel, nnetwork, &
 ndativarr, ndativari, ndativarb, ndativard, ndativarc,&
 ndatiattrr, ndatiattri, ndatiattrb, ndatiattrd, ndatiattrc,&
 ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc,&
 nanavarr, nanavari, nanavarb, nanavard, nanavarc,&
 nanaattrr, nanaattri, nanaattrb, nanaattrd, nanaattrc,&
 nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc
!integer :: im,id,iy
integer :: tarray(8)
logical :: opened,exist

 nana=0
 ntime=0
 ntimerange=0
 nlevel=0
 nnetwork=0
 ndativarr=0
 ndativari=0
 ndativarb=0
 ndativard=0
 ndativarc=0
 ndatiattrr=0
 ndatiattri=0
 ndatiattrb=0
 ndatiattrd=0
 ndatiattrc=0
 ndativarattrr=0
 ndativarattri=0
 ndativarattrb=0
 ndativarattrd=0
 ndativarattrc=0
 nanavarr=0
 nanavari=0
 nanavarb=0
 nanavard=0
 nanavarc=0
 nanaattrr=0
 nanaattri=0
 nanaattrb=0
 nanaattrd=0
 nanaattrc=0
 nanavarattrr=0
 nanavarattri=0
 nanavarattrb=0
 nanavarattrd=0
 nanavarattrc=0


!call idate(im,id,iy)
call date_and_time(values=tarray)
call getarg(0,arg)

if (present(description))then
  ldescription=description
else
  ldescription="Vol7d generated by: "//trim(arg)
end if

if (.not. present(unit))then
  lunit=getunit()
else
  if (unit==0)then
    lunit=getunit()
    unit=lunit
  else
    lunit=unit
  end if
end if

lfilename=trim(arg)//".v7d"
if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if

if (present(filename_auto))filename_auto=lfilename


inquire(unit=lunit,opened=opened)
if (.not. opened) then 
  inquire(file=lfilename,EXIST=exist)
  if (exist) CALL raise_error('file exist; cannot open new file')
  if (.not.exist) open (unit=lunit,file=lfilename,form="UNFORMATTED")
  print *, "opened: ",lfilename
end if

if (associated(this%ana)) nana=size(this%ana)
if (associated(this%time)) ntime=size(this%time)
if (associated(this%timerange)) ntimerange=size(this%timerange)
if (associated(this%level)) nlevel=size(this%level)
if (associated(this%network)) nnetwork=size(this%network)

if (associated(this%dativar%r)) ndativarr=size(this%dativar%r)
if (associated(this%dativar%i)) ndativari=size(this%dativar%i)
if (associated(this%dativar%b)) ndativarb=size(this%dativar%b)
if (associated(this%dativar%d)) ndativard=size(this%dativar%d)
if (associated(this%dativar%c)) ndativarc=size(this%dativar%c)

if (associated(this%datiattr%r)) ndatiattrr=size(this%datiattr%r)
if (associated(this%datiattr%i)) ndatiattri=size(this%datiattr%i)
if (associated(this%datiattr%b)) ndatiattrb=size(this%datiattr%b)
if (associated(this%datiattr%d)) ndatiattrd=size(this%datiattr%d)
if (associated(this%datiattr%c)) ndatiattrc=size(this%datiattr%c)

if (associated(this%dativarattr%r)) ndativarattrr=size(this%dativarattr%r)
if (associated(this%dativarattr%i)) ndativarattri=size(this%dativarattr%i)
if (associated(this%dativarattr%b)) ndativarattrb=size(this%dativarattr%b)
if (associated(this%dativarattr%d)) ndativarattrd=size(this%dativarattr%d)
if (associated(this%dativarattr%c)) ndativarattrc=size(this%dativarattr%c)
 
if (associated(this%anavar%r)) nanavarr=size(this%anavar%r)
if (associated(this%anavar%i)) nanavari=size(this%anavar%i)
if (associated(this%anavar%b)) nanavarb=size(this%anavar%b)
if (associated(this%anavar%d)) nanavard=size(this%anavar%d)
if (associated(this%anavar%c)) nanavarc=size(this%anavar%c)

if (associated(this%anaattr%r)) nanaattrr=size(this%anaattr%r)
if (associated(this%anaattr%i)) nanaattri=size(this%anaattr%i)
if (associated(this%anaattr%b)) nanaattrb=size(this%anaattr%b)
if (associated(this%anaattr%d)) nanaattrd=size(this%anaattr%d)
if (associated(this%anaattr%c)) nanaattrc=size(this%anaattr%c)

if (associated(this%anavarattr%r)) nanavarattrr=size(this%anavarattr%r)
if (associated(this%anavarattr%i)) nanavarattri=size(this%anavarattr%i)
if (associated(this%anavarattr%b)) nanavarattrb=size(this%anavarattr%b)
if (associated(this%anavarattr%d)) nanavarattrd=size(this%anavarattr%d)
if (associated(this%anavarattr%c)) nanavarattrc=size(this%anavarattr%c)

write(unit=lunit)ldescription
write(unit=lunit)tarray

write(unit=lunit)&
 nana, ntime, ntimerange, nlevel, nnetwork, &
 ndativarr, ndativari, ndativarb, ndativard, ndativarc,&
 ndatiattrr, ndatiattri, ndatiattrb, ndatiattrd, ndatiattrc,&
 ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc,&
 nanavarr, nanavari, nanavarb, nanavard, nanavarc,&
 nanaattrr, nanaattri, nanaattrb, nanaattrd, nanaattrc,&
 nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc


!write(unit=lunit)this


!! prime 5 dimensioni
if (associated(this%ana))       call write_unit(this%ana, lunit)
if (associated(this%time))      call write_unit(this%time, lunit)
if (associated(this%level))     write(unit=lunit)this%level
if (associated(this%timerange)) write(unit=lunit)this%timerange
if (associated(this%network))   write(unit=lunit)this%network

  !! 6a dimensione: variabile dell'anagrafica e dei dati
  !! con relativi attributi e in 5 tipi diversi

if (associated(this%anavar%r))      write(unit=lunit)this%anavar%r    
if (associated(this%anavar%i))      write(unit=lunit)this%anavar%i    
if (associated(this%anavar%b))      write(unit=lunit)this%anavar%b    
if (associated(this%anavar%d))      write(unit=lunit)this%anavar%d    
if (associated(this%anavar%c))      write(unit=lunit)this%anavar%c    

if (associated(this%anaattr%r))     write(unit=lunit)this%anaattr%r
if (associated(this%anaattr%i))     write(unit=lunit)this%anaattr%i
if (associated(this%anaattr%b))     write(unit=lunit)this%anaattr%b
if (associated(this%anaattr%d))     write(unit=lunit)this%anaattr%d
if (associated(this%anaattr%c))     write(unit=lunit)this%anaattr%c

if (associated(this%anavarattr%r))  write(unit=lunit)this%anavarattr%r
if (associated(this%anavarattr%i))  write(unit=lunit)this%anavarattr%i
if (associated(this%anavarattr%b))  write(unit=lunit)this%anavarattr%b
if (associated(this%anavarattr%d))  write(unit=lunit)this%anavarattr%d
if (associated(this%anavarattr%c))  write(unit=lunit)this%anavarattr%c

if (associated(this%dativar%r))     write(unit=lunit)this%dativar%r
if (associated(this%dativar%i))     write(unit=lunit)this%dativar%i
if (associated(this%dativar%b))     write(unit=lunit)this%dativar%b
if (associated(this%dativar%d))     write(unit=lunit)this%dativar%d
if (associated(this%dativar%c))     write(unit=lunit)this%dativar%c

if (associated(this%datiattr%r))    write(unit=lunit)this%datiattr%r
if (associated(this%datiattr%i))    write(unit=lunit)this%datiattr%i
if (associated(this%datiattr%b))    write(unit=lunit)this%datiattr%b
if (associated(this%datiattr%d))    write(unit=lunit)this%datiattr%d
if (associated(this%datiattr%c))    write(unit=lunit)this%datiattr%c

if (associated(this%dativarattr%r)) write(unit=lunit)this%dativarattr%r
if (associated(this%dativarattr%i)) write(unit=lunit)this%dativarattr%i
if (associated(this%dativarattr%b)) write(unit=lunit)this%dativarattr%b
if (associated(this%dativarattr%d)) write(unit=lunit)this%dativarattr%d
if (associated(this%dativarattr%c)) write(unit=lunit)this%dativarattr%c

!! Volumi di valori e attributi per anagrafica e dati

if (associated(this%volanar))      write(unit=lunit)this%volanar
if (associated(this%volanaattrr))  write(unit=lunit)this%volanaattrr
if (associated(this%voldatir))     write(unit=lunit)this%voldatir
if (associated(this%voldatiattrr)) write(unit=lunit)this%voldatiattrr

if (associated(this%volanai))      write(unit=lunit)this%volanai
if (associated(this%volanaattri))  write(unit=lunit)this%volanaattri
if (associated(this%voldatii))     write(unit=lunit)this%voldatii
if (associated(this%voldatiattri)) write(unit=lunit)this%voldatiattri

if (associated(this%volanab))      write(unit=lunit)this%volanab
if (associated(this%volanaattrb))  write(unit=lunit)this%volanaattrb
if (associated(this%voldatib))     write(unit=lunit)this%voldatib
if (associated(this%voldatiattrb)) write(unit=lunit)this%voldatiattrb

if (associated(this%volanad))      write(unit=lunit)this%volanad
if (associated(this%volanaattrd))  write(unit=lunit)this%volanaattrd
if (associated(this%voldatid))     write(unit=lunit)this%voldatid
if (associated(this%voldatiattrd)) write(unit=lunit)this%voldatiattrd

if (associated(this%volanac))      write(unit=lunit)this%volanac
if (associated(this%volanaattrc))  write(unit=lunit)this%volanaattrc
if (associated(this%voldatic))     write(unit=lunit)this%voldatic
if (associated(this%voldatiattrc)) write(unit=lunit)this%voldatiattrc

if (.not. present(unit)) close(unit=lunit)

end subroutine vol7d_write_on_file


!>\brief Lettura da file di un volume Vol7d.
!! Lettura da file unformatted di un intero volume Vol7d.
!! Questa subroutine comprende vol7d_alloc e vol7d_alloc_vol.
!! Il file puo' essere aperto opzionalmente dall'utente. Si possono passare
!! opzionalmente unità e nome del file altrimenti assegnati internamente a dei default; se assegnati internamente 
!! tali parametri saranno in output.


subroutine vol7d_read_from_file (this,unit,filename,description,tarray,filename_auto)

TYPE(vol7d),INTENT(OUT) :: this !< Volume vol7d da leggere
integer,intent(inout),optional :: unit !< unità su cui è stato aperto un file; se =0 rielaborato internamente (default = elaborato internamente con getunit)
character(len=*),INTENT(in),optional :: filename !< nome del file eventualmente da aprire (default = (nome dell'eseguibile)//.v7d )
character(len=*),intent(out),optional :: filename_auto !< nome del file generato se "filename" è omesso
character(len=*),INTENT(out),optional :: description !< descrizione del volume letto
integer,intent(out),optional :: tarray(8) !< vettore come definito da "date_and_time" della data di scrittura del volume


integer :: nana, ntime, ntimerange, nlevel, nnetwork, &
 ndativarr, ndativari, ndativarb, ndativard, ndativarc,&
 ndatiattrr, ndatiattri, ndatiattrb, ndatiattrd, ndatiattrc,&
 ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc,&
 nanavarr, nanavari, nanavarb, nanavard, nanavarc,&
 nanaattrr, nanaattri, nanaattrb, nanaattrd, nanaattrc,&
 nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc

character(len=254) :: ldescription,lfilename,arg
integer :: ltarray(8),lunit
logical :: opened,exist


call getarg(0,arg)

if (.not. present(unit))then
  lunit=getunit()
else
  if (unit==0)then
    lunit=getunit()
    unit=lunit
  else
    lunit=unit
  end if
end if

lfilename=trim(arg)//".v7d"
if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if

if (present(filename_auto))filename_auto=lfilename


inquire(unit=lunit,opened=opened)
if (.not. opened) then 
  inquire(file=lfilename,EXIST=exist)
  if (.not. exist) CALL raise_error('file do not exist; cannot open file')
  if (exist) open (unit=lunit,file=lfilename,form="UNFORMATTED")
  print *, "opened: ",lfilename
end if


read(unit=lunit)ldescription
read(unit=lunit)ltarray

print *,"Info: reading vol7d from file"
print *,"Info: description: ",trim(ldescription)
print *,"Info: written on ",ltarray

if (present(description))description=ldescription
if (present(tarray))tarray=ltarray

read(unit=lunit)&
 nana, ntime, ntimerange, nlevel, nnetwork, &
 ndativarr, ndativari, ndativarb, ndativard, ndativarc,&
 ndatiattrr, ndatiattri, ndatiattrb, ndatiattrd, ndatiattrc,&
 ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc,&
 nanavarr, nanavari, nanavarb, nanavard, nanavarc,&
 nanaattrr, nanaattri, nanaattrb, nanaattrd, nanaattrc,&
 nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc

call vol7d_alloc (this, &
 nana=nana, ntime=ntime, ntimerange=ntimerange, nlevel=nlevel, nnetwork=nnetwork,&
 ndativarr=ndativarr, ndativari=ndativari, ndativarb=ndativarb,&
 ndativard=ndativard, ndativarc=ndativarc,&
 ndatiattrr=ndatiattrr, ndatiattri=ndatiattri, ndatiattrb=ndatiattrb,&
 ndatiattrd=ndatiattrd, ndatiattrc=ndatiattrc,&
 ndativarattrr=ndativarattrr, ndativarattri=ndativarattri, ndativarattrb=ndativarattrb, &
 ndativarattrd=ndativarattrd, ndativarattrc=ndativarattrc,&
 nanavarr=nanavarr, nanavari=nanavari, nanavarb=nanavarb, &
 nanavard=nanavard, nanavarc=nanavarc,&
 nanaattrr=nanaattrr, nanaattri=nanaattri, nanaattrb=nanaattrb,&
 nanaattrd=nanaattrd, nanaattrc=nanaattrc,&
 nanavarattrr=nanavarattrr, nanavarattri=nanavarattri, nanavarattrb=nanavarattrb, &
 nanavarattrd=nanavarattrd, nanavarattrc=nanavarattrc)

call vol7d_alloc_vol (this)


if (associated(this%ana))       call read_unit(this%ana, lunit)
if (associated(this%time))      call read_unit(this%time, lunit)
if (associated(this%level))     read(unit=lunit)this%level
if (associated(this%timerange)) read(unit=lunit)this%timerange
if (associated(this%network))   read(unit=lunit)this%network

if (associated(this%anavar%r))      read(unit=lunit)this%anavar%r    
if (associated(this%anavar%i))      read(unit=lunit)this%anavar%i    
if (associated(this%anavar%b))      read(unit=lunit)this%anavar%b    
if (associated(this%anavar%d))      read(unit=lunit)this%anavar%d    
if (associated(this%anavar%c))      read(unit=lunit)this%anavar%c    

if (associated(this%anaattr%r))     read(unit=lunit)this%anaattr%r
if (associated(this%anaattr%i))     read(unit=lunit)this%anaattr%i
if (associated(this%anaattr%b))     read(unit=lunit)this%anaattr%b
if (associated(this%anaattr%d))     read(unit=lunit)this%anaattr%d
if (associated(this%anaattr%c))     read(unit=lunit)this%anaattr%c

if (associated(this%anavarattr%r))  read(unit=lunit)this%anavarattr%r
if (associated(this%anavarattr%i))  read(unit=lunit)this%anavarattr%i
if (associated(this%anavarattr%b))  read(unit=lunit)this%anavarattr%b
if (associated(this%anavarattr%d))  read(unit=lunit)this%anavarattr%d
if (associated(this%anavarattr%c))  read(unit=lunit)this%anavarattr%c

if (associated(this%dativar%r))     read(unit=lunit)this%dativar%r
if (associated(this%dativar%i))     read(unit=lunit)this%dativar%i
if (associated(this%dativar%b))     read(unit=lunit)this%dativar%b
if (associated(this%dativar%d))     read(unit=lunit)this%dativar%d
if (associated(this%dativar%c))     read(unit=lunit)this%dativar%c

if (associated(this%datiattr%r))    read(unit=lunit)this%datiattr%r
if (associated(this%datiattr%i))    read(unit=lunit)this%datiattr%i
if (associated(this%datiattr%b))    read(unit=lunit)this%datiattr%b
if (associated(this%datiattr%d))    read(unit=lunit)this%datiattr%d
if (associated(this%datiattr%c))    read(unit=lunit)this%datiattr%c

if (associated(this%dativarattr%r)) read(unit=lunit)this%dativarattr%r
if (associated(this%dativarattr%i)) read(unit=lunit)this%dativarattr%i
if (associated(this%dativarattr%b)) read(unit=lunit)this%dativarattr%b
if (associated(this%dativarattr%d)) read(unit=lunit)this%dativarattr%d
if (associated(this%dativarattr%c)) read(unit=lunit)this%dativarattr%c

!! Volumi di valori e attributi per anagrafica e dati

! if (associated(this%volanar))print*,"leggo volanar"
! if (associated(this%volanaattrr))print*,"leggo volanaattrr"
! if (associated(this%voldatir))print*,"leggo voldatir"
! if (associated(this%voldatiattrr))print*,"leggo voldatiattrr"

if (associated(this%volanar))      read(unit=lunit)this%volanar
if (associated(this%volanaattrr))  read(unit=lunit)this%volanaattrr
if (associated(this%voldatir))     read(unit=lunit)this%voldatir
if (associated(this%voldatiattrr)) read(unit=lunit)this%voldatiattrr

if (associated(this%volanai))      read(unit=lunit)this%volanai
if (associated(this%volanaattri))  read(unit=lunit)this%volanaattri
if (associated(this%voldatii))     read(unit=lunit)this%voldatii
if (associated(this%voldatiattri)) read(unit=lunit)this%voldatiattri

if (associated(this%volanab))      read(unit=lunit)this%volanab
if (associated(this%volanaattrb))  read(unit=lunit)this%volanaattrb
if (associated(this%voldatib))     read(unit=lunit)this%voldatib
if (associated(this%voldatiattrb)) read(unit=lunit)this%voldatiattrb

if (associated(this%volanad))      read(unit=lunit)this%volanad
if (associated(this%volanaattrd))  read(unit=lunit)this%volanaattrd
if (associated(this%voldatid))     read(unit=lunit)this%voldatid
if (associated(this%voldatiattrd)) read(unit=lunit)this%voldatiattrd

if (associated(this%volanac))      read(unit=lunit)this%volanac
if (associated(this%volanaattrc))  read(unit=lunit)this%volanaattrc
if (associated(this%voldatic))     read(unit=lunit)this%voldatic
if (associated(this%voldatiattrc)) read(unit=lunit)this%voldatiattrc

if (.not. present(unit)) close(unit=lunit)

end subroutine vol7d_read_from_file



! to double precision

elemental doubleprecision function doubledatd(voldat,var)

doubleprecision,intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  doubledatd=voldat
else
  doubledatd=dmiss
end if

end function doubledatd


elemental doubleprecision function doubledatr(voldat,var)

real,intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  doubledatr=voldat
else
  doubledatr=dmiss
end if

end function doubledatr


elemental doubleprecision function doubledati(voldat,var)

integer,intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  doubledati=dble(voldat)/10d0**var%scalefactor
else
  doubledati=dmiss
end if

end function doubledati


elemental doubleprecision function doubledatb(voldat,var)

integer(kind=int_b),intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  doubledatb=dble(voldat)/10d0**var%scalefactor
else
  doubledatb=dmiss
end if

end function doubledatb



elemental doubleprecision function doubledatc(voldat,var)

CHARACTER(len=vol7d_cdatalen),intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  read (voldat,*)doubledatc
  doubledatc=doubledatc/10d0**var%scalefactor
else
  doubledatc=dmiss
end if

end function doubledatc


!!$!esempio senza elemental
!!$function doubledatc(voldat,var,double)
!!$
!!$doubleprecision :: doubledatc(size(voldat))
!!$CHARACTER(len=vol7d_cdatalen),intent(in) :: voldat(:)
!!$type(vol7d_var),intent(in) :: var
!!$doubleprecision,intent(in) :: double
!!$
!!$integer :: i
!!$
!!$do i =1 ,size(voldat)
!!$  read (voldat(i),*)doubledatc(i)
!!$end do
!!$
!!$doubledatc=doubledatc/10d0**var%scalefactor
!!$
!!$end function doubledatc


! to real

elemental real function realdatd(voldat,var)

doubleprecision,intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  realdatd=voldat
else
  realdatd=rmiss
end if

end function realdatd


elemental real function realdatr(voldat,var)

real,intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  realdatr=voldat
else
  realdatr=rmiss
end if

end function realdatr


elemental real function realdati(voldat,var)

integer,intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  realdati=float(voldat)/10.**var%scalefactor
else
  realdati=rmiss
end if

end function realdati


elemental real function realdatb(voldat,var)

integer(kind=int_b),intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  realdatb=float(voldat)/10.**var%scalefactor
else
  realdatb=rmiss
end if

end function realdatb



elemental real function realdatc(voldat,var)

CHARACTER(len=vol7d_cdatalen),intent(in) :: voldat
type(vol7d_var),intent(in) :: var

if (c_e(voldat))then
  read (voldat,*)realdatc
  realdatc=realdatc/10.**var%scalefactor
else
  realdatc=rmiss
end if

end function realdatc




!!$elemental INTEGER(kind=int_b) function doubledatb(voldat,var,double)
!!$
!!$real,intent(in) :: voldat
!!$type(vol7d_var),intent(in) :: var
!!$integer(kind=int_b),intent(in) :: byte
!!$
!!$doubledatb=voldat*10.**var%scalefactor
!!$
!!$end function doubledatb
!!$
!!$
!!$
!!$elemental CHARACTER(len=vol7d_cdatalen) function doubledatc(voldat,var,double)
!!$
!!$real,intent(in) :: voldat
!!$type(vol7d_var),intent(in) :: var
!!$CHARACTER(len=vol7d_cdatalen),intent(in) :: char
!!$
!!$write (doubledatc,'(i20)')voldat*10.**var%scalefactor
!!$
!!$end function doubledatc





END MODULE vol7d_class

!>\example esempio_qc_convert.f90
!!\brief Programma esempio semplice per la scrittura su file di un volume vol7d 
!!
!!Programma che scrive su file  un volume vol7d letto da una serie di file ASCII.
!!Questo programma scrive i dati del clima che poi verranno letti da modqccli
