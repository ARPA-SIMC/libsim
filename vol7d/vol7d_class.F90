MODULE vol7d_class
USE kinds
USE datetime_class
USE vol7d_utilities
USE vol7d_ana_class
USE vol7d_timerange_class
USE vol7d_level_class
USE vol7d_network_class
USE vol7d_varvect_class
IMPLICIT NONE

!! Le dimensioni sono queste:
!! datiana(ana,anavar,network)
!! attrdatiana(ana,attranavar,network,attr)
!! dati(ana,time,level,timerange,dativar,network)
!! attrdati(ana,time,level,timerange,attrdativar,network,attr)

INTEGER, PARAMETER :: vol7d_maxdim_a = 3, vol7d_maxdim_aa = 4, &
 vol7d_maxdim_d = 6, vol7d_maxdim_ad = 7, &
 vol7d_ana_a = 1, vol7d_var_a = 2, vol7d_network_a = 3, &
 vol7d_attr_a = 4, &
 vol7d_ana_d = 1, vol7d_time_d = 2,  vol7d_level_d = 3, &
 vol7d_timerange_d = 4, vol7d_var_d = 5, vol7d_network_d = 6, &
 vol7d_attr_d = 7, &
 vol7d_cdatalen=20

TYPE vol7d_varmap
  INTEGER :: r, d, i, b, c
END TYPE vol7d_varmap

TYPE vol7d
  !! prime 5 dimensioni
  TYPE(vol7d_ana),POINTER :: ana(:)
  TYPE(datetime),POINTER :: time(:)
  TYPE(vol7d_level),POINTER :: level(:)
  TYPE(vol7d_timerange),POINTER :: timerange(:)
  TYPE(vol7d_network),POINTER :: network(:)
  !! 6a dimensione: variabile dell'anagrafica e dei dati
  !! con relativi attributi e in 5 tipi diversi
  TYPE(vol7d_varvect) :: &
   anavar,     & ! Variabili dell'anagrafica
   anaattr,    & ! Attributi delle variabili dell'anagrafica
   anavarattr, & ! Variabili dell'anagrafica che hanno tali attributi
   dativar,    & ! Variabili dei dati
   datiattr,   & ! Attributi delle variabili dei dati
   dativarattr   ! Variabili dei dati che hanno tali attributi

  !! Volumi di valori e attributi per anagrafica e dati
  REAL,POINTER :: volanar(:,:,:)
  REAL(kind=fp_d),POINTER :: volanad(:,:,:)
  INTEGER,POINTER :: volanai(:,:,:)
  INTEGER(kind=int_b),POINTER :: volanab(:,:,:)
  CHARACTER(len=vol7d_cdatalen),POINTER :: volanac(:,:,:)

  REAL,POINTER :: volanaattrr(:,:,:,:)
  REAL(kind=fp_d),POINTER :: volanaattrd(:,:,:,:)
  INTEGER,POINTER :: volanaattri(:,:,:,:)
  INTEGER(kind=int_b),POINTER :: volanaattrb(:,:,:,:)
  CHARACTER(len=vol7d_cdatalen),POINTER :: volanaattrc(:,:,:,:)

  REAL,POINTER :: voldatir(:,:,:,:,:,:) ! sono i dati
  REAL(kind=fp_d),POINTER :: voldatid(:,:,:,:,:,:)
  INTEGER,POINTER :: voldatii(:,:,:,:,:,:)
  INTEGER(kind=int_b),POINTER :: voldatib(:,:,:,:,:,:)
  CHARACTER(len=vol7d_cdatalen),POINTER :: voldatic(:,:,:,:,:,:)

  REAL,POINTER :: voldatiattrr(:,:,:,:,:,:,:)
  REAL(kind=fp_d),POINTER :: voldatiattrd(:,:,:,:,:,:,:)
  INTEGER,POINTER :: voldatiattri(:,:,:,:,:,:,:)
  INTEGER(kind=int_b),POINTER :: voldatiattrb(:,:,:,:,:,:,:)
  CHARACTER(len=vol7d_cdatalen),POINTER :: voldatiattrc(:,:,:,:,:,:,:)

END TYPE vol7d

INTERFACE init
  MODULE PROCEDURE vol7d_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_delete
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
 vol7d_nullifyr, vol7d_nullifyd, vol7d_nullifyi, vol7d_nullifyb, vol7d_nullifyc


CONTAINS


SUBROUTINE vol7d_init(this)
TYPE(vol7d),intent(out) :: this

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


SUBROUTINE vol7d_delete(this)
TYPE(vol7d),intent(inout) :: this

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

IF (ASSOCIATED(this%ana)) DEALLOCATE(this%ana)
IF (ASSOCIATED(this%time)) DEALLOCATE(this%time)
IF (ASSOCIATED(this%level)) DEALLOCATE(this%level)
IF (ASSOCIATED(this%timerange)) DEALLOCATE(this%timerange)
IF (ASSOCIATED(this%network)) DEALLOCATE(this%network)

CALL delete(this%anavar)
CALL delete(this%anaattr)
CALL delete(this%anavarattr)
CALL delete(this%dativar)
CALL delete(this%datiattr)
CALL delete(this%dativarattr)

END SUBROUTINE vol7d_delete


SUBROUTINE vol7d_alloc(this, nana, ntime, nlevel, ntimerange, nnetwork, &
 nanavarr, nanavard, nanavari, nanavarb, nanavarc, &
 nanaattrr, nanaattrd, nanaattri, nanaattrb, nanaattrc, &
 nanavarattrr, nanavarattrd, nanavarattri, nanavarattrb, nanavarattrc, &
 ndativarr, ndativard, ndativari, ndativarb, ndativarc, &
 ndatiattrr, ndatiattrd, ndatiattri, ndatiattrb, ndatiattrc, &
 ndativarattrr, ndativarattrd, ndativarattri, ndativarattrb, ndativarattrc, &
 ini)
TYPE(vol7d),INTENT(inout) :: this
INTEGER,INTENT(in),OPTIONAL :: nana, ntime, nlevel, ntimerange, nnetwork, &
 nanavarr, nanavard, nanavari, nanavarb, nanavarc, &
 nanaattrr, nanaattrd, nanaattri, nanaattrb, nanaattrc, &
 nanavarattrr, nanavarattrd, nanavarattri, nanavarattrb, nanavarattrc, &
 ndativarr, ndativard, ndativari, ndativarb, ndativarc, &
 ndatiattrr, ndatiattrd, ndatiattri, ndatiattrb, ndatiattrc, &
 ndativarattrr, ndativarattrd, ndativarattri, ndativarattrb, ndativarattrc
LOGICAL,INTENT(in),OPTIONAL :: ini

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
      DO i = i, nana
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
      DO i = i, ntime
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
      DO i = i, nlevel
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
      DO i = i, ntimerange
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
      DO i = i, nnetwork
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


SUBROUTINE vol7d_alloc_vol(this, ini, inivol)
TYPE(vol7d),INTENT(inout) :: this
LOGICAL,INTENT(in),OPTIONAL :: ini, inivol

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
  IF (linivol) this%volanad(:,:,:) = dmiss
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
  IF (linivol) this%volanaattrd(:,:,:,:) = dmiss
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
  IF (linivol) this%voldatid(:,:,:,:,:,:) = dmiss
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
  IF (linivol) this%voldatiattrd(:,:,:,:,:,:,:) = dmiss
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

END SUBROUTINE vol7d_alloc_vol


SUBROUTINE vol7d_set_attr_ind(this)
TYPE(vol7d),INTENT(inout) :: this

integer :: i

IF (associated(this%dativar%r)) THEN
   IF (associated(this%dativarattr%r)) then
      DO i = 1, size(this%dativar%r)
         this%dativar%r(i)%r = firsttrue(this%dativar%r(i)%btable == this%dativarattr%r(:)%btable)
      enddo
   endif
endif


IF (associated(this%dativar%r)) THEN
   IF (associated(this%dativarattr%b)) then
      DO i = 1, size(this%dativar%r)
         this%dativar%r(i)%b = firsttrue(this%dativar%r(i)%btable == this%dativarattr%b(:)%btable)
      enddo
   endif
endif

END SUBROUTINE vol7d_set_attr_ind


SUBROUTINE vol7d_merge(this, that, sort)
TYPE(vol7d),INTENT(INOUT) :: this, that
LOGICAL,INTENT(IN),OPTIONAL :: sort

! Accodo that a this e distruggo that
CALL vol7d_append(this, that, sort)
CALL delete(that)

END SUBROUTINE vol7d_merge


!!$SUBROUTINE vol7d_duplicate(this, that, sort)
!!$TYPE(vol7d),INTENT(INOUT) :: this, that
!!$LOGICAL,INTENT(IN),OPTIONAL :: sort
!!$
!!$! Creo un volume vuoto e gli accodo this
!!$CALL init(that)
!!$CALL vol7d_alloc(that, nana=0, ntime=0, nlevel=0, ntimerange=0, nnetwork=0)
!!$CALL vol7d_alloc_vol(that)
!!$CALL vol7d_append(that, this, sort)
!!$
!!$END SUBROUTINE vol7d_duplicate


SUBROUTINE vol7d_append(this, that, sort)
TYPE(vol7d),INTENT(INOUT) :: this, that
LOGICAL,INTENT(IN),OPTIONAL :: sort

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

END SUBROUTINE vol7d_append


SUBROUTINE vol7d_reform(this, sort, unique, miss)
TYPE(vol7d),INTENT(INOUT) :: this
LOGICAL,INTENT(IN),OPTIONAL :: sort, unique, miss

TYPE(vol7d) :: v7dtmp

CALL vol7d_copy(this, v7dtmp, sort, unique, miss)
! Distruggo il vecchio volume e assegno il nuovo a this
CALL delete(this)
this = v7dtmp

END SUBROUTINE vol7d_reform


SUBROUTINE vol7d_copy(this, that, sort, unique, miss)
TYPE(vol7d),INTENT(INOUT) :: this, that
LOGICAL,INTENT(IN),OPTIONAL :: sort, unique, miss

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
 remapt)
CALL vol7d_remap1_vol7d_timerange(this%timerange, that%timerange, &
 lsort, lunique, lmiss, remaptr)
CALL vol7d_remap1_vol7d_level(this%level, that%level, lsort, lunique, lmiss, &
 remapl)
CALL vol7d_remap1_vol7d_ana(this%ana, that%ana, lsort, lunique, lmiss, &
 remapa)
CALL vol7d_remap1_vol7d_network(this%network, that%network, &
 lsort, lunique, lmiss, remapn)

! Faccio la riforma fisica dei volumi
CALL vol7d_reform_finalr(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss)
CALL vol7d_reform_finald(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss)
CALL vol7d_reform_finali(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss)
CALL vol7d_reform_finalb(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss)
CALL vol7d_reform_finalc(this, that, &
 remapa, remapt, remapl, remaptr, remapn, lsort, lunique, lmiss)

! Dealloco i vettori di rimappatura
IF (ASSOCIATED(remapt)) DEALLOCATE(remapt)
IF (ASSOCIATED(remaptr)) DEALLOCATE(remaptr)
IF (ASSOCIATED(remapl)) DEALLOCATE(remapl)
IF (ASSOCIATED(remapa)) DEALLOCATE(remapa)
IF (ASSOCIATED(remapn)) DEALLOCATE(remapn)

END SUBROUTINE vol7d_copy

! Creo le routine da ripetere per i vari tipi di dati vi v7d
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

END MODULE vol7d_class
