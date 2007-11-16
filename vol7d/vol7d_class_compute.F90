!> \brief Estensione della class vol7d_class per compiere semplici
!! operazioni matematico-statistiche sui volumi.
!!
!! Questo modulo estende vol7d_class aggiungendo operazioni elementari
!! quali la media e la cumulazione di dati in un volume vol7d_class::vol7d.
!! \ingroup vol7d
!!
!! \todo estendere l'operazione di media anche a dati istantanei.
MODULE vol7d_class_compute
USE vol7d_class

IMPLICIT NONE

CONTAINS

!> Cumula le osservazioni su un intervallo specificato quando possibile.
!! Crea un nuovo oggetto vol7d che contiene solo i dati del volume originario
!! soddisfacenti le condizioni:
!!  - variabili di tipo reale o doppia precisione
!!  - intervallo temporale di tipo "cumulazione"
!!    (vol7d_timerange_class::vol7d_timerange::timerange = 4)
!!    da un tempo passato al tempo attuale
!!  - intervallo di cumulazione che sia uguale o un sottomultiplo dell'intervallo
!!    di cumulazione desiderato \a step
!!
!! I volumi di anagrafica e relativi attributi sono copiati nel nuovo oggetto
!! senza variazioni.
!! Se \a start non è specificato, il metodo calcola automaticamente l'inizio
!! del periodo di cumulazione come il primo intervallo di tempo disponibile
!! nel volume, modulo \a step.
!! Per avere un pieno controllo sull'inizio della cumulazione, conviene
!! comunque specificare
!! \a start (attenzione che nel volume finale il tempo iniziale non sarà poi
!! \a start ma \a start + \a step perché le osservazioni cumulate sono
!! considerate valide al termine del periodo di cumulazione).
! Questo significa, ad esempio, che se estraiamo
! le precipitazioni cumulate dall'archivio Oracle del SIM (vedi
! vol7d_oraclecim_class) ad es. dalle 00 del 10/12/2004 alle 00 dell'11/12/2004
!!
!! \todo il parametro \a this è dichiarato \a INOUT perché la vol7d_alloc_vol
!! può modificarlo, bisognerebbe implementare una vol7d_check_vol che restituisca
!! errore anziché usare la vol7d_alloc_vol.
SUBROUTINE vol7d_cumulate(this, that, step, start, frac_valid)
TYPE(vol7d),INTENT(inout) :: this !< oggetto da cumulare, non viene modificato dal metodo
TYPE(vol7d),INTENT(out) :: that !< oggetto contenente, in uscita, i valori cumulati
TYPE(timedelta),INTENT(in) :: step !< intervallo di cumulazione
TYPE(datetime),INTENT(in),OPTIONAL :: start !< inizio del periodo di cumulazione
REAL,INTENT(in),OPTIONAL :: frac_valid !< frazione minima di dati validi necessaria per considerare accettabile un dato cumulato, default=1

CALL vol7d_extend_cumavg(this, that, 4, step, start, frac_valid)

END SUBROUTINE vol7d_cumulate


!> Media le osservazioni su un intervallo specificato quando possibile.
!! Funziona esattamente come il metodo ::vol7d_cumulate ma agisce
!! sui dati aventi un timerange di tipo "media"
!! (vol7d_timerange_class::vol7d_timerange::timerange = 3) e ne calcola
!! la media sull'intervallo specificato.
!!
!! \todo il parametro \a this è dichiarato \a INOUT perché la vol7d_alloc_vol
!! può modificarlo, bisognerebbe implementare una vol7d_check_vol che restituisca
!! errore anziché usare la vol7d_alloc_vol.
SUBROUTINE vol7d_average(this, that, step, start, frac_valid)
TYPE(vol7d),INTENT(inout) :: this !< oggetto da mediare, non viene modificato dal metodo
TYPE(vol7d),INTENT(out) :: that !< oggetto contenente, in uscita, i valori mediati
TYPE(timedelta),INTENT(in) :: step !< intervallo di media
TYPE(datetime),INTENT(in),OPTIONAL :: start !< inizio del periodo di media
REAL,INTENT(in),OPTIONAL :: frac_valid !< frazione minima di dati validi necessaria per considerare accettabile un dato mediato, default=1

CALL vol7d_extend_cumavg(this, that, 3, step, start, frac_valid)

END SUBROUTINE vol7d_average


SUBROUTINE vol7d_extend_cumavg(this, that, tri, step, start, frac_valid)
TYPE(vol7d),INTENT(in) :: this
TYPE(vol7d),INTENT(out) :: that
INTEGER,INTENT(in) :: tri
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),INTENT(in),OPTIONAL :: start
REAL,INTENT(in),OPTIONAL :: frac_valid

TYPE(datetime) :: lstart, lend, tmptime, tmptimes, t1, t2
TYPE(timedelta) dt1, dt2, stepvero
INTEGER :: steps, ntr, nstep, ncum, nval, i, j, k, i1, i3, i5, i6, n
INTEGER,ALLOCATABLE :: map_tr(:), map_trc(:,:), count_trc(:,:)
LOGICAL,ALLOCATABLE :: mask_time(:)
REAL :: lfrac_valid, frac_c, frac_m
TYPE(vol7d) :: v7dtmp

IF (PRESENT(frac_valid)) THEN
  lfrac_valid = frac_valid
ELSE
  lfrac_valid = 1.0
ENDIF

! mi premunisco da un oggetto non inizializzato
CALL vol7d_alloc_vol(this)
! conto quanti timerange si riferiscono a cumulazioni
ntr = COUNT(this%timerange(:)%timerange == tri .AND. this%timerange(:)%p2 == 0 &
 .AND. this%timerange(:)%p1 < 0)
IF (ntr == 0) THEN
  CALL raise_warning('nessun timerange adatto per media/cumulazione')
  RETURN
ENDIF
! pulisco e ordino il volume originale
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)
! riconto i timerange, potrebbero essere diminuiti a causa di unique
ntr = COUNT(this%timerange(:)%timerange == tri .AND. this%timerange(:)%p2 == 0 &
 .AND. this%timerange(:)%p1 < 0)

! conto il numero di time necessari in uscita
! lo faccio passo-passo e non con una divisione
! per farlo funzionare anche con le cumulate "umane"
! lstart e` l'inizio (non il termine) del primo periodo di cumulazione
IF (PRESENT(start)) THEN ! start fornito esplicitamente
  lstart = start
ELSE ! calcolo automatico di start
! calcolo il piu` breve intervallo di cumulazione disponibile
! potrei usare il piu` lungo se volessi (avrei piu` dati ma peggiori)
  i = MAXVAL(this%timerange(:)%p1, mask=(this%timerange(:)%timerange == tri &
   .AND. this%timerange(:)%p2 == 0 .AND. this%timerange(:)%p1 < 0))
  CALL init(dt1, minute=i/60)
  lstart = this%time(1)+dt1 ! torno indietro di dt1 (dt1 < 0!)
  lstart = lstart+(MOD(lstart, step)) ! arrotondo a step
ENDIF
lend = this%time(SIZE(this%time))
tmptime = lstart+step
nstep = 0
DO WHILE(tmptime <= lend)
  nstep = nstep + 1
  tmptime = tmptime + step
ENDDO
ALLOCATE(map_tr(ntr), map_trc(SIZE(this%time), ntr), count_trc(nstep, ntr), &
 mask_time(SIZE(this%time)))
map_trc(:,:) = 0
! creo un modello di volume con cio` che potrebbe non esserci nel volume vecchio
CALL init(that)
CALL vol7d_alloc(that, nana=0, ntime=nstep, nlevel=0, ntimerange=1, nnetwork=0)
CALL vol7d_alloc_vol(that)
DO i = 1, nstep
  that%time(i) = lstart + i*step ! non i-1 perche' le cumulate sono valide alla fine
ENDDO
CALL getval(step, aminute=steps)
steps = steps*60
CALL init(that%timerange(1), timerange=tri, p1=-steps, p2=0)

! Faccio una prima copia del volume originale
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE.)
! elimino da essa quello che non serve
DO i = 1, SIZE(v7dtmp%time)
  IF (.NOT.ANY(v7dtmp%time(i) == that%time)) v7dtmp%time(i) = datetime_miss
ENDDO
DO i = 1, SIZE(v7dtmp%timerange)
  IF (v7dtmp%timerange(i) /= that%timerange(1)) &
   v7dtmp%timerange(i) = vol7d_timerange_miss
ENDDO
CALL vol7d_reform(v7dtmp, miss=.TRUE., sort=.FALSE., unique=.FALSE.)
! Infine fondo quanto rimasto del volume originale con la bozza del nuovo volume
CALL vol7d_merge(that, v7dtmp, sort=.TRUE.)

nval = 0
DO j = 1, SIZE(this%timerange)
  IF (this%timerange(j)%timerange /= tri .OR. this%timerange(j)%p2 /= 0 &
   .OR. this%timerange(j)%p1 >= 0) CYCLE
  nval = nval + 1
  map_tr(nval) = j ! mappatura per ottimizzare il successivo ciclo sui timerange
  CALL init(dt1, minute=this%timerange(j)%p1/60)
  CALL init(dt2, minute=this%timerange(j)%p2/60)

  ! calcolo il numero teorico di intervalli in ingresso che
  ! contribuiscono all'intervallo corrente in uscita
  tmptimes = lstart
  tmptime = lstart+step
  ncum = 0
  DO WHILE(tmptime <= lend)
    ncum = ncum + 1
    stepvero = tmptime - tmptimes ! funziona anche se step e` "umano"
    count_trc(ncum,nval) = stepvero/(dt2-dt1)
    tmptimes = tmptime
    tmptime = tmptime + step
  ENDDO
  ! individuo gli intervalli in ingresso che contribuiscono all'intervallo
  ! corrente in uscita, scartando quelli che distano un numero non intero
  ! di intervalli in ingresso dall'inizio dell'intervallo in uscita
  DO i = 1, SIZE(this%time)
    t1 = this%time(i) + dt1
    t2 = this%time(i) + dt2
    DO k = 1, nstep
      IF (t1 >= that%time(k) - step .AND. t2 <= that%time(k)) THEN
        IF (MOD(t1-(that%time(k)-step), t2-t1) == timedelta_0) THEN
          map_trc(i,nval) = k
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

! finalmente cumulo
IF (ASSOCIATED(this%voldatir)) THEN
  DO i = 1, nstep
    DO i1 = 1, SIZE(this%ana)
      DO i3 = 1, SIZE(this%level)
        DO i6 = 1, SIZE(this%network)
          DO i5 = 1, SIZE(this%dativar%r)
            frac_m = 0. ! tengo il timerange che mi da` la frac max
            DO j = 1, ntr
              ! conto i dati che contribuiscono alla cumulata corrente
              mask_time = this%voldatir(i1,:,i3,map_tr(j),i5,i6) /= rmiss .AND. &
               map_trc(:,j) == i
              n = COUNT(mask_time)
              ! conto la frazione di dati presenti rispetto a quelli necessari
              frac_c = REAL(n)/count_trc(i,j)
              IF (frac_c >= MAX(lfrac_valid, frac_m)) THEN
                frac_m = frac_c
                IF (tri == 3) THEN
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)/n
                ELSE
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

IF (ASSOCIATED(this%voldatid)) THEN
  DO i = 1, nstep
    DO i1 = 1, SIZE(this%ana)
      DO i3 = 1, SIZE(this%level)
        DO i6 = 1, SIZE(this%network)
          DO i5 = 1, SIZE(this%dativar%r)
            frac_m = 0. ! tengo il timerange che mi da` la frac max
            DO j = 1, ntr
              ! conto i dati che contribuiscono alla cumulata corrente
              mask_time = this%voldatid(i1,:,i3,map_tr(j),i5,i6) /= rdmiss .AND. &
               map_trc(:,j) == i
              n = COUNT(mask_time)
              ! conto la frazione di dati presenti rispetto a quelli necessari
              frac_c = REAL(n)/count_trc(i,j)
              IF (frac_c >= MAX(lfrac_valid, frac_m)) THEN
                frac_m = frac_c
                IF (tri == 3) THEN
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)/n
                ELSE
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

DEALLOCATE(map_tr, map_trc, count_trc, mask_time)

END SUBROUTINE vol7d_extend_cumavg

END MODULE vol7d_class_compute
