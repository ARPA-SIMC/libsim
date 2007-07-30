MODULE vol7d_class_compute
USE vol7d_class

IMPLICIT NONE

CONTAINS

SUBROUTINE vol7d_cumulate(this, that, step, start, frac_valid)
TYPE(vol7d),INTENT(in) :: this
TYPE(vol7d),INTENT(out) :: that
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),INTENT(in),OPTIONAL :: start
REAL,INTENT(in),OPTIONAL :: frac_valid

TYPE(datetime) :: lstart, tmptime, tmptimes, t1, t2
TYPE(timedelta) dt1, dt2, stepvero
INTEGER :: steps, ntr, nstep, ncum, nval, i, j, k, i1, i3, i5, i6
INTEGER,ALLOCATABLE :: map_tr(:), map_trc(:,:), count_trc(:,:)
REAL :: lfrac_valid, frac_c, frac_m

TYPE(vol7d) :: v7dtmp

!TYPE(datetime),ALLOCATABLE :: time_bak(:)
!TYPE(vol7d_timerange),ALLOCATABLE :: timerange_bak(:)

IF (PRESENT(frac_valid)) THEN
  lfrac_valid = frac_valid
ELSE
  lfrac_valid = 1.0
ENDIF

! mi premunisco da un oggetto non inizializzato
CALL vol7d_alloc_vol(this)

! conto quanti timerange si riferiscono a cumulazioni
ntr = COUNT(this%timerange(:)%timerange == 4 .AND. this%timerange(:)%p2 == 0 &
 .AND. this%timerange(:)%p1 < 0)
IF (ntr == 0) THEN
  CALL raise_warning('nessun timerange adatto per la cumulazione')
  RETURN
ENDIF
! conto il numero di time necessari in uscita
! lo faccio passo-passo e non con una divisione
! per farlo funzionare anche con le cumulate "umane"
IF (PRESENT(start)) THEN
  lstart = start
ELSE
  lstart = this%time(1)
ENDIF
tmptime = lstart+step
nstep = 0
DO WHILE(tmptime <= this%time(SIZE(this%time)))
  nstep = nstep + 1
  tmptime = tmptime + step
ENDDO
ALLOCATE(map_tr(ntr), map_trc(SIZE(this%time), ntr), count_trc(nstep, ntr))
map_trc(:,:) = 0

CALL init(that)
CALL vol7d_alloc(that, nana=0, ntime=nstep, nlevel=0, ntimerange=1, nnetwork=0)
CALL vol7d_alloc_vol(that)
DO i = 1, nstep
  that%time(i) = lstart + i*step ! non i-1 perche' le cumulate sono valide alla fine
ENDDO
CALL getval(step, aminute=steps)
steps = steps*60
CALL init(that%timerange(1), timerange=4, p1=-steps, p2=0)

! Faccio una prima copia del volume originale
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE.)
! Elimino da esso quello che non serve
DO i = 1, SIZE(v7dtmp%time)
  IF (.NOT.ANY(v7dtmp%time(i) == that%time)) v7dtmp%time(i) = datetime_miss
ENDDO
DO i = 1, SIZE(v7dtmp%timerange)
  IF (v7dtmp%timerange(i) /= that%timerange(1)) &
   v7dtmp%timerange(i) = vol7d_timerange_miss
ENDDO
CALL vol7d_reform(v7dtmp, miss=.TRUE., sort=.TRUE.)
! Infine fondo quanto rimasto del volume originale con la bozza del nuovo volume
CALL vol7d_merge(that, v7dtmp, sort=.TRUE.)

nval = 0
DO j = 1, SIZE(this%timerange)
  IF (this%timerange(j)%timerange /= 4 .OR. this%timerange(j)%p2 /= 0) CYCLE
  nval = nval + 1
  map_tr(nval) = j
  CALL init(dt1, minute=this%timerange(j)%p1/60)
  CALL init(dt2, minute=this%timerange(j)%p2/60)

  ! calcolo il numero teorico di intervalli in ingresso che
  ! contribuiscono all'intervallo corrente in uscita
  tmptimes = lstart
  tmptime = lstart+step
  ncum = 0
  DO WHILE(tmptime <= this%time(SIZE(this%time)))
    ncum = ncum + 1
    stepvero = tmptime - tmptimes ! funziona anche se step e` "umano"
    count_trc(ncum,nval) = stepvero/(dt2-dt1) ! attenzione a div per zero qui!
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

that%voldatir = rmiss
DO i = 1, nstep ! finalmente cumulo, per ora solo voldatir
  DO i1 = 1, SIZE(this%ana)
    DO i3 = 1, SIZE(this%level)
      DO i6 = 1, SIZE(this%network)
        DO i5 = 1, SIZE(this%dativar%r)
          frac_m = 0. ! tengo il timerange che mi da` la frac max
          DO j = 1, ntr ! SIZE(this%timerange)
            ! conto i dati che contribuiscono alla cumulata corrente
            nval = COUNT(this%voldatir(i1,:,i3,map_tr(j),i5,i6) /= rmiss .AND. &
             map_trc(:,j) == i)
            ! conto la frazione di dati presenti rispetto a quelli necessari
            frac_c = REAL(nval)/count_trc(i,j)
            IF (frac_c >= MAX(lfrac_valid, frac_m)) THEN
              frac_m = frac_c
              that%voldatir(i1,i,i3,1,i5,i6) = &
               SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
               mask=this%voldatir(i1,:,i3,map_tr(j),i5,i6) /= rmiss .AND. &
               map_trc(:,j) == i)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

DEALLOCATE(map_tr, map_trc, count_trc)

END SUBROUTINE vol7d_cumulate


SUBROUTINE vol7d_cumulate_old(this, that, step, start, frac_valid)
TYPE(vol7d),INTENT(in) :: this
TYPE(vol7d),INTENT(out) :: that
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),INTENT(in),OPTIONAL :: start
REAL,INTENT(in),OPTIONAL :: frac_valid

TYPE(datetime) :: lstart, tmptime, tmptimes, t1, t2
TYPE(timedelta) dt1, dt2, stepvero
INTEGER :: steps, ntr, nstep, ncum, nval, i, j, k, i1, i3, i5, i6
INTEGER,ALLOCATABLE :: map_tr(:), map_trc(:,:), count_trc(:,:)
REAL :: lfrac_valid, frac_c, frac_m

PRINT*,'vecio'

IF (PRESENT(frac_valid)) THEN
  lfrac_valid = frac_valid
ELSE
  lfrac_valid = 1.0
ENDIF

! mi premunisco da un oggetto non inizializzato
CALL vol7d_alloc_vol(this)
! conto quanti timerange si riferiscono a cumulazioni
ntr = COUNT(this%timerange(:)%timerange == 4 .AND. this%timerange(:)%p2 == 0 &
 .AND. this%timerange(:)%p1 < 0)
IF (ntr == 0) THEN
  CALL raise_warning('nessun timerange adatto per la cumulazione')
  RETURN
ENDIF
! conto il numero di time necessari in uscita
! lo faccio passo-passo e non con una divisione
! per farlo funzionare anche con le cumulate "umane"
IF (PRESENT(start)) THEN
  lstart = start
ELSE
  lstart = this%time(1)
ENDIF
tmptime = lstart+step
nstep = 0
DO WHILE(tmptime <= this%time(SIZE(this%time)))
  nstep = nstep + 1
  tmptime = tmptime + step
ENDDO
ALLOCATE(map_tr(ntr), map_trc(SIZE(this%time), ntr), count_trc(nstep, ntr))
map_trc(:,:) = 0

! Clono parte del volume (fare meglio)
CALL init(that)
CALL vol7d_alloc(that, nana=SIZE(this%ana), ntime=nstep, nlevel=SIZE(this%level), &
 ntimerange=1, ndativarr=SIZE(this%dativar%r))
CALL vol7d_alloc_vol(that)
that%ana = this%ana
that%level = this%level
that%dativar%r = this%dativar%r

DO i = 1, nstep
  that%time(i) = lstart + i*step ! non i-1 perche' le cumulate sono valide alla fine
ENDDO
CALL getval(step, aminute=steps)
steps = steps*60
CALL init(that%timerange(1), timerange=4, p1=-steps, p2=0)
!CALL vol7d_reform(that, miss=.FALSE., sort=.FALSE.)

nval = 0
DO j = 1, SIZE(this%timerange)
  IF (this%timerange(j)%timerange /= 4 .OR. this%timerange(j)%p2 /= 0) CYCLE
  nval = nval + 1
  map_tr(nval) = j
  CALL init(dt1, minute=this%timerange(j)%p1/60)
  CALL init(dt2, minute=this%timerange(j)%p2/60)

  ! calcolo il numero teorico di intervalli in ingresso che
  ! contribuiscono all'intervallo corrente in uscita
  tmptimes = lstart
  tmptime = lstart+step
  ncum = 0
  DO WHILE(tmptime <= this%time(SIZE(this%time)))
    ncum = ncum + 1
    stepvero = tmptime - tmptimes ! funziona anche se step e` "umano"
    count_trc(ncum,nval) = stepvero/(dt2-dt1) ! attenzione a div per zero qui!
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

that%voldatir = rmiss
DO i = 1, nstep ! finalmente cumulo, per ora solo voldatir
  DO i1 = 1, SIZE(this%ana)
    DO i3 = 1, SIZE(this%level)
      DO i6 = 1, SIZE(this%network)
        DO i5 = 1, SIZE(this%dativar%r)
          frac_m = 0. ! tengo il timerange che mi da` la frac max
          DO j = 1, ntr ! SIZE(this%timerange)
            ! conto i dati che contribuiscono alla cumulata corrente
            nval = COUNT(this%voldatir(i1,:,i3,map_tr(j),i5,i6) /= rmiss .AND. &
             map_trc(:,j) == i)
            ! conto la frazione di dati presenti rispetto a quelli necessari
            frac_c = REAL(nval)/count_trc(i,j)
            IF (frac_c >= MAX(lfrac_valid, frac_m)) THEN
              frac_m = frac_c
              that%voldatir(i1,i,i3,1,i5,i6) = &
               SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
               mask=this%voldatir(i1,:,i3,map_tr(j),i5,i6) /= rmiss .AND. &
               map_trc(:,j) == i)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

DEALLOCATE(map_tr, map_trc, count_trc)

END SUBROUTINE vol7d_cumulate_old

END MODULE vol7d_class_compute
