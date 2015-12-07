! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
#include "config.h"
!> This module contains functions that are only for internal use of
!! the library. It should not be used by user procedures because it is
!! subject to change
!! \ingroup vol7d
MODULE stat_proc_engine
USE datetime_class
USE vol7d_class
IMPLICIT NONE

CONTAINS

! common operations for statistical processing by aggregation
SUBROUTINE recompute_stat_proc_agg_common(itime, itimerange, stat_proc, tri, &
 step, otime, otimerange, map_tr, map_trc, count_trc, start)
TYPE(datetime),INTENT(in) :: itime(:)
TYPE(vol7d_timerange),INTENT(in) :: itimerange(:)
INTEGER,INTENT(in) :: stat_proc
INTEGER,INTENT(in) :: tri
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),POINTER :: otime(:)
TYPE(vol7d_timerange),INTENT(out) :: otimerange
INTEGER,POINTER :: map_tr(:), map_trc(:,:), count_trc(:,:)
TYPE(datetime),INTENT(in),OPTIONAL :: start

INTEGER :: i, j, k, nval, ncum, ntr, nstep
LOGICAL :: usestart, tr_mask(SIZE(itimerange))
TYPE(datetime) :: lstart, lend, tmptime, tmptimes, t1, t2
TYPE(timedelta) :: dt1, stepvero
INTEGER(kind=int_ll) :: tmpmsec


IF (SIZE(itime) == 0) THEN ! avoid segmentation fault in case of empty volume
  ALLOCATE(otime(0), map_tr(0), map_trc(0,0), count_trc(0,0))
  otimerange = vol7d_timerange_miss
  RETURN
ENDIF

! useful timeranges
tr_mask(:) = itimerange(:)%timerange == tri .AND. itimerange(:)%p2 /= imiss &
 .AND. itimerange(:)%p2 /= 0 .AND. itimerange(:)%p1 == 0
ntr = COUNT(tr_mask)

! compute lstart = the start time (not the end) of the first
! processing interval
usestart = PRESENT(start) ! treat datetime_miss as .NOT.PRESENT()
IF (usestart) usestart = usestart .AND. start /= datetime_miss
IF (usestart) THEN ! start explicitely provided
  lstart = start
ELSE ! compute start automatically
! the shortest interval available is used, the longest could be used
! obtaining more data but worse
  lstart = itime(1) - &
   timedelta_new(msec=MINVAL(itimerange(:)%p2, mask=tr_mask(:))*1000)

  lstart = lstart-(MOD(lstart, step)) ! round to step, check the - sign!!!
ENDIF
lend = itime(SIZE(itime))

! count the size of output time, it is done step by step and not with
! a / operation in order to make it work also for "popular" intervals
tmptime = lstart+step
nstep = 0
DO WHILE(tmptime <= lend)
  nstep = nstep + 1
  tmptime = tmptime + step
ENDDO
ALLOCATE(map_tr(ntr), map_trc(SIZE(itime), ntr), count_trc(nstep, ntr))
map_trc(:,:) = 0

! compute otime
ALLOCATE(otime(nstep))
tmptime = lstart+step
DO i = 1, nstep
  otime(i) = tmptime ! validity time is the end of the interval
  tmptime = tmptime + step
ENDDO
! compute otimerange
CALL getval(timedelta_depop(step), amsec=tmpmsec)
CALL init(otimerange, timerange=stat_proc, p1=0, p2=INT(tmpmsec/1000))

nval = 0
DO j = 1, SIZE(itimerange)
  IF (.NOT.tr_mask(j)) CYCLE

  nval = nval + 1
  map_tr(nval) = j ! mappatura per ottimizzare il successivo ciclo sui timerange
  dt1 = timedelta_new(msec=itimerange(j)%p2*1000)

  ! calcolo il numero teorico di intervalli in ingresso che
  ! contribuiscono all'intervallo corrente in uscita
  tmptimes = lstart
  tmptime = lstart+step
  ncum = 0
  DO WHILE(tmptime <= lend)
    ncum = ncum + 1
    stepvero = tmptime - tmptimes ! funziona anche se step e` "umano"
    count_trc(ncum,nval) = stepvero/dt1
    tmptimes = tmptime
    tmptime = tmptime + step
  ENDDO
  ! individuo gli intervalli in ingresso che contribuiscono all'intervallo
  ! corrente in uscita, scartando quelli che distano un numero non intero
  ! di intervalli in ingresso dall'inizio dell'intervallo in uscita
  DO i = 1, SIZE(itime)
    t1 = itime(i) - dt1
    t2 = itime(i)
    DO k = 1, nstep
      IF (t1 >= otime(k) - step .AND. t2 <= otime(k)) THEN
        IF (MOD(t1-(otime(k)-step), t2-t1) == timedelta_0) THEN
          map_trc(i,nval) = k
        ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE recompute_stat_proc_agg_common


! common operations for statistical processing by aggregation from
! instantaneous data
SUBROUTINE compute_stat_proc_agg_common(itime, stat_proc, &
 step, otime, otimerange, itime_start, itime_end, start)
TYPE(datetime),INTENT(in) :: itime(:)
INTEGER,INTENT(in) :: stat_proc
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),POINTER :: otime(:)
TYPE(vol7d_timerange),INTENT(out) :: otimerange
INTEGER,POINTER :: itime_start(:), itime_end(:)
TYPE(datetime),INTENT(in),OPTIONAL :: start

INTEGER :: i, j, nstep
LOGICAL :: usestart
TYPE(datetime) :: lstart, lend, tmptime
INTEGER(kind=int_ll) :: tmpmsec
CHARACTER(len=8) :: env_var
LOGICAL :: climat_behavior

IF (SIZE(itime) == 0) THEN ! avoid segmentation fault in case of empty volume
  ALLOCATE(otime(0), itime_start(0), itime_end(0))
  otimerange = vol7d_timerange_miss
  RETURN
ENDIF

! enable bad behavior for climat database
env_var = ''
CALL getenv('LIBSIM_CLIMAT_BEHAVIOR', env_var)
climat_behavior = LEN_TRIM(env_var) > 0

! compute lstart = the start time (not the end) of the first
! processing interval
usestart = PRESENT(start) ! treat datetime_miss as .NOT.PRESENT()
IF (usestart) usestart = usestart .AND. start /= datetime_miss
IF (usestart) THEN ! start explicitely provided
  lstart = start
ELSE ! compute start automatically
! the shortest interval available is used, the longest could be used
! obtaining more data but worse
  lstart = itime(1)
ENDIF
lend = itime(SIZE(itime))

! count the size of output time, it is done step by step and not with
! a / operation in order to make it work also for "popular" intervals
tmptime = lstart+step
nstep = 0
DO WHILE(tmptime <= lend)
  nstep = nstep + 1
  tmptime = tmptime + step
ENDDO

ALLOCATE(itime_start(nstep), itime_end(nstep))
itime_start(:) = SIZE(itime)+1
itime_end(:) = 0

! compute otime
ALLOCATE(otime(nstep))
tmptime = lstart!+step
j = 1
DO i = 1, nstep
! search for start interval (itime sorted)
  DO WHILE(itime(j) < tmptime .AND. j < SIZE(itime))
    j = j + 1
  ENDDO
  IF (itime(j) >= tmptime) THEN
    itime_start(i) = j
    IF (climat_behavior .AND. itime(j) == tmptime) THEN
      itime_start(i) = j + 1
    ENDIF
  ENDIF

  tmptime = tmptime + step
  DO WHILE(itime(j) < tmptime .AND. j < SIZE(itime))
    j = j + 1
  ENDDO
  IF (itime(j) <= tmptime) THEN ! take into account corner cases
    itime_end(i) = j
  ELSE
    itime_end(i) = j - 1
  ENDIF

  otime(i) = tmptime ! validity time is the end of the interval
ENDDO
! compute otimerange
CALL getval(timedelta_depop(step), amsec=tmpmsec)
CALL init(otimerange, timerange=stat_proc, p1=0, p2=INT(tmpmsec/1000_int_ll))

END SUBROUTINE compute_stat_proc_agg_common


! common operations for statistical processing by differences
SUBROUTINE recompute_stat_proc_diff_common(itime, itimerange, stat_proc, step, &
 nitr, otime, otimerange, map_tr, f, mask_timerange, time_definition, full_steps, &
 start)
TYPE(datetime),INTENT(in) :: itime(:)
TYPE(vol7d_timerange),INTENT(in) :: itimerange(:)
INTEGER,INTENT(in) :: stat_proc
TYPE(timedelta),INTENT(in) :: step
INTEGER,INTENT(out) :: nitr
TYPE(datetime),POINTER :: otime(:)
TYPE(vol7d_timerange),POINTER :: otimerange(:)
INTEGER,POINTER :: map_tr(:,:,:,:,:), f(:)
LOGICAL,POINTER :: mask_timerange(:)
INTEGER,INTENT(in) :: time_definition
LOGICAL,INTENT(in),OPTIONAL :: full_steps
TYPE(datetime),INTENT(in),OPTIONAL :: start

INTEGER :: i, j, k, l, dirtyrep
INTEGER :: steps, deltas
LOGICAL :: useful
TYPE(datetime) :: pstart1, pstart2, pend1, pend2, reftime1, reftime2, tmptime
TYPE(vol7d_timerange) :: tmptimerange
TYPE(arrayof_datetime) :: a_otime
TYPE(arrayof_vol7d_timerange) :: a_otimerange

! compute length of cumulation step in seconds
CALL getval(step, asec=steps)

deltas = 0
IF (PRESENT(start)) THEN
  IF (SIZE(itime) > 1 .AND. c_e(start)) THEN ! security check
    CALL getval(start-itime(1), asec=deltas)
  ENDIF
ENDIF
  
! create a mask of suitable timeranges
ALLOCATE(mask_timerange(SIZE(itimerange)))
mask_timerange(:) = itimerange(:)%timerange == stat_proc &
 .AND. itimerange(:)%p1 /= imiss .AND. itimerange(:)%p2 /= imiss &
 .AND. itimerange(:)%p1 >= 0 &
 .AND. itimerange(:)%p2 > 0

IF (optio_log(full_steps) .AND. steps /= 0) THEN ! keep only timeranges defining intervals ending at integer steps, check better steps /= 0
  mask_timerange(:) = mask_timerange(:) .AND. (MOD(itimerange(:)%p2-deltas, steps) == 0)
ENDIF
nitr = COUNT(mask_timerange)
ALLOCATE(f(nitr))
j = 1
DO i = 1, nitr
  DO WHILE(.NOT.mask_timerange(j))
    j = j + 1
  ENDDO
  f(i) = j
  j = j + 1
ENDDO

ALLOCATE(map_tr(nitr, SIZE(itime), nitr, SIZE(itime), 2))
map_tr(:,:,:,:,:) = imiss

mask_timerange(:) = mask_timerange(:) .AND. itimerange(:)%p2 == steps
DO i = 1, SIZE(mask_timerange)
  IF (mask_timerange(i)) THEN
    j = append_unique(a_otimerange, itimerange(i))
  ENDIF
ENDDO

! scan through all possible combinations of time and timerange
DO dirtyrep = 1, 2
  IF (dirtyrep == 2) THEN ! dirty and expensive trick for sorting descriptors
    CALL packarray(a_otime)
    CALL packarray(a_otimerange)
    CALL sort(a_otime%array)
    CALL sort(a_otimerange%array)
  ENDIF
  DO l = 1, SIZE(itime)
    DO k = 1, nitr
      CALL time_timerange_get_period(itime(l), itimerange(f(k)), &
       time_definition, pstart2, pend2, reftime2)

      DO j = 1, SIZE(itime)
        DO i = 1, nitr
          useful = .FALSE.
          CALL time_timerange_get_period(itime(j), itimerange(f(i)), &
           time_definition, pstart1, pend1, reftime1)
          tmptimerange = vol7d_timerange_new(timerange=stat_proc)

          IF (reftime2 == pend2 .AND. reftime1 == pend1) THEN ! analysis
            IF (pstart2 == pstart1 .AND. pend2 > pend1) THEN ! =-|
              CALL time_timerange_set_period(tmptime, tmptimerange, &
               time_definition, pend1, pend2, reftime2)
              useful = .TRUE.

            ELSE IF (pstart2 < pstart1 .AND. pend2 == pend1) THEN ! -=|
              CALL time_timerange_set_period(tmptime, tmptimerange, &
               time_definition, pstart2, pstart1, pstart1)
              useful = .TRUE.
            ENDIF

          ELSE IF (reftime2 == reftime1) THEN ! forecast, same reftime
            IF (pstart2 == pstart1 .AND. pend2 > pend1) THEN ! |=-
              CALL time_timerange_set_period(tmptime, tmptimerange, &
               time_definition, pend1, pend2, reftime2)
              useful = .TRUE.

            ELSE IF (pstart2 < pstart1 .AND. pend2 == pend1) THEN ! |-=
              CALL time_timerange_set_period(tmptime, tmptimerange, &
               time_definition, pstart2, pstart1, reftime2)
              useful = .TRUE.
            ENDIF

          ENDIF
          useful = useful .AND. tmptime /= datetime_miss .AND. &
           tmptimerange /= vol7d_timerange_miss .AND. tmptimerange%p2 == steps

          IF (useful) THEN ! add a_otime, a_otimerange
            map_tr(i,j,k,l,1) = append_unique(a_otime, tmptime)
            map_tr(i,j,k,l,2) = append_unique(a_otimerange, tmptimerange)
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

otime => a_otime%array
otimerange => a_otimerange%array
! delete local objects keeping the contents
CALL delete(a_otime, nodealloc=.TRUE.)
CALL delete(a_otimerange, nodealloc=.TRUE.)

mask_timerange(:) = mask_timerange(:) .AND. itimerange(:)%p2 == steps

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, map_tr: '//t2c((SIZE(map_tr,1)))//', '// &
 t2c((SIZE(map_tr,2)))//', '// &
 t2c((SIZE(map_tr,3)))//', '// &
 t2c((SIZE(map_tr,4))))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, map_tr: '//t2c((SIZE(map_tr)))//', '// &
 t2c(COUNT(c_e(map_tr))))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, nitr: '//t2c(nitr))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, good timeranges: '//t2c(COUNT(mask_timerange)))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, output times: '//t2c(SIZE(otime)))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, output timeranges: '//t2c(SIZE(otimerange)))
#endif

END SUBROUTINE recompute_stat_proc_diff_common


! common operations for statistical processing by metamorphosis
SUBROUTINE compute_stat_proc_metamorph_common(istat_proc, itimerange, ostat_proc, &
 otimerange, map_tr)
INTEGER,INTENT(in) :: istat_proc
TYPE(vol7d_timerange),INTENT(in) :: itimerange(:)
INTEGER,INTENT(in) :: ostat_proc
!TYPE(timedelta),INTENT(in) :: step
TYPE(vol7d_timerange),POINTER :: otimerange(:)
INTEGER,POINTER :: map_tr(:)

INTEGER :: i
LOGICAL :: tr_mask(SIZE(itimerange))

IF (SIZE(itimerange) == 0) THEN ! avoid segmentation fault in case of empty volume
  ALLOCATE(otimerange(0), map_tr(0))
  RETURN
ENDIF

! useful timeranges
tr_mask(:) = itimerange(:)%timerange == istat_proc .AND. itimerange(:)%p2 /= imiss &
 .AND. itimerange(:)%p2 /= 0 ! .AND. itimerange(:)%p1 == 0
ALLOCATE(otimerange(COUNT(tr_mask)), map_tr(COUNT(tr_mask)))

otimerange = PACK(itimerange, mask=tr_mask)
otimerange(:)%timerange = ostat_proc
map_tr = PACK((/(i,i=1,SIZE(itimerange))/), mask=tr_mask)

END SUBROUTINE compute_stat_proc_metamorph_common


! common operations for statistical processing by aggregation, experimental
SUBROUTINE recompute_stat_proc_agg_common_exp(itime, itimerange, stat_proc, tri, &
 step, time_definition, otime, otimerange, map_ttr, dtratio, start)
TYPE(datetime),INTENT(in) :: itime(:)
TYPE(vol7d_timerange),INTENT(in) :: itimerange(:)
INTEGER,INTENT(in) :: stat_proc
INTEGER,INTENT(in) :: tri
TYPE(timedelta),INTENT(in) :: step
INTEGER,INTENT(in) :: time_definition
TYPE(datetime),POINTER :: otime(:)
TYPE(vol7d_timerange),POINTER :: otimerange(:)
INTEGER,POINTER :: map_ttr(:,:,:)
INTEGER,POINTER :: dtratio(:)
TYPE(datetime),INTENT(in),OPTIONAL :: start

INTEGER :: i, j, k, l, na, nf
INTEGER :: steps, p1, maxp1, minp2, minp1mp2, dstart
LOGICAL :: lforecast
TYPE(datetime) :: lstart, lend, pstart1, pstart2, pend1, pend2, reftime1, reftime2, tmptime
TYPE(arrayof_datetime) :: a_otime
TYPE(arrayof_vol7d_timerange) :: a_otimerange
TYPE(arrayof_integer) :: a_dtratio
LOGICAL,ALLOCATABLE :: mask_timerange(:) ! improve !!!!

! compute length of cumulation step in seconds
CALL getval(timedelta_depop(step), asec=steps)

! create a mask of suitable timeranges
ALLOCATE(mask_timerange(SIZE(itimerange)))
mask_timerange(:) = itimerange(:)%timerange == tri &
 .AND. itimerange(:)%p1 /= imiss .AND. itimerange(:)%p2 /= imiss &
 .AND. itimerange(:)%p1 >= 0 &
 .AND. itimerange(:)%p2 > 0 &
 .AND. MOD(steps, itimerange(:)%p2) == 0

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, number of useful timeranges before choosing analysis/forecast: '// &
 t2c(COUNT(mask_timerange)))
#endif

! euristically determine whether we are dealing with an
! analysis/observation or a forecast dataset
na = COUNT(itimerange(:)%timerange == tri .AND. &
 itimerange(:)%p1 == 0 .AND. itimerange(:)%p2 > 0)
nf = COUNT(itimerange(:)%timerange == tri .AND. &
 itimerange(:)%p1 > 0 .AND. itimerange(:)%p2 > 0)
lforecast = nf >= na
#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, na: '//t2c(na)//', nf: '//t2c(nf))
#endif

! keep only timeranges of one type (really necessary?)
IF (lforecast) THEN
  mask_timerange(:) = mask_timerange(:) .AND. itimerange(:)%p1 > 0
  CALL l4f_log(L4F_INFO, &
   'recompute_stat_proc_agg, processing in forecast mode')
ELSE
  mask_timerange(:) = mask_timerange(:) .AND. itimerange(:)%p1 == 0
  CALL l4f_log(L4F_INFO, &
   'recompute_stat_proc_agg, processing in analysis mode')
ENDIF

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, number of useful timeranges: '// &
 t2c(COUNT(mask_timerange)))
#endif

IF (SIZE(itime) == 0 .OR. COUNT(mask_timerange) == 0) THEN ! avoid segmentation fault in case of empty volume
  ALLOCATE(otime(0), otimerange(0), dtratio(0), map_ttr(0,0,0))
  RETURN
ENDIF

! determine start and end of processing period
lstart = datetime_miss
IF (PRESENT(start)) lstart = start
IF (lstart == datetime_miss) THEN
  lstart = itime(1)
ENDIF
lend = itime(SIZE(itime))
! correct them
maxp1 = MAXVAL(itimerange(:)%p1, mask=mask_timerange)
minp2 = MINVAL(itimerange(:)%p2, mask=mask_timerange)
minp1mp2 = MINVAL(itimerange(:)%p1 - itimerange(:)%p2, mask=mask_timerange)
IF (time_definition == 0) THEN ! reference time
  lstart = lstart + timedelta_new(msec=1000*minp1mp2)
  lend = lend + timedelta_new(msec=1000*maxp1)
ELSE ! verification time
  lstart = lstart - timedelta_new(msec=1000*minp2)
  lstart = lstart - (MOD(lstart, step)) ! round to step, check the - sign!!!
ENDIF
#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, processing period: '//t2c(lstart)//' - '//t2c(lend))
#endif

! create output time and timerange lists

IF (lforecast) THEN ! forecast mode
  IF (time_definition == 0) THEN ! reference time
    CALL insert(a_otime, itime)

! apply start shift to timerange, not to reftime
    CALL getval(lstart-itime(1), asec=dstart)
    dstart = MAX(0, dstart)
    DO p1 = steps + dstart, maxp1, steps
      CALL insert_unique(a_otimerange, vol7d_timerange_new(stat_proc, p1, steps))
    ENDDO

  ELSE ! verification time
    tmptime = lstart + step
    DO WHILE(tmptime <= lend)
      CALL insert_unique(a_otime, tmptime)
      tmptime = tmptime + step
    ENDDO
    DO p1 = steps, maxp1, steps
      CALL insert_unique(a_otimerange, vol7d_timerange_new(stat_proc, p1, steps))
    ENDDO

  ENDIF

ELSE ! analysis mode
  tmptime = lstart + step
  DO WHILE(tmptime <= lend)
    CALL insert_unique(a_otime, tmptime)
    tmptime = tmptime + step
  ENDDO
  CALL insert_unique(a_otimerange, vol7d_timerange_new(stat_proc, 0, steps))

ENDIF

! count the possible i/o interval ratios
DO k = 1, SIZE(itimerange)
  IF (itimerange(k)%p2 /= 0) &
   CALL insert_unique(a_dtratio, steps/itimerange(k)%p2) ! guaranteed to be integer
ENDDO

CALL packarray(a_otime)
CALL packarray(a_otimerange)
CALL packarray(a_dtratio)
otime => a_otime%array
otimerange => a_otimerange%array
dtratio => a_dtratio%array
CALL sort(otime)
CALL sort(otimerange)
CALL sort(dtratio)
! delete local objects keeping the contents
CALL delete(a_otime, nodealloc=.TRUE.)
CALL delete(a_otimerange, nodealloc=.TRUE.)
CALL delete(a_dtratio, nodealloc=.TRUE.)

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, output time and timerange: '//&
 t2c(SIZE(otime))//', '//t2c(size(otimerange)))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, found '//t2c(size(dtratio))// &
 ' possible aggregation ratios, from '// &
 t2c(dtratio(1))//' to '//t2c(dtratio(SIZE(dtratio))))
#endif

ALLOCATE(map_ttr(SIZE(itime),SIZE(itimerange),3))
map_ttr(:,:,:) = imiss
do_itimerange: DO l = 1, SIZE(itimerange)
  IF (.NOT.mask_timerange(l)) CYCLE do_itimerange
  do_itime: DO k = 1, SIZE(itime)
  CALL time_timerange_get_period(itime(k), itimerange(l), &
   time_definition, pstart1, pend1, reftime1)
    do_otimerange: DO j = 1, SIZE(otimerange)
      do_otime: DO i = 1, SIZE(otime)
        CALL time_timerange_get_period(otime(i), otimerange(j), &
         time_definition, pstart2, pend2, reftime2)
        IF (lforecast) THEN
          IF (reftime1 /= reftime2) CYCLE do_otime
        ENDIF

        IF (pstart1 >= pstart2 .AND. pend1 <= pend2 .AND. &
         MOD(pstart1-pstart2, pend1-pstart1) == timedelta_0) THEN ! useful
          map_ttr(k,l,1) = i
          map_ttr(k,l,2) = j
          map_ttr(k,l,3) = steps/itimerange(l)%p2 ! guaranteed to be integer
          CYCLE do_itime
        ENDIF
      ENDDO do_otime
    ENDDO do_otimerange
  ENDDO do_itime
ENDDO do_itimerange

END SUBROUTINE recompute_stat_proc_agg_common_exp


! get start of period, end of period and reference time from time,
! timerange, according to time_definition.
SUBROUTINE time_timerange_get_period(time, timerange, time_definition, &
 pstart, pend, reftime)
TYPE(datetime),INTENT(in) :: time
TYPE(vol7d_timerange),INTENT(in) :: timerange
INTEGER,INTENT(in) :: time_definition
TYPE(datetime),INTENT(out) :: reftime
TYPE(datetime),INTENT(out) :: pstart
TYPE(datetime),INTENT(out) :: pend

TYPE(timedelta) :: p1, p2


p1 = timedelta_new(msec=timerange%p1*1000) ! end of period
p2 = timedelta_new(msec=timerange%p2*1000) ! length of period

IF (time == datetime_miss .OR. .NOT.c_e(timerange%p1) .OR. .NOT.c_e(timerange%p2) .OR. &
 (timerange%p1 > 0 .AND. timerange%p1 < timerange%p2) .OR. &
 timerange%p1 < 0 .OR. timerange%p2 < 0) THEN ! is this too pedantic and slow?
  pstart = datetime_miss
  pend = datetime_miss
  reftime = datetime_miss
  RETURN
ENDIF

IF (time_definition == 0) THEN ! time == reference time
  reftime = time
  pend = time + p1
  pstart = pend - p2
ELSE IF (time_definition == 1) THEN ! time == verification time
  pend = time
  pstart = time - p2
  reftime = time - p1
ELSE
  pstart = datetime_miss
  pend = datetime_miss
  reftime = datetime_miss
ENDIF

END SUBROUTINE time_timerange_get_period


! set time, timerange%p1, timerange%p2 according to pstart, pend,
! reftime and time_definition.
SUBROUTINE time_timerange_set_period(time, timerange, time_definition, &
 pstart, pend, reftime)
TYPE(datetime),INTENT(out) :: time
TYPE(vol7d_timerange),INTENT(inout) :: timerange
INTEGER,INTENT(in) :: time_definition
TYPE(datetime),INTENT(in) :: reftime
TYPE(datetime),INTENT(in) :: pstart
TYPE(datetime),INTENT(in) :: pend

TYPE(timedelta) :: p1, p2
INTEGER(kind=int_ll) :: dmsec


IF (time_definition == 0) THEN ! time == reference time
  time = reftime
  p1 = pend - reftime
  p2 = pend - pstart
ELSE IF (time_definition == 1) THEN ! time == verification time
  time = pend
  p1 = pend - reftime
  p2 = pend - pstart
ELSE
  time = datetime_miss
ENDIF

IF (time /= datetime_miss) THEN
  CALL getval(p1, amsec=dmsec) ! end of period
  timerange%p1 = int(dmsec/1000_int_ll)
  CALL getval(p2, amsec=dmsec) ! length of period
  timerange%p2 = int(dmsec/1000_int_ll)
ELSE
  timerange%p1 = imiss
  timerange%p2 = imiss
ENDIF

END SUBROUTINE time_timerange_set_period


END MODULE stat_proc_engine
