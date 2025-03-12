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

! per ogni elemento i,j dell'output, definire n elementi di input ad
! esso contribuenti (arrayof_ttr_mapper) con le seguenti informazioni
TYPE ttr_mapper
  INTEGER :: it=imiss ! k
  INTEGER :: itr=imiss ! l
  INTEGER :: extra_info=imiss ! dtratio for intervals, extreme for point in time
  TYPE(datetime) :: time=datetime_miss ! time for point in time
END TYPE ttr_mapper

INTERFACE OPERATOR (==)
  MODULE PROCEDURE ttr_mapper_eq
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE ttr_mapper_ne
END INTERFACE

INTERFACE OPERATOR (>)
  MODULE PROCEDURE ttr_mapper_gt
END INTERFACE

INTERFACE OPERATOR (<)
  MODULE PROCEDURE ttr_mapper_lt
END INTERFACE

INTERFACE OPERATOR (>=)
  MODULE PROCEDURE ttr_mapper_ge
END INTERFACE

INTERFACE OPERATOR (<=)
  MODULE PROCEDURE ttr_mapper_le
END INTERFACE

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#undef ENABLE_SORT
#define VOL7D_POLY_TYPE TYPE(ttr_mapper)
#define VOL7D_POLY_TYPES _ttr_mapper
#define ENABLE_SORT
#include "array_utilities_pre.F90"

#define ARRAYOF_ORIGTYPE TYPE(ttr_mapper)
#define ARRAYOF_TYPE arrayof_ttr_mapper
#define ARRAYOF_ORIGEQ 1
#define ARRAYOF_ORIGGT 1
#include "arrayof_pre.F90"
! from arrayof


CONTAINS

! simplified comparisons only on time
ELEMENTAL FUNCTION ttr_mapper_eq(this, that) RESULT(res)
TYPE(ttr_mapper),INTENT(IN) :: this, that
LOGICAL :: res

res = this%time == that%time

END FUNCTION ttr_mapper_eq

ELEMENTAL FUNCTION ttr_mapper_ne(this, that) RESULT(res)
TYPE(ttr_mapper),INTENT(IN) :: this, that
LOGICAL :: res

res = this%time /= that%time

END FUNCTION ttr_mapper_ne

ELEMENTAL FUNCTION ttr_mapper_gt(this, that) RESULT(res)
TYPE(ttr_mapper),INTENT(IN) :: this, that
LOGICAL :: res

res = this%time > that%time

END FUNCTION ttr_mapper_gt

ELEMENTAL FUNCTION ttr_mapper_lt(this, that) RESULT(res)
TYPE(ttr_mapper),INTENT(IN) :: this, that
LOGICAL :: res

res = this%time < that%time

END FUNCTION ttr_mapper_lt

ELEMENTAL FUNCTION ttr_mapper_ge(this, that) RESULT(res)
TYPE(ttr_mapper),INTENT(IN) :: this, that
LOGICAL :: res

res = this%time >= that%time

END FUNCTION ttr_mapper_ge

ELEMENTAL FUNCTION ttr_mapper_le(this, that) RESULT(res)
TYPE(ttr_mapper),INTENT(IN) :: this, that
LOGICAL :: res

res = this%time <= that%time

END FUNCTION ttr_mapper_le

#include "arrayof_post.F90"
#include "array_utilities_inc.F90"


! common operations for statistical processing by differences
SUBROUTINE recompute_stat_proc_diff_common(itime, itimerange, stat_proc, step, &
 otime, otimerange, map_tr, f, keep_tr, time_definition, full_steps, &
 start)
TYPE(datetime),INTENT(in) :: itime(:)
TYPE(vol7d_timerange),INTENT(in) :: itimerange(:)
INTEGER,INTENT(in) :: stat_proc
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),POINTER :: otime(:)
TYPE(vol7d_timerange),POINTER :: otimerange(:)
INTEGER,ALLOCATABLE,INTENT(out) :: map_tr(:,:,:,:,:), f(:), keep_tr(:,:,:)
INTEGER :: nitr
LOGICAL,ALLOCATABLE :: mask_timerange(:)
INTEGER,INTENT(in) :: time_definition
LOGICAL,INTENT(in),OPTIONAL :: full_steps
TYPE(datetime),INTENT(in),OPTIONAL :: start

INTEGER :: i, j, k, l, dirtyrep
INTEGER :: steps
LOGICAL :: lfull_steps, useful
TYPE(datetime) :: lstart, pstart1, pstart2, pend1, pend2, reftime1, reftime2, tmptime
TYPE(vol7d_timerange) :: tmptimerange
TYPE(arrayof_datetime) :: a_otime
TYPE(arrayof_vol7d_timerange) :: a_otimerange
TYPE(timedelta) :: start_delta

! compute length of cumulation step in seconds
CALL getval(step, asec=steps)

lstart = datetime_miss
IF (PRESENT(start)) lstart = start
lfull_steps = optio_log(full_steps)

! create a mask of suitable timeranges
ALLOCATE(mask_timerange(SIZE(itimerange)))
mask_timerange(:) = itimerange(:)%timerange == stat_proc &
 .AND. itimerange(:)%p1 /= imiss .AND. itimerange(:)%p2 /= imiss &
 .AND. itimerange(:)%p1 >= 0 &
 .AND. itimerange(:)%p2 > 0

IF (lfull_steps .AND. steps /= 0) THEN ! keep only timeranges defining intervals ending at integer forecast steps or analysis timeranges
  mask_timerange(:) = mask_timerange(:) .AND. &
  (itimerange(:)%p1 == 0 .OR. & ! all analyses
  MOD(itimerange(:)%p1, steps) == 0 .OR. & ! end time is mod step
  MOD(itimerange(:)%p1 - itimerange(:)%p2, steps) == 0) ! start time is mod step
ENDIF
! mask_timerange includes all candidate timeranges

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

! now we have to evaluate time/timerage pairs which do not need processing
ALLOCATE(keep_tr(nitr, SIZE(itime), 2))
CALL compute_keep_tr()

ALLOCATE(map_tr(nitr, SIZE(itime), nitr, SIZE(itime), 2))
map_tr(:,:,:,:,:) = imiss

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
              IF (lfull_steps) THEN
                IF (MOD(reftime2, step) == timedelta_0) THEN
                  useful = .TRUE.
                ENDIF
              ELSE
                useful = .TRUE.
              ENDIF

            ELSE IF (pstart2 < pstart1 .AND. pend2 == pend1) THEN ! -=|
              CALL time_timerange_set_period(tmptime, tmptimerange, &
               time_definition, pstart2, pstart1, pstart1)
              IF (lfull_steps) THEN
                IF (MOD(pstart1, step) == timedelta_0) THEN
                  useful = .TRUE.
                ENDIF
              ELSE
                useful = .TRUE.
              ENDIF
            ENDIF

          ELSE IF (reftime2 == reftime1) THEN ! forecast, same reftime
            IF (c_e(lstart)) THEN
! lstart shifts the interval for computing modulo step, it is not
! really an absolute start but a phase shifter
              start_delta = lstart-reftime2
            ELSE
              start_delta = timedelta_0
            ENDIF

            IF (pstart2 == pstart1 .AND. pend2 > pend1) THEN ! |=-
              CALL time_timerange_set_period(tmptime, tmptimerange, &
               time_definition, pend1, pend2, reftime2)
              IF (lfull_steps) THEN
                IF (MOD(pend2-reftime2-start_delta, step) == timedelta_0) THEN
                  useful = .TRUE.
                ENDIF
              ELSE
                useful = .TRUE.
              ENDIF

            ELSE IF (pstart2 < pstart1 .AND. pend2 == pend1) THEN ! |-=
              CALL time_timerange_set_period(tmptime, tmptimerange, &
               time_definition, pstart2, pstart1, reftime2)
              IF (lfull_steps) THEN
                IF (MOD(pstart1-reftime2-start_delta, step) == timedelta_0) THEN
                  useful = .TRUE.
                ENDIF
              ELSE
                useful = .TRUE.
              ENDIF

            ENDIF
! draft of the second part, to be checked and completed, also in keep_tr
!            IF (c_e(lstart)) THEN ! this should do the real work of start
!              IF (lstart > pstart2) useful = .FALSE. ! pstart1 not checked since we consider only a half triangle of the ((i,j),(k,l)) matrix
!            ENDIF
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

! we have to repeat the computation with sorted arrays
CALL compute_keep_tr()

otime => a_otime%array
otimerange => a_otimerange%array
! delete local objects keeping the contents
CALL delete(a_otime, nodealloc=.TRUE.)
CALL delete(a_otimerange, nodealloc=.TRUE.)

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, map_tr: '//t2c((SIZE(map_tr,1)))//', '// &
 t2c((SIZE(map_tr,2)))//', '// &
 t2c((SIZE(map_tr,3)))//', '// &
 t2c((SIZE(map_tr,4))))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, map_tr: '//t2c((SIZE(map_tr))/2)//', '// &
 t2c(COUNT(c_e(map_tr))/2))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, nitr: '//t2c(nitr))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, good timeranges: '//t2c(COUNT(c_e(keep_tr))/2))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, output times: '//t2c(SIZE(otime)))
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_diff, output timeranges: '//t2c(SIZE(otimerange)))
#endif

CONTAINS

SUBROUTINE compute_keep_tr()
INTEGER :: start_deltas

keep_tr(:,:,:) = imiss
DO l = 1, SIZE(itime)
  itrloop: DO k = 1, nitr
    IF (itimerange(f(k))%p2 == steps) THEN
      CALL time_timerange_get_period(itime(l), itimerange(f(k)), &
       time_definition, pstart2, pend2, reftime2)
      useful = .FALSE.
      IF (c_e(lstart)) THEN
        IF (pstart2 <  lstart) CYCLE itrloop ! too early
      ENDIF
      IF (reftime2 == pend2) THEN ! analysis
        IF (c_e(lstart)) THEN ! in analysis mode start wins over full_steps
          IF (MOD(reftime2-lstart, step) == timedelta_0) THEN
            useful = .TRUE.
          ENDIF
        ELSE IF (lfull_steps) THEN
          IF (MOD(reftime2, step) == timedelta_0) THEN
            useful = .TRUE.
          ENDIF
        ELSE
          useful = .TRUE.
        ENDIF
      ELSE ! forecast
        IF (lfull_steps) THEN
          IF (c_e(lstart)) THEN
            start_deltas = timedelta_getamsec(lstart-reftime2)/1000
          ELSE
            start_deltas = 0
          ENDIF
          IF (MOD(itimerange(f(k))%p1 - start_deltas, steps) == 0) THEN
            useful = .TRUE.
          ENDIF
        ELSE
          useful = .TRUE.
        ENDIF
      ENDIF
      IF (useful) THEN
!        CALL time_timerange_set_period(tmptime, tmptimerange, &
!         time_definition, pstart2, pend2, reftime2)
        keep_tr(k,l,1) = append_unique(a_otime, itime(l))
        keep_tr(k,l,2) = append_unique(a_otimerange, itimerange(f(k)))
      ENDIF
    ENDIF
  ENDDO itrloop
ENDDO

END SUBROUTINE compute_keep_tr

END SUBROUTINE recompute_stat_proc_diff_common


! common operations for statistical processing by metamorphosis
SUBROUTINE compute_stat_proc_metamorph_common(istat_proc, itimerange, ostat_proc, &
 otimerange, map_tr)
INTEGER,INTENT(in) :: istat_proc
TYPE(vol7d_timerange),INTENT(in) :: itimerange(:)
INTEGER,INTENT(in) :: ostat_proc
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


! common operations for statistical processing by aggregation
SUBROUTINE recompute_stat_proc_agg_common(itime, itimerange, stat_proc, tri, &
 step, time_definition, otime, otimerange, map_ttr, dtratio, start, full_steps)
TYPE(datetime),INTENT(in) :: itime(:)
TYPE(vol7d_timerange),INTENT(in) :: itimerange(:)
INTEGER,INTENT(in) :: stat_proc
INTEGER,INTENT(in) :: tri
TYPE(timedelta),INTENT(in) :: step
INTEGER,INTENT(in) :: time_definition
TYPE(datetime),POINTER :: otime(:)
TYPE(vol7d_timerange),POINTER :: otimerange(:)
TYPE(arrayof_ttr_mapper),POINTER :: map_ttr(:,:)
INTEGER,POINTER,OPTIONAL :: dtratio(:)
TYPE(datetime),INTENT(in),OPTIONAL :: start
LOGICAL,INTENT(in),OPTIONAL :: full_steps

INTEGER :: i, j, k, l, na, nf, n
INTEGER :: steps, p1, maxp1, maxp2, minp1mp2, dstart, msteps
INTEGER(kind=int_ll) :: stepms, mstepms
LOGICAL :: lforecast
TYPE(datetime) :: lstart, lend, pstart1, pstart2, pend1, pend2, reftime1, reftime2, tmptime
TYPE(arrayof_datetime) :: a_otime
TYPE(arrayof_vol7d_timerange) :: a_otimerange
TYPE(arrayof_integer) :: a_dtratio
LOGICAL,ALLOCATABLE :: mask_timerange(:) ! improve !!!!
TYPE(ttr_mapper) :: lmapper
CHARACTER(len=8) :: env_var
LOGICAL :: climat_behavior


! enable bad behavior for climat database (only for instantaneous data)
env_var = ''
CALL getenv('LIBSIM_CLIMAT_BEHAVIOR', env_var)
climat_behavior = LEN_TRIM(env_var) > 0 .AND. .NOT.PRESENT(dtratio)

! compute length of cumulation step in seconds
CALL getval(timedelta_depop(step), asec=steps)

! create a mask of suitable timeranges
ALLOCATE(mask_timerange(SIZE(itimerange)))
mask_timerange(:) = itimerange(:)%timerange == tri &
 .AND. itimerange(:)%p1 /= imiss .AND. itimerange(:)%p1 >= 0

IF (PRESENT(dtratio)) THEN
  WHERE(itimerange(:)%p2 > 0 .AND. itimerange(:)%p2 /= imiss) ! for avoiding FPE MOD(steps, 0)
    mask_timerange(:) = mask_timerange(:) .AND. MOD(steps, itimerange(:)%p2) == 0
  ELSEWHERE
    mask_timerange(:) = .FALSE.
  END WHERE
ELSE ! instantaneous
  mask_timerange(:) = mask_timerange(:) .AND. itimerange(:)%p2 == 0
ENDIF

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 '(re)compute_stat_proc_agg, number of useful timeranges before choosing analysis/forecast: '// &
 t2c(COUNT(mask_timerange)))
#endif

! euristically determine whether we are dealing with an
! analysis/observation or a forecast dataset
na = COUNT(mask_timerange(:) .AND. itimerange(:)%p1 == 0)
nf = COUNT(mask_timerange(:) .AND. itimerange(:)%p1 > 0)
lforecast = nf >= na
#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, na: '//t2c(na)//', nf: '//t2c(nf))
#endif

! keep only timeranges of one type (really necessary?)
IF (lforecast) THEN
  CALL l4f_log(L4F_INFO, &
   'recompute_stat_proc_agg, processing in forecast mode')
ELSE
  mask_timerange(:) = mask_timerange(:) .AND. itimerange(:)%p1 == 0
  CALL l4f_log(L4F_INFO, &
   'recompute_stat_proc_agg, processing in analysis mode')
ENDIF

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 '(re)compute_stat_proc_agg, number of useful timeranges: '// &
 t2c(COUNT(mask_timerange)))
#endif

IF (SIZE(itime) == 0 .OR. COUNT(mask_timerange) == 0) THEN ! avoid segmentation fault in case of empty volume
  ALLOCATE(otime(0), otimerange(0), map_ttr(0,0))
  IF (PRESENT(dtratio)) ALLOCATE(dtratio(0))
  RETURN
ENDIF

! determine start and end of processing period, should work with p2==0
lstart = datetime_miss
IF (PRESENT(start)) lstart = start
lend = itime(SIZE(itime))
! compute some quantities
maxp1 = MAXVAL(itimerange(:)%p1, mask=mask_timerange)
maxp2 = MAXVAL(itimerange(:)%p2, mask=mask_timerange)
minp1mp2 = MINVAL(itimerange(:)%p1 - itimerange(:)%p2, mask=mask_timerange)
IF (time_definition == 0) THEN ! reference time
  lend = lend + timedelta_new(sec=maxp1)
ENDIF
! extend interval at the end in order to include all the data in case
! frac_valid<1; must use < and not <= in the DO WHILE loops some lines
! below in order to exclude the last full interval which would be empty
lend = lend + step
IF (lstart == datetime_miss) THEN ! autodetect
  lstart = itime(1)
! if autodetected, adjust to obtain real absolute start of data
  IF (time_definition == 0) THEN ! reference time
    lstart = lstart + timedelta_new(sec=minp1mp2)
  ELSE ! verification time
! go back to start of longest processing interval
    lstart = lstart - timedelta_new(sec=maxp2)
  ENDIF
! full_steps is effective only in analysis mode and when start is not
! specified (start by itself in analysis mode implies full_steps with
! respect to start instead of absolute full steps)
  IF (optio_log(full_steps) .AND. .NOT.lforecast) THEN
    lstart = lstart - (MOD(lstart, step)) ! round to step, (should be MODULO, not MOD)
  ENDIF
ENDIF

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, processing period: '//t2c(lstart)//' - '//t2c(lend))
#endif

! create output time and timerange lists

IF (lforecast) THEN ! forecast mode
  IF (time_definition == 0) THEN ! reference time
    CALL insert(a_otime, itime) ! should I limit to elements itime >= lstart?

! apply start shift to timerange, not to reftime
! why did we use itime(SIZE(itime)) (last ref time)?
!    CALL getval(lstart-itime(SIZE(itime)), asec=dstart)
    CALL getval(lstart-itime(1), asec=dstart)
! allow starting before first reftime but restrict dtstart to -steps
!    dstart = MAX(0, dstart)
    IF (dstart < 0) dstart = MOD(dstart, steps)
    DO p1 = steps + dstart, maxp1, steps
      CALL insert_unique(a_otimerange, vol7d_timerange_new(stat_proc, p1, steps))
    ENDDO

  ELSE ! verification time

! verification time in forecast mode is the ugliest case, because we
! don't know a priori how many different (thus incompatible) reference
! times we have, so some assumption of regularity has to be made. For
! this reason msteps, the minimum step between two times, is
! computed. We choose to compute it as a difference between itime
! elements, it could be also computed as difference of itimerange%p1
! elements. But what if it is not modulo steps?
    mstepms = steps*1000_int_ll
    DO i = 2, SIZE(itime)
      CALL getval(itime(i)-itime(i-1), amsec=stepms)
      IF (stepms > 0_int_ll .AND. stepms < mstepms) THEN
        msteps = stepms/1000_int_ll
        IF (MOD(steps, msteps) == 0) mstepms = stepms
      ENDIF
    ENDDO
    msteps = mstepms/1000_int_ll

    tmptime = lstart + step
    DO WHILE(tmptime < lend) ! < since lend has been extended
      CALL insert_unique(a_otime, tmptime)
      tmptime = tmptime + step
    ENDDO

! in next loop, we used initially steps instead of msteps (see comment
! above), this gave much less combinations of time/timeranges with
! possible empty output; we start from msteps instead of steps in
! order to include partial initial intervals in case frac_valid<1;
! however, as a gemeral rule, for aggregation of forecasts it's better
! to use reference time
    DO p1 = msteps, maxp1, msteps
      CALL insert_unique(a_otimerange, vol7d_timerange_new(stat_proc, p1, steps))
    ENDDO

  ENDIF

ELSE ! analysis mode
  tmptime = lstart + step
  DO WHILE(tmptime < lend) ! < since lend has been extended
    CALL insert_unique(a_otime, tmptime)
    tmptime = tmptime + step
  ENDDO
  CALL insert_unique(a_otimerange, vol7d_timerange_new(stat_proc, 0, steps))

ENDIF

CALL packarray(a_otime)
CALL packarray(a_otimerange)
otime => a_otime%array
otimerange => a_otimerange%array
CALL sort(otime)
CALL sort(otimerange)
! delete local objects keeping the contents
CALL delete(a_otime, nodealloc=.TRUE.)
CALL delete(a_otimerange, nodealloc=.TRUE.)

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, &
 'recompute_stat_proc_agg, output time and timerange: '//&
 t2c(SIZE(otime))//', '//t2c(size(otimerange)))
#endif

IF (PRESENT(dtratio)) THEN
! count the possible i/o interval ratios
  DO k = 1, SIZE(itimerange)
    IF (itimerange(k)%p2 /= 0) &
     CALL insert_unique(a_dtratio, steps/itimerange(k)%p2) ! guaranteed to be integer
  ENDDO
  CALL packarray(a_dtratio)
  dtratio => a_dtratio%array
  CALL sort(dtratio)
! delete local object keeping the contents
  CALL delete(a_dtratio, nodealloc=.TRUE.)

#ifdef DEBUG
  CALL l4f_log(L4F_DEBUG, &
   'recompute_stat_proc_agg, found '//t2c(size(dtratio))// &
   ' possible aggregation ratios, from '// &
   t2c(dtratio(1))//' to '//t2c(dtratio(SIZE(dtratio))))
#endif
  
  ALLOCATE(map_ttr(SIZE(otime),SIZE(otimerange)))
  do_itimerange1: DO l = 1, SIZE(itimerange)
    IF (.NOT.mask_timerange(l)) CYCLE do_itimerange1
    do_itime1: DO k = 1, SIZE(itime)
      CALL time_timerange_get_period(itime(k), itimerange(l), &
       time_definition, pstart1, pend1, reftime1)
      do_otimerange1: DO j = 1, SIZE(otimerange)
        do_otime1: DO i = 1, SIZE(otime)
          CALL time_timerange_get_period_pop(otime(i), otimerange(j), step, &
           time_definition, pstart2, pend2, reftime2)
          IF (lforecast) THEN
            IF (reftime1 /= reftime2) CYCLE do_otime1
          ENDIF

          IF (pstart1 >= pstart2 .AND. pend1 <= pend2 .AND. &
           MOD(pstart1-pstart2, pend1-pstart1) == timedelta_0) THEN ! useful
            lmapper%it = k
            lmapper%itr = l
            lmapper%extra_info = steps/itimerange(l)%p2 ! dtratio, guaranteed to be integer
            n = append(map_ttr(i,j), lmapper)
            CYCLE do_itime1 ! can contribute only to a single interval
          ENDIF
        ENDDO do_otime1
      ENDDO do_otimerange1
    ENDDO do_itime1
  ENDDO do_itimerange1

ELSE
  
  ALLOCATE(map_ttr(SIZE(otime),SIZE(otimerange)))
  do_itimerange2: DO l = 1, SIZE(itimerange)
    IF (.NOT.mask_timerange(l)) CYCLE do_itimerange2
    do_itime2: DO k = 1, SIZE(itime)
      CALL time_timerange_get_period(itime(k), itimerange(l), &
       time_definition, pstart1, pend1, reftime1)
      do_otimerange2: DO j = 1, SIZE(otimerange)
        do_otime2: DO i = 1, SIZE(otime)
          CALL time_timerange_get_period_pop(otime(i), otimerange(j), step, &
           time_definition, pstart2, pend2, reftime2)
          IF (lforecast) THEN
            IF (reftime1 /= reftime2) CYCLE do_otime2
          ENDIF

          IF (climat_behavior .AND. pstart1 == pstart2) CYCLE do_otime2
          IF (pstart1 >= pstart2 .AND. pend1 <= pend2) THEN ! useful
            lmapper%it = k
            lmapper%itr = l
            IF (pstart1 == pstart2) THEN
              lmapper%extra_info = 1 ! start of interval
            ELSE IF (pend1 == pend2) THEN
              lmapper%extra_info = 2 ! end of interval
            ELSE
              lmapper%extra_info = imiss
            ENDIF
            lmapper%time = pstart1 ! = pend1, order by time?
            n = insert_sorted(map_ttr(i,j), lmapper, .TRUE., .TRUE.)
! no CYCLE, a single input can contribute to multiple output intervals
          ENDIF
        ENDDO do_otime2
      ENDDO do_otimerange2
    ENDDO do_itime2
  ENDDO do_itimerange2

ENDIF

END SUBROUTINE recompute_stat_proc_agg_common


SUBROUTINE compute_stat_proc_agg_sw(vertime, pstart, pend, time_mask, &
 max_step, weights)
TYPE(datetime),INTENT(in) :: vertime(:)
TYPE(datetime),INTENT(in) :: pstart
TYPE(datetime),INTENT(in) :: pend
LOGICAL,INTENT(in) :: time_mask(:)
TYPE(timedelta),OPTIONAL,INTENT(out) :: max_step
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: weights(:)

INTEGER :: i, nt
TYPE(datetime),ALLOCATABLE :: lvertime(:)
TYPE(datetime) :: half, nexthalf
INTEGER(kind=int_ll) :: dt, tdt

nt = COUNT(time_mask)
ALLOCATE(lvertime(nt))
lvertime = PACK(vertime, mask=time_mask)

IF (PRESENT(max_step)) THEN
! new way
!  max_step = timedelta_0
!  DO i = 1, nt - 1
!    IF (lvertime(i+1) - lvertime(i) > max_step) &
!     max_step = lvertime(i+1) - lvertime(i)
!  ENDDO
!  IF (lvertime(1) - pstart > max_step) max_step = lvertime(1) - pstart
!  IF (pend - lvertime(nt) > max_step) max_step = pend - lvertime(nt)
! old way
  IF (nt == 1) THEN
    max_step = pend - pstart
  ELSE
    half = lvertime(1) + (lvertime(2) - lvertime(1))/2
    max_step = half - pstart
    DO i = 2, nt - 1
      nexthalf = lvertime(i) + (lvertime(i+1) - lvertime(i))/2
      IF (nexthalf - half > max_step) max_step = nexthalf - half
      half = nexthalf
    ENDDO
    IF (pend - half > max_step) max_step = pend - half
  ENDIF

ENDIF

IF (PRESENT(weights)) THEN
  IF (nt == 1) THEN
    weights(1) = 1.0
  ELSE
    CALL getval(pend - pstart, amsec=tdt)
    half = lvertime(1) + (lvertime(2) - lvertime(1))/2
    CALL getval(half - pstart, amsec=dt)
    weights(1) = DBLE(dt)/DBLE(tdt)
    DO i = 2, nt - 1
      nexthalf = lvertime(i) + (lvertime(i+1) - lvertime(i))/2
      CALL getval(nexthalf - half, amsec=dt)
      weights(i) = DBLE(dt)/DBLE(tdt)
      half = nexthalf
    ENDDO
    CALL getval(pend - half, amsec=dt)
    weights(nt) = DBLE(dt)/DBLE(tdt)
  ENDIF
ENDIF

END SUBROUTINE compute_stat_proc_agg_sw

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


p1 = timedelta_new(sec=timerange%p1) ! end of period
p2 = timedelta_new(sec=timerange%p2) ! length of period

IF (time == datetime_miss .OR. .NOT.c_e(timerange%p1) .OR. .NOT.c_e(timerange%p2) .OR. &
! (timerange%p1 > 0 .AND. timerange%p1 < timerange%p2) .OR. &
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


! get start of period, end of period and reference time from time,
! timerange, according to time_definition. step is taken separately
! from timerange and can be "popular"
SUBROUTINE time_timerange_get_period_pop(time, timerange, step, time_definition, &
 pstart, pend, reftime)
TYPE(datetime),INTENT(in) :: time
TYPE(vol7d_timerange),INTENT(in) :: timerange
TYPE(timedelta),INTENT(in) :: step
INTEGER,INTENT(in) :: time_definition
TYPE(datetime),INTENT(out) :: reftime
TYPE(datetime),INTENT(out) :: pstart
TYPE(datetime),INTENT(out) :: pend

TYPE(timedelta) :: p1


p1 = timedelta_new(sec=timerange%p1) ! end of period

IF (time == datetime_miss .OR. .NOT.c_e(timerange%p1) .OR. .NOT.c_e(timerange%p2) .OR. &
! (timerange%p1 > 0 .AND. timerange%p1 < timerange%p2) .OR. &
 timerange%p1 < 0 .OR. timerange%p2 < 0) THEN ! is this too pedantic and slow?
  pstart = datetime_miss
  pend = datetime_miss
  reftime = datetime_miss
  RETURN
ENDIF

IF (time_definition == 0) THEN ! time == reference time
  reftime = time
  pend = time + p1
  pstart = pend - step
ELSE IF (time_definition == 1) THEN ! time == verification time
  pend = time
  pstart = time - step
  reftime = time - p1
ELSE
  pstart = datetime_miss
  pend = datetime_miss
  reftime = datetime_miss
ENDIF

END SUBROUTINE time_timerange_get_period_pop


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
