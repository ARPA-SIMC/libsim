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
!> \brief Estensione della class vol7d_class per compiere semplici
!! operazioni matematico-statistiche sui volumi.
!!
!! Questo modulo estende vol7d_class aggiungendo operazioni elementari
!! quali la media e la cumulazione di dati in un volume vol7d_class::vol7d.
!! \ingroup vol7d
!!
!! \todo estendere l'operazione di media anche a dati istantanei.
MODULE vol7d_class_compute
USE datetime_class
USE vol7d_class

IMPLICIT NONE

CONTAINS


!> General-purpose method for recomputing a statistical processing on
!! a set of data already processed with the same statistical
!! processing, on a different time interval.  This method does not
!! change the statistical processing (timerange) of the data (unless
!! \a stat_proc_input is provided, see vol7d_recompute_stat_proc_agg()
!! ), it just recomputes it on a specified interval, different from
!! the one over which data is provided.  Only floating point single or
!! double precision data are processed.  Statistical processing by
!! aggregation of shorter intervals and statistical processing by
!! difference of partially overlapping intervals are tried in turn. If
!! only one of the two operations is desired, use the specific method
!! \a vol7d_recompute_stat_proc_agg() or \a
!! vol7d_recompute_stat_proc_diff(). In case one or both operations
!! cannot be performed a warning is issued, but the program continues.
!!
!! See also the description of vol7d_recompute_stat_proc_agg() and
!! vol7d_recompute_stat_proc_diff() methods for more details.
SUBROUTINE vol7d_recompute_stat_proc(this, that, stat_proc, &
 step, start, full_steps, frac_valid, other, stat_proc_input)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if \a .TRUE. cumulate only on intervals starting at a forecast time modulo \a step (default is to cumulate on all possible combinations of intervals)
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to be merged with the data that did not contribute to the accumulation computation
INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< to be used with care, type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be recomputed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument

TYPE(vol7d) :: that1, that2, other1

IF (PRESENT(other)) THEN
  CALL vol7d_recompute_stat_proc_agg(this, that1, stat_proc, &
   step, start, frac_valid, other=other1, stat_proc_input=stat_proc_input)
!  CALL init(that2, time_definition=this%time_definition)
  CALL vol7d_extend_cumavg_diff(other1, that2, stat_proc, step, full_steps, other=other)
!  CALL vol7d_recompute_stat_proc_diff(other1, that2, stat_proc, step, full_steps, other=other)
ELSE
  CALL vol7d_recompute_stat_proc_agg(this, that1, stat_proc, &
   step, start, frac_valid, stat_proc_input=stat_proc_input)
!  CALL init(that2, time_definition=this%time_definition)
  CALL vol7d_extend_cumavg_diff(this, that2, stat_proc, step, full_steps)
!  CALL vol7d_recompute_stat_proc_diff(this, that2, stat_proc, step, full_steps)
ENDIF

CALL vol7d_merge(that1, that2, sort=.TRUE.)
that = that1

END SUBROUTINE vol7d_recompute_stat_proc


!> Specialized method for statistically processing a set of data
!! already processed with the same statistical processing, on a
!! different time interval.  This method performs statistical
!! processing by aggregation of shorter intervals.  Only floating
!! point single or double precision data are processed.
!!
!! The output \a that vol7d object contains elements from the original volume
!! \a this satisfying the conditions
!!  - real single or double precision variables
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type \a stat_proc (or \a stat_proc_input if provided)
!!  - p1 = 0 (end of period == reference time)
!!  - p2 > 0 (processing interval non null, non instantaneous data)
!!    and equal to a multiplier of \a step
!!
!! Output data will have timerange of type \a stat_proc, p1 = 0 and p2
!! = \a step.  The supported statistical processing methods (parameter
!! \a stat_proc) are:
!!
!!  - 0 average
!!  - 1 cumulation
!!  - 2 maximum
!!  - 3 minimum
!!
!! The purpose of the optional argument \a stat_proc_input is to allow
!! processing with a certain statistical processing operator a dataset
!! already processed with a different operator, by specifying the
!! latter as stat_proc_input; this is useful, for example, if one
!! wants to compute the monthly average of daily maximum temperatures;
!! however this has to be used with care since the resulting data
!! volume will not carry all the information about the processing
!! which has been done, in the previous case, for example, the
!! temperatures will simply look like monthly average temperatures.
SUBROUTINE vol7d_recompute_stat_proc_agg(this, that, stat_proc, &
 step, start, frac_valid, other, stat_proc_input)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to be merged with the data that did not contribute to the accumulation computation
INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< to be used with care, type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be recomputed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument

INTEGER :: tri
INTEGER i, j, n, i1, i3, i5, i6
INTEGER,POINTER :: map_tr(:), map_trc(:,:), count_trc(:,:)
REAL :: lfrac_valid, frac_c, frac_m
LOGICAL,ALLOCATABLE :: mask_time(:)
TYPE(vol7d_timerange) :: otimerange
TYPE(vol7d) :: v7dtmp


IF (PRESENT(stat_proc_input)) THEN
  tri = stat_proc_input
ELSE
  tri = stat_proc
ENDIF
IF (PRESENT(frac_valid)) THEN
  lfrac_valid = frac_valid
ELSE
  lfrac_valid = 1.0
ENDIF

CALL init(that, time_definition=this%time_definition)
! be safe
CALL vol7d_alloc_vol(this)
! initial check
IF (COUNT(this%timerange(:)%timerange == tri .AND. this%timerange(:)%p2 /= imiss &
 .AND. this%timerange(:)%p2 /= 0 .AND. this%timerange(:)%p1 == 0) == 0) THEN
  CALL l4f_log(L4F_WARN, &
   'vol7d_compute, no timeranges suitable for statistical processing by aggregation')
  CALL makeother()
  RETURN
ENDIF

! cleanup the original volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.) ! time-ordered volume needed
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)

! compute the output time and timerange and all the required mappings
CALL recompute_stat_proc_agg_common(this%time, this%timerange, stat_proc, tri, &
 step, that%time, otimerange, map_tr, map_trc, count_trc, start)

! create a template for the new volume with the elements that may not
! be present in the original one, that%time is already allocated
CALL vol7d_alloc(that, nana=0, nlevel=0, ntimerange=1, nnetwork=0)
that%timerange(1) = otimerange
CALL vol7d_alloc_vol(that)

! copy the elements of the original volume that may be useful for the
! new volume into a temporary object, this is usually useless
ALLOCATE(mask_time(SIZE(this%time)))
DO i = 1, SIZE(this%time)
  mask_time(i) = ANY(this%time(i) == that%time)
ENDDO
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
 ltimerange=(this%timerange(:) == that%timerange(1)), ltime=mask_time)
! merge with template
CALL vol7d_merge(that, v7dtmp)

! finally perform computations
! warning: mask_time is reused for a different purpose
IF (ASSOCIATED(this%voldatir)) THEN
  DO i = 1, SIZE(that%time)
    DO i1 = 1, SIZE(this%ana)
      DO i3 = 1, SIZE(this%level)
        DO i6 = 1, SIZE(this%network)
          DO i5 = 1, SIZE(this%dativar%r)
            frac_m = 0.
            DO j = 1, SIZE(map_tr)
! count the number of valid data that contribute to the current interval
              mask_time = this%voldatir(i1,:,i3,map_tr(j),i5,i6) /= rmiss .AND. &
               map_trc(:,j) == i
              n = COUNT(mask_time)
! compute the ratio between n. of valid data and n. of required data (fraction)
              frac_c = REAL(n)/count_trc(i,j)
! keep the timerange giving the maximum fraction
              IF (n > 0 .AND. frac_c >= MAX(lfrac_valid, frac_m)) THEN
                frac_m = frac_c
                IF (stat_proc == 0) THEN ! average
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)/n
                ELSE IF (stat_proc == 1) THEN ! cumulation
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (stat_proc == 2) THEN ! maximum
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   MAXVAL(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (stat_proc == 3) THEN ! minimum
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   MINVAL(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
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
  DO i = 1, SIZE(that%time)
    DO i1 = 1, SIZE(this%ana)
      DO i3 = 1, SIZE(this%level)
        DO i6 = 1, SIZE(this%network)
          DO i5 = 1, SIZE(this%dativar%d)
            frac_m = 0.
            DO j = 1, SIZE(map_tr)
! count the number of valid data that contribute to the current interval
              mask_time = this%voldatid(i1,:,i3,map_tr(j),i5,i6) /= rdmiss .AND. &
               map_trc(:,j) == i
              n = COUNT(mask_time)
! compute the ratio between n. of valid data and n. of required data (fraction)
              frac_c = REAL(n)/count_trc(i,j)
! keep the timerange giving the maximum fraction
              IF (n > 0 .AND. frac_c >= MAX(lfrac_valid, frac_m)) THEN
                frac_m = frac_c
                IF (stat_proc == 0) THEN ! average
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)/n
                ELSE IF (stat_proc == 1) THEN ! cumulation
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (stat_proc == 2) THEN ! maximum
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   MAXVAL(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (stat_proc == 3) THEN ! minimum
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   MINVAL(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
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

CALL makeother()

CONTAINS

SUBROUTINE makeother()
IF (PRESENT(other)) THEN ! create volume with the remaining data for further processing
  CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(this%timerange(:)%timerange /= tri .OR. this%timerange(:)%p2 == imiss &
   .OR. this%timerange(:)%p2 == 0 .OR. this%timerange(:)%p1 /= 0))
ENDIF
END SUBROUTINE makeother

END SUBROUTINE vol7d_recompute_stat_proc_agg


! internal subroutine which may be useful also for statistically
! processing volgrid6d objects
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

INTEGER :: i, j, k, nval, ncum, ntr, steps, nstep
LOGICAL :: usestart, tr_mask(SIZE(itimerange))
TYPE(datetime) :: lstart, lend, tmptime, tmptimes, t1, t2
TYPE(timedelta) :: dt1, stepvero


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
  i = MINVAL(itimerange(:)%p2, mask=tr_mask(:))

  CALL init(dt1, minute=-i/60) ! use msec
  lstart = itime(1)+dt1 ! go back by dt1 (dt1 < 0!)
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
CALL getval(step, aminute=steps)
CALL init(otimerange, timerange=stat_proc, p1=0, p2=steps*60)

nval = 0
DO j = 1, SIZE(itimerange)
  IF (.NOT.tr_mask(j)) CYCLE

  nval = nval + 1
  map_tr(nval) = j ! mappatura per ottimizzare il successivo ciclo sui timerange
  CALL init(dt1, minute=itimerange(j)%p2/60) ! usare msec

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


!> Method for statistically processing a set of instantaneous data.
!! This method performs statistical processing by aggregation of
!! instantaneous data.  Only floating point single or double precision
!! data are processed.
!!
!! The output \a that vol7d object contains elements from the original volume
!! \a this satisfying the conditions
!!  - real single or double precision variables
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type 254 (instantaeous)
!!  - p1 = 0 (end of period == reference time, analysis/observation)
!!  - p2 = 0 (processing interval null, instantaneous data)
!!
!! Output data will have timerange of type \a stat_proc, p1 = 0 and p2
!! = \a step.  The supported statistical processing methods (parameter
!! \a stat_proc) are:
!!
!!  - 0 average
!!  - 2 maximum
!!  - 3 minimum
!!
!! In the case of average, it is possible to weigh the data
!! proportionally to the length of the time interval for which every
!! single value is valid, i.e. halfway between the time level of the
!! value itself and the time of its nearest valid neighbours. A
!! maximum distance in time for input valid data can be assigned in
!! order to filter datasets with too long "holes".
SUBROUTINE vol7d_compute_stat_proc_agg(this, that, stat_proc, &
 step, start, max_step, weighted, other)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be computed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the computed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be computed (from grib2 table)
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
TYPE(timedelta),INTENT(in),OPTIONAL :: max_step ! maximum allowed distance in time between two single valid data within a dataset, for the dataset to be eligible for statistical processing
LOGICAL,INTENT(in),OPTIONAL :: weighted !< if provided and \c .TRUE., the statistical process is computed, if possible, by weighting every value with a weight proportional to its validity interval
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to be merged with the data that did not contribute to the accumulation computation
!INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< to be used with care, type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be recomputed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument

INTEGER :: tri, itr, iw, iwn, i, i1, i3, i5, i6
REAL,ALLOCATABLE :: stepmsec(:)
INTEGER(kind=int_ll) :: dmsec, stepmseci
TYPE(datetime) :: w_start, w_end
TYPE(timedelta) :: sub_step, lmax_step
TYPE(vol7d_timerange) :: otimerange
TYPE(vol7d) :: v7dtmp
INTEGER,POINTER :: itime_start(:), itime_end(:)
REAL,ALLOCATABLE :: weightr(:)
DOUBLE PRECISION,ALLOCATABLE :: weightd(:)
LOGICAL,ALLOCATABLE :: mask_time(:)
LOGICAL :: lweighted

IF (PRESENT(max_step)) THEN
  lmax_step = max_step
ELSE
  lmax_step = timedelta_max
ENDIF
lweighted = optio_log(weighted)
tri = 254

CALL init(that, time_definition=this%time_definition)
! be safe
CALL vol7d_alloc_vol(this)
! initial check
itr = index(this%timerange(:), vol7d_timerange_new(tri, 0, 0))
IF (itr <= 0) THEN
  CALL l4f_log(L4F_WARN, &
   'vol7d_compute, no timeranges suitable for statistical processing by aggregation')
  CALL makeother()
  RETURN
ENDIF

! cleanup the original volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.) ! time-ordered volume needed
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)
! recompute itr, it may have changed
itr = index(this%timerange(:), vol7d_timerange_new(tri, 0, 0))

! compute the output time and timerange and all the required mappings
CALL compute_stat_proc_agg_common(this%time, stat_proc, &
 step, that%time, otimerange, itime_start, itime_end, start)

! create a template for the new volume with the elements that may not
! be present in the original one, that%time is already allocated
CALL vol7d_alloc(that, nana=0, nlevel=0, ntimerange=1, nnetwork=0)
that%timerange(1) = otimerange
CALL vol7d_alloc_vol(that)

! copy the elements of the original volume that may be useful for the
! new volume into a temporary object, this is usually useless
ALLOCATE(mask_time(SIZE(this%time)))
DO i = 1, SIZE(this%time)
  mask_time(i) = ANY(this%time(i) == that%time)
ENDDO
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
 ltimerange=(this%timerange(:) == that%timerange(1)), ltime=mask_time)
! merge with template
CALL vol7d_merge(that, v7dtmp)

! compute step length taking into account "pop" intervals
ALLOCATE(stepmsec(SIZE(that%time)))
DO i = 1, SIZE(that%time)
  CALL getval(that%time(i)-(that%time(i)-step), amsec=stepmseci)
  stepmsec(i) = stepmseci
ENDDO

! finally perform computations
! warning: mask_time is reused for a different purpose
IF (ASSOCIATED(this%voldatir)) THEN
  ALLOCATE(weightr(SIZE(this%time)))
  DO i6 = 1, SIZE(this%network)
    DO i5 = 1, SIZE(this%dativar%r)
      DO i3 = 1, SIZE(this%level)
        DO i1 = 1, SIZE(this%ana)
          mask_time(:) = this%voldatir(i1,:,i3,itr,i5,i6) /= rmiss
          weightr(:) = rmiss
          timeloopr: DO i = 1, SIZE(that%time)

! start of first interval = start of output interval
            w_start = that%time(i) - step
! search first valid value index (iw)
            iw = itime_start(i)
            DO WHILE(iw <= itime_end(i))
              IF (mask_time(iw)) EXIT
              iw = iw + 1
            ENDDO

! loop on input data in output interval
            DO WHILE(iw <= itime_end(i))
! search next valid value index (iwn)
              iwn = iw + 1
              DO WHILE(iwn <= itime_end(i))
                IF (mask_time(iwn)) EXIT
                iwn = iwn + 1
              ENDDO
              IF (iwn > itime_end(i)) THEN ! next is the last
! end of last interval = end of output interval
                w_end = that%time(i) ! 
              ELSE ! next is not the last
! end of interval = midpoint between this and next value
                w_end = this%time(iw) + (this%time(iwn)-this%time(iw))/2
              ENDIF
! compute length of interval associated to current value
              sub_step = w_end - w_start
              IF (sub_step > lmax_step) CYCLE timeloopr ! output interval rejected
              IF (lweighted) THEN ! we should optimize for the unweighted case 
! compute relative weight
                CALL getval(sub_step, amsec=dmsec)
                weightr(iw) = REAL(dmsec)/stepmsec(i)
              ENDIF
! next becomes current
              iw = iwn
              w_start = w_end
            ENDDO

            IF (COUNT(mask_time(itime_start(i):itime_end(i))) > 0) THEN
              IF (stat_proc == 0) THEN ! average

                IF (lweighted) THEN
                  WHERE(mask_time(itime_start(i):itime_end(i)))
                    weightr(itime_start(i):itime_end(i)) = &
                     weightr(itime_start(i):itime_end(i)) * &
                     this%voldatir(i1,itime_start(i):itime_end(i),i3,itr,i5,i6)
                  END WHERE
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(weightr(itime_start(i):itime_end(i)), &
                   mask=mask_time(itime_start(i):itime_end(i)))

                ELSE
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                   mask=mask_time(itime_start(i):itime_end(i)))/ &
                   COUNT(mask_time(itime_start(i):itime_end(i)))
                ENDIF

              ELSE IF (stat_proc == 2) THEN ! maximum

                that%voldatir(i1,i,i3,1,i5,i6) = &
                 MAXVAL( &
                 this%voldatir(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                 mask=mask_time(itime_start(i):itime_end(i)))

              ELSE IF (stat_proc == 3) THEN ! minimum

                that%voldatir(i1,i,i3,1,i5,i6) = &
                 MINVAL( &
                 this%voldatir(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                 mask=mask_time(itime_start(i):itime_end(i)))

              ENDIF
            ENDIF
          ENDDO timeloopr
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  DEALLOCATE(weightr)
ENDIF

! finally perform computations
! warning: mask_time is reused for a different purpose
IF (ASSOCIATED(this%voldatid)) THEN
  ALLOCATE(weightd(SIZE(this%time)))
  DO i6 = 1, SIZE(this%network)
    DO i5 = 1, SIZE(this%dativar%r)
      DO i3 = 1, SIZE(this%level)
        DO i1 = 1, SIZE(this%ana)
          mask_time(:) = this%voldatid(i1,:,i3,itr,i5,i6) /= rmiss
          weightd(:) = dmiss
          timeloopd: DO i = 1, SIZE(that%time)

! start of first interval = start of output interval
            w_start = that%time(i) - step
! search first valid value index (iw)
            iw = itime_start(i)
            DO WHILE(iw <= itime_end(i))
              IF (mask_time(iw)) EXIT
              iw = iw + 1
            ENDDO

! loop on input data in output interval
            DO WHILE(iw <= itime_end(i))
! search next valid value index (iwn)
              iwn = iw + 1
              DO WHILE(iwn <= itime_end(i))
                IF (mask_time(iwn)) EXIT
                iwn = iwn + 1
              ENDDO
              IF (iwn > itime_end(i)) THEN ! next is the last
! end of last interval = end of output interval
                w_end = that%time(i) ! 
              ELSE ! next is not the last
! end of interval = midpoint between this and next value
                w_end = this%time(iw) + (this%time(iwn)-this%time(iw))/2
              ENDIF
! compute length of interval associated to current value
              sub_step = w_end - w_start
              IF (sub_step > lmax_step) CYCLE timeloopd ! output interval rejected
              IF (lweighted) THEN ! we should optimize for the unweighted case 
! compute relative weight
                CALL getval(sub_step, amsec=dmsec)
                weightd(iw) = DBLE(dmsec)/stepmsec(i)
              ENDIF
! next becomes current
              iw = iwn
              w_start = w_end
            ENDDO

            IF (COUNT(mask_time(itime_start(i):itime_end(i))) > 0) THEN
              IF (stat_proc == 0) THEN ! average

                IF (lweighted) THEN
                  WHERE(mask_time(itime_start(i):itime_end(i)))
                    weightd(itime_start(i):itime_end(i)) = &
                     weightd(itime_start(i):itime_end(i)) * &
                     this%voldatid(i1,itime_start(i):itime_end(i),i3,itr,i5,i6)
                  END WHERE
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(weightd(itime_start(i):itime_end(i)), &
                   mask=mask_time(itime_start(i):itime_end(i)))

                ELSE
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                   mask=mask_time(itime_start(i):itime_end(i)))/ &
                   COUNT(mask_time(itime_start(i):itime_end(i)))
                ENDIF

              ELSE IF (stat_proc == 2) THEN ! maximum

                that%voldatid(i1,i,i3,1,i5,i6) = &
                 MAXVAL( &
                 this%voldatid(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                 mask=mask_time(itime_start(i):itime_end(i)))

              ELSE IF (stat_proc == 3) THEN ! minimum

                that%voldatid(i1,i,i3,1,i5,i6) = &
                 MINVAL( &
                 this%voldatid(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                 mask=mask_time(itime_start(i):itime_end(i)))

              ENDIF
            ENDIF
          ENDDO timeloopd
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  DEALLOCATE(weightd)
ENDIF

DEALLOCATE(stepmsec, mask_time, itime_start, itime_end)

CALL makeother()

CONTAINS

SUBROUTINE makeother()
IF (PRESENT(other)) THEN ! create volume with the remaining data for further processing
  CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(this%timerange(:)%timerange /= tri))
ENDIF
END SUBROUTINE makeother

END SUBROUTINE vol7d_compute_stat_proc_agg


SUBROUTINE compute_stat_proc_agg_common(itime, stat_proc, &
 step, otime, otimerange, itime_start, itime_end, start)
TYPE(datetime),INTENT(in) :: itime(:)
INTEGER,INTENT(in) :: stat_proc
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),POINTER :: otime(:)
TYPE(vol7d_timerange),INTENT(out) :: otimerange
INTEGER,POINTER :: itime_start(:), itime_end(:)
TYPE(datetime),INTENT(in),OPTIONAL :: start

INTEGER :: i, j, nstep, steps
LOGICAL :: usestart
TYPE(datetime) :: lstart, lend, tmptime


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
  IF (itime(j) >= tmptime) itime_start(i) = j

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
CALL getval(step, aminute=steps)
CALL init(otimerange, timerange=stat_proc, p1=0, p2=steps*60)

END SUBROUTINE compute_stat_proc_agg_common


!> Cumula le osservazioni su un intervallo specificato quando possibile.
!! Crea un nuovo oggetto vol7d che contiene solo i dati del volume originario
!! soddisfacenti le condizioni:
!!  - variabili di tipo reale o doppia precisione
!!  - cumulabilità per aggregazione di osservazioni:
!!    - intervallo temporale di tipo "cumulazione"
!!      (vol7d_timerange_class::vol7d_timerange::timerange = 1)
!!      da un tempo passato al tempo attuale
!!    - intervallo di cumulazione che sia uguale o un sottomultiplo dell'intervallo
!!      di cumulazione desiderato \a step
!!  - oppure cumulabilità per dfferenza di previsioni:
!!    - \a time_definition=0 (reference time)
!!    - intervallo temporale di tipo "cumulazione"
!!      (vol7d_timerange_class::vol7d_timerange::timerange = 1)
!!      nel futuro (previsione)
!!    - intervallo di cumulazione che sia &gt;= all'intervallo
!!      di cumulazione desiderato \a step
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
!! \a full_steps=.TRUE. permette di evitare la creazione di cumulate previste
!! su intervalli non modulo il passo di cumulazione desiderato, ad es. per
!! evitare di generare dati cumulati tra +2 e +5 ore avendo a disposizione
!! previsioni con passo orario di cumulazione dall'inizio.
!!
!! \todo il parametro \a this è dichiarato \a INOUT perché la vol7d_alloc_vol
!! può modificarlo, bisognerebbe implementare una vol7d_check_vol che restituisca
!! errore anziché usare la vol7d_alloc_vol.
SUBROUTINE vol7d_cumulate(this, that, step, start, full_steps, frac_valid, other)
TYPE(vol7d),INTENT(inout) :: this !< oggetto da cumulare, non viene modificato dal metodo
TYPE(vol7d),INTENT(out) :: that !< oggetto contenente, in uscita, i valori cumulati
TYPE(timedelta),INTENT(in) :: step !< intervallo di cumulazione
TYPE(datetime),INTENT(in),OPTIONAL :: start !< inizio del periodo di cumulazione
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if \a .TRUE. cumulate only on intervals starting at a forecast time modulo \a step (default is to cumulate on all possible combinations of intervals)
REAL,INTENT(in),OPTIONAL :: frac_valid !< frazione minima di dati validi necessaria per considerare accettabile un dato cumulato, default=1
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< opional volume that, on exit, is going to be merged with the data that did not contribute to the accumulation computation

TYPE(vol7d) :: that1, that2, other1

IF (PRESENT(other)) THEN
  CALL vol7d_extend_cumavg_sum(this, that1, 1, step, start, frac_valid, other=other1)
  CALL vol7d_extend_cumavg_diff(other1, that2, 1, step, full_steps, other=other)
ELSE
  CALL vol7d_extend_cumavg_sum(this, that1, 1, step, start, frac_valid)
  CALL vol7d_extend_cumavg_diff(this, that2, 1, step, full_steps)
ENDIF

CALL vol7d_merge(that1, that2, sort=.TRUE.)
that = that1

END SUBROUTINE vol7d_cumulate


!> Media le osservazioni su un intervallo specificato quando possibile.
!! Funziona esattamente come il metodo ::vol7d_cumulate ma agisce
!! sui dati aventi un timerange di tipo "media"
!! (vol7d_timerange_class::vol7d_timerange::timerange = 0) e ne calcola
!! la media sull'intervallo specificato.
!!
!! \todo il parametro \a this è dichiarato \a INOUT perché la vol7d_alloc_vol
!! può modificarlo, bisognerebbe implementare una vol7d_check_vol che restituisca
!! errore anziché usare la vol7d_alloc_vol.
SUBROUTINE vol7d_average(this, that, step, start, full_steps, frac_valid, other)
TYPE(vol7d),INTENT(inout) :: this !< oggetto da mediare, non viene modificato dal metodo
TYPE(vol7d),INTENT(out) :: that !< oggetto contenente, in uscita, i valori mediati
TYPE(timedelta),INTENT(in) :: step !< intervallo di media
TYPE(datetime),INTENT(in),OPTIONAL :: start !< inizio del periodo di media
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if \a .TRUE. average only on intervals starting at a forecast time modulo \a step (default is to average on all possible combinations of intervals)
REAL,INTENT(in),OPTIONAL :: frac_valid !< frazione minima di dati validi necessaria per considerare accettabile un dato mediato, default=1
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< opional volume that, on exit, is going to be merged with the data that did not contribute to the average computation

TYPE(vol7d) :: that1, that2, other1

IF (PRESENT(other)) THEN
  CALL vol7d_extend_cumavg_sum(this, that1, 0, step, start, frac_valid, other=other1)
  CALL vol7d_extend_cumavg_diff(other1, that2, 0, step, full_steps, other=other)
ELSE
  CALL vol7d_extend_cumavg_sum(this, that1, 0, step, start, frac_valid)
  CALL vol7d_extend_cumavg_diff(this, that2, 0, step, full_steps)
ENDIF

CALL vol7d_merge(that1, that2, sort=.TRUE.)
that = that1

END SUBROUTINE vol7d_average


! Statistically process the data having time range indicator =tri
! recomputing the values over an interval =step by aggregating data
! statistically processed over shorter periods.
SUBROUTINE vol7d_extend_cumavg_sum(this, that, tri, step, start, frac_valid, other)
TYPE(vol7d),INTENT(inout) :: this
TYPE(vol7d),INTENT(out) :: that
INTEGER,INTENT(in) :: tri
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),INTENT(in),OPTIONAL :: start
REAL,INTENT(in),OPTIONAL :: frac_valid
TYPE(vol7d),INTENT(out),OPTIONAL :: other

TYPE(datetime) :: lstart, lend, tmptime, tmptimes, t1, t2
TYPE(timedelta) dt1, stepvero
INTEGER :: steps, ntr, nstep, ncum, nval, i, j, k, i1, i3, i5, i6, n
INTEGER,ALLOCATABLE :: map_tr(:), map_trc(:,:), count_trc(:,:)
LOGICAL,ALLOCATABLE :: mask_time(:)
REAL :: lfrac_valid, frac_c, frac_m
TYPE(vol7d) :: v7dtmp
LOGICAL usestart

CALL init(that, time_definition=this%time_definition)
IF (PRESENT(frac_valid)) THEN
  lfrac_valid = frac_valid
ELSE
  lfrac_valid = 1.0
ENDIF

! be safe
CALL vol7d_alloc_vol(this)
! count timeranges of type statistical processing for observed data
ntr = COUNT(this%timerange(:)%timerange == tri .AND. this%timerange(:)%p2 /= imiss &
 .AND. this%timerange(:)%p2 /= 0 .AND. this%timerange(:)%p1 == 0)
IF (ntr == 0) THEN
  CALL l4f_log(L4F_WARN, &
   'vol7d_compute, no timeranges suitable for statistical processing by sum')
  CALL makeother()
  RETURN
ENDIF
! cleanup the original volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.) ! time-ordered volume needed
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)
! recount timeranges, they could have diminished because of unique
ntr = COUNT(this%timerange(:)%timerange == tri .AND. this%timerange(:)%p2 /= imiss &
 .AND. this%timerange(:)%p2 /= 0 .AND. this%timerange(:)%p1 == 0)

! determino lstart
! lstart e` l'inizio (non il termine) del primo periodo di cumulazione
usestart = PRESENT(start) ! treat datetime_miss as .NOT.PRESENT()
IF (usestart) usestart = usestart .AND. start /= datetime_miss
IF (usestart) THEN ! start fornito esplicitamente
  lstart = start
ELSE ! calcolo automatico di start
! calcolo il piu` breve intervallo di cumulazione disponibile
! potrei usare il piu` lungo se volessi (avrei piu` dati ma peggiori)
  i = MINVAL(this%timerange(:)%p2, mask=(this%timerange(:)%timerange == tri .AND. &
   this%timerange(:)%p2 /= imiss .AND. this%timerange(:)%p2 /= 0 .AND. this%timerange(:)%p1 == 0))

  CALL init(dt1, minute=-i/60) ! usare msec
  lstart = this%time(1)+dt1 ! torno indietro di dt1 (dt1 < 0!)
  lstart = lstart-(MOD(lstart, step)) ! arrotondo a step, controllare il -!!!
ENDIF
lend = this%time(SIZE(this%time))

! conto il numero di time necessari in uscita
! lo faccio passo-passo e non con una divisione
! per farlo funzionare anche con le cumulate "umane"
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
CALL vol7d_alloc(that, nana=0, ntime=nstep, nlevel=0, ntimerange=1, nnetwork=0)
CALL vol7d_alloc_vol(that)
! ripeto il ciclo per assegnare i time
tmptime = lstart+step
DO i = 1, nstep
  that%time(i) = tmptime ! le cumulate sono valide alla fine
  tmptime = tmptime + step
ENDDO
CALL getval(step, aminute=steps)
steps = steps*60
CALL init(that%timerange(1), timerange=tri, p1=0, p2=steps) ! modificare eventualmente p1

! Faccio una copia del volume originale eliminando da essa quello che non serve
! attenzione uso mask_time
DO i = 1, SIZE(this%time)
  mask_time(i) = ANY(this%time(i) == that%time)
ENDDO
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
 ltimerange=(this%timerange(:) == that%timerange(1)), ltime=mask_time)
! Infine fondo quanto rimasto del volume originale con la bozza del nuovo volume
CALL vol7d_merge(that, v7dtmp)

nval = 0
DO j = 1, SIZE(this%timerange)
  IF (this%timerange(j)%timerange /= tri .OR. this%timerange(j)%p2 == imiss &
   .OR. this%timerange(j)%p2 == 0 .OR. this%timerange(j)%p1 /= 0) CYCLE

  nval = nval + 1
  map_tr(nval) = j ! mappatura per ottimizzare il successivo ciclo sui timerange
!  CALL init(dt1, minute=-this%timerange(j)%p2/60) ! usare msec
  CALL init(dt1, minute=this%timerange(j)%p2/60) ! usare msec

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
  DO i = 1, SIZE(this%time)
    t1 = this%time(i) - dt1
    t2 = this%time(i)
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
! attenzione riuso mask_time
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
                IF (tri == 0) THEN ! average
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)/n
                ELSE IF (tri == 1) THEN ! cumulation
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (tri == 2) THEN ! maximum
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   MAXVAL(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (tri == 3) THEN ! minimum
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   MINVAL(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
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
          DO i5 = 1, SIZE(this%dativar%d)
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
                IF (tri == 0) THEN ! average
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)/n
                ELSE IF (tri == 1) THEN ! cumulation
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (tri == 2) THEN ! maximum
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   MAXVAL(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (tri == 3) THEN ! minimum
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   MINVAL(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
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

CALL makeother()

CONTAINS

SUBROUTINE makeother()
IF (PRESENT(other)) THEN ! create volume with the remaining data for further processing
  CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(this%timerange(:)%timerange /= tri .OR. this%timerange(:)%p2 == imiss &
   .OR. this%timerange(:)%p2 == 0 .OR. this%timerange(:)%p1 /= 0))
ENDIF
END SUBROUTINE makeother

END SUBROUTINE vol7d_extend_cumavg_sum


! Statistically process the data having time range indicator =tri
! recomputing the values over an interval =step by disaggregating data
! statistically processed over longer periods.
SUBROUTINE vol7d_extend_cumavg_diff(this, that, tri, step, full_steps, other)
TYPE(vol7d),INTENT(inout) :: this
TYPE(vol7d),INTENT(out) :: that
INTEGER,INTENT(in) :: tri
TYPE(timedelta),INTENT(in) :: step
LOGICAL,INTENT(in),OPTIONAL :: full_steps
TYPE(vol7d),INTENT(out),OPTIONAL :: other

TYPE(vol7d_timerange),ALLOCATABLE :: tr(:)
INTEGER :: steps, ntr, ndtr, i, j, i1, i2, i3, i4, i5, i6, n
INTEGER(kind=int_ll) :: msteps
INTEGER,ALLOCATABLE :: map_tr(:,:)
LOGICAL,ALLOCATABLE :: mask_timerange(:)
!LOGICAL :: lfull_steps
TYPE(vol7d) :: v7dtmp

CALL init(that, time_definition=this%time_definition)
IF (this%time_definition == 1) THEN ! improve and handle time_definition == 1
  CALL l4f_log(L4F_WARN, &
   'vol7d_extend_cumavg_diff, cumulation by differences not implemented with &
   &time_definition == 1')
  CALL makeother(.FALSE.) ! a useless copy is done here, improve!?
  RETURN
ENDIF

!IF (PRESENT(full_steps)) THEN
!  lfull_steps = full_steps
!ELSE
!  lfull_steps = .FALSE.
!ENDIF

! be safe
CALL vol7d_alloc_vol(this)

! compute length of cumulation step in seconds
CALL getval(step, amsec=msteps)
steps = msteps/1000_int_ll
! create a mask of suitable timeranges
ALLOCATE(mask_timerange(SIZE(this%timerange)))
mask_timerange(:) = this%timerange(:)%timerange == tri &
 .AND. this%timerange(:)%p1 /= imiss .AND. this%timerange(:)%p2 /= imiss &
 .AND. this%timerange(:)%p1 > 0 &
 .AND. this%timerange(:)%p1 >= this%timerange(:)%p2

IF (optio_log(full_steps)) THEN ! keep only timeranges defining intervals ending at integer steps
  mask_timerange(:) = mask_timerange(:) .AND. (MOD(this%timerange(:)%p1, steps) == 0)
ENDIF

! safety dimension, less is usually enough
ALLOCATE(tr(COUNT(mask_timerange)*COUNT(mask_timerange)))
ntr = 0
! count the new possible timeranges
DO i = 1, SIZE(this%timerange)
  IF (.NOT.mask_timerange(i)) CYCLE
  DO j = 1, SIZE(this%timerange)
    IF (.NOT.mask_timerange(j)) CYCLE
    IF (this%timerange(j)%p1 > this%timerange(i)%p1 .AND. & ! right order
     this%timerange(j)%p1-this%timerange(j)%p2 == &    ! same start
     this%timerange(i)%p1-this%timerange(i)%p2 .AND. & !
     this%timerange(j)%p2-this%timerange(i)%p2 == steps) THEN ! right step
      ntr = ntr + 1
      tr(ntr) = vol7d_timerange_new(tri,this%timerange(j)%p1, steps)
    ENDIF
  ENDDO
ENDDO
! allocate and assign them
ndtr = count_distinct(tr(1:ntr), back=.TRUE.)
CALL vol7d_alloc(that, ntimerange=ndtr)
that%timerange(:) = pack_distinct(tr, ndtr, back=.TRUE.)
DEALLOCATE(tr)

! create the new volume template
! check whether any timerange already satisfies the requirements and copy it
IF (ANY(mask_timerange(:) .AND. this%timerange(:)%p2 == steps)) THEN
  CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(mask_timerange(:) .AND. this%timerange(:)%p2 == steps))
ELSE
  CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(/.FALSE./))
ENDIF

! merge with template
CALL vol7d_merge(that, v7dtmp, lanasimple=.TRUE.)

! ALLOCATE(map_tr(ndtr,2)) ! 1-1 mapping, faster but less general
ALLOCATE(map_tr(SIZE(this%timerange),SIZE(this%timerange)))
map_tr(:,:) = imiss
DO i = 1, SIZE(this%timerange)
  IF (.NOT.mask_timerange(i)) CYCLE
  DO j = 1, SIZE(this%timerange)
    IF (.NOT.mask_timerange(j)) CYCLE
    IF (this%timerange(j)%p1 > this%timerange(i)%p1 .AND. & ! right order
     this%timerange(j)%p1-this%timerange(j)%p2 == &    ! same start
     this%timerange(i)%p1-this%timerange(i)%p2 .AND. & !
     this%timerange(j)%p2-this%timerange(i)%p2 == steps) THEN ! right step
      n = INDEX(that%timerange(:),vol7d_timerange_new(tri,this%timerange(j)%p1, steps))
      IF (n > 0) map_tr(j,i) = n
    ENDIF
  ENDDO
ENDDO

#ifdef DEBUG
IF (COUNT(c_e(map_tr(:,:))) > SIZE(that%timerange(:))) THEN
  CALL l4f_log(L4F_INFO, &
   'vol7d_extend_cumavg_diff, duplicate sources for timeranges: '// &
   TRIM(to_char(COUNT(c_e(map_tr(:,:)))))//' sources and '// &
   TRIM(to_char(SIZE(that%timerange(:))))//' timeranges.')
ENDIF
#endif

IF (ASSOCIATED(this%voldatir)) THEN
  DO i = 1, SIZE(this%timerange)
    IF (.NOT.mask_timerange(i)) CYCLE ! does this speed up?
    DO j = 1, SIZE(this%timerange)
      IF (.NOT.mask_timerange(j)) CYCLE ! does this speed up?
      i4 = map_tr(j,i)
      IF (.NOT.c_e(i4)) CYCLE
      DO i6 = 1, SIZE(this%network)
        DO i5 = 1, SIZE(this%dativar%r)
          DO i3 = 1, SIZE(this%level)
            DO i2 = 1, SIZE(this%time)
              DO i1 = 1, SIZE(this%ana)
                IF (this%voldatir(i1,i2,i3,j,i5,i6) /= rmiss .AND. &
                 this%voldatir(i1,i2,i3,i,i5,i6) /= rmiss) THEN
                  IF (tri == 0) THEN ! average
                    that%voldatir(i1,i2,i3,i4,i5,i6) = &
                     (this%voldatir(i1,i2,i3,j,i5,i6)*this%timerange(j)%p2 - &
                     this%voldatir(i1,i2,i3,i,i5,i6)*this%timerange(i)%p2)/ &
                     steps ! optimize avoiding conversions
                  ELSE IF (tri == 1) THEN ! cumulation, compute MAX(0.,)?
                    that%voldatir(i1,i2,i3,i4,i5,i6) = &
                     this%voldatir(i1,i2,i3,j,i5,i6) - &
                     this%voldatir(i1,i2,i3,i,i5,i6)
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

IF (ASSOCIATED(this%voldatid)) THEN
  DO i = 1, SIZE(this%timerange)
    IF (.NOT.mask_timerange(i)) CYCLE ! does this speed up?
    DO j = 1, SIZE(this%timerange)
      IF (.NOT.mask_timerange(j)) CYCLE ! does this speed up?
      i4 = map_tr(j,i)
      IF (.NOT.c_e(i4)) CYCLE
      DO i6 = 1, SIZE(this%network)
        DO i5 = 1, SIZE(this%dativar%d)
          DO i3 = 1, SIZE(this%level)
            DO i2 = 1, SIZE(this%time)
              DO i1 = 1, SIZE(this%ana)
                IF (this%voldatid(i1,i2,i3,j,i5,i6) /= dmiss .AND. &
                 this%voldatid(i1,i2,i3,i,i5,i6) /= dmiss) THEN
                  IF (tri == 0) THEN ! average
                    that%voldatid(i1,i2,i3,i4,i5,i6) = &
                     (this%voldatid(i1,i2,i3,j,i5,i6)*this%timerange(j)%p2 - &
                     this%voldatid(i1,i2,i3,i,i5,i6)*this%timerange(i)%p2)/ &
                     steps ! optimize avoiding conversions
                  ELSE IF (tri == 1) THEN ! cumulation, compute MAX(0.,)?
                    that%voldatid(i1,i2,i3,i4,i5,i6) = &
                     this%voldatid(i1,i2,i3,j,i5,i6) - &
                     this%voldatid(i1,i2,i3,i,i5,i6)
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

DEALLOCATE(mask_timerange, map_tr)
CALL makeother(.TRUE.)

CONTAINS

SUBROUTINE makeother(filter)
LOGICAL,INTENT(in) :: filter
IF (PRESENT(other)) THEN
  IF (filter) THEN ! create volume with the remaining data for further processing
    CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
     ltimerange=(this%timerange(:)%timerange /= tri .OR. this%timerange(:)%p2 == imiss))
  ELSE
    CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE.)
  ENDIF
ENDIF
END SUBROUTINE makeother

END SUBROUTINE vol7d_extend_cumavg_diff


!> Riempimento dei buchi temporali in un volume.
!! Questo metodo crea, a partire da un volume originale, un nuovo
!! volume dati in cui la dimensione tempo contiene tutti gli istanti
!! tra \a start e \a stopp (o tra il primo e l'ultimo livello
!! temporale) ad intervalli \a step. Gli eventuali livelli mancanti
!! vengono aggiunti riempiendo le corrispondenti posizioni dei volumi
!! dati con valori mancanti. I livelli temporali che non sono ad
!! intervalli \a step interi a partire dall'inizio, oppure quelli che
!! giacciono fuori dall'intervallo \a start:stop non vengono toccati e
!! quindi rimangono immutati nel volume finale (si veda anche la
!! descrizione di ::vol7d_regularize_time). Il volume originale non
!! viene modificato e quindi dovrà essere distrutto da parte del
!! programma chiamante se il suo contenuto non è più
!! richiesto. Attenzione, se necessario la dimensione tempo (vettore
!! \a this%time del volume \a this ) viene riordinata, come effetto
!! collaterale della chiamata.
SUBROUTINE vol7d_fill_time(this, that, step, start, stopp)
TYPE(vol7d),INTENT(inout) :: this
TYPE(vol7d),INTENT(inout) :: that
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),INTENT(in),OPTIONAL :: start
TYPE(datetime),INTENT(in),OPTIONAL :: stopp

TYPE(datetime) :: counter, lstart, lstop
INTEGER :: i, naddtime

CALL vol7d_smart_sort(this, lsort_time=.TRUE.)
IF (PRESENT(start)) THEN
  lstart = start
ELSE
  lstart = this%time(1)
ENDIF
IF (PRESENT(stopp)) THEN
  lstop = stopp
ELSE
  lstop = this%time(SIZE(this%time))
ENDIF
CALL l4f_log(L4F_INFO, 'Time level interval: '//TRIM(to_char(lstart))// &
 ' '//TRIM(to_char(lstop)))

! Count the number of time levels required for completing the series
! valid also in the case (SIZE(this%time) == 0)
naddtime = 0
counter = lstart
i = 1
naddcount: DO WHILE(counter <= lstop)
  DO WHILE(i <= SIZE(this%time)) ! this%time(i) chases counter
    IF (counter < this%time(i)) THEN ! this%time(i) overtook counter
      i = MAX(i-1,1) ! go back if possible
      EXIT
    ELSE IF (counter == this%time(i)) THEN ! found matching time
      counter = counter + step
      CYCLE naddcount
    ENDIF
    i = i + 1
  ENDDO
  naddtime = naddtime + 1
  counter = counter + step
ENDDO naddcount

! old universal algorithm, not optimized, check that the new one is equivalent
!naddtime = 0
!counter = lstart
!DO WHILE(counter <= lstop)
!  IF (.NOT.ANY(counter == this%time(:))) THEN
!    naddtime = naddtime + 1
!  ENDIF
!  counter = counter + step
!ENDDO

IF (naddtime > 0) THEN

  CALL init(that)
  CALL vol7d_alloc(that, ntime=naddtime)
  CALL vol7d_alloc_vol(that)

  ! Repeat the count loop setting the time levels to be added
  naddtime = 0
  counter = lstart
  i = 1
  naddadd: DO WHILE(counter <= lstop)
    DO WHILE(i <= SIZE(this%time)) ! this%time(i) chases counter
      IF (counter < this%time(i)) THEN ! this%time(i) overtook counter
        i = MAX(i-1,1) ! go back if possible
        EXIT
      ELSE IF (counter == this%time(i)) THEN ! found matching time
        counter = counter + step
        CYCLE naddadd
      ENDIF
      i = i + 1
    ENDDO
    naddtime = naddtime + 1
    that%time(naddtime) = counter ! only difference
    counter = counter + step
  ENDDO naddadd

  CALL vol7d_append(that, this, sort=.TRUE.)

ELSE
  CALL vol7d_copy(this, that)
ENDIF

END SUBROUTINE vol7d_fill_time


!> Regolarizzazione della dimensione tempo in un volume.
!! Questo metodo crea, a partire da un volume originale, un nuovo
!! volume dati in cui la dimensione tempo contiene tutti e soli gli
!! istanti tra \a start e \a stopp (o tra il primo e l'ultimo livello
!! temporale) ad intervalli \a step. Gli eventuali livelli mancanti
!! vengono aggiunti riempiendo le corrispondenti posizioni dei volumi
!! dati con valori mancanti. Fa quindi un lavoro simile al metodo
!! ::vol7d_fill_time ma in più elimina i livelli temporali che non
!! soddisfano la condizione richiesta.  Il volume originale non viene
!! modificato e quindi dovrà essere distrutto da parte del programma
!! chiamante se il suo contenuto non è più richiesto. Attenzione, se
!! necessario, la dimensione tempo (vettore \a this%time del volume \a
!! this ) viene riordinata, come effetto collaterale della chiamata.
SUBROUTINE vol7d_regularize_time(this, that, step, start, stopp)
TYPE(vol7d),INTENT(inout) :: this
TYPE(vol7d),INTENT(inout) :: that
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),INTENT(in),OPTIONAL :: start
TYPE(datetime),INTENT(in),OPTIONAL :: stopp

TYPE(datetime) :: lstart, lstop
INTEGER :: n
LOGICAL, ALLOCATABLE :: time_mask(:)
TYPE(vol7d) :: v7dtmp

CALL vol7d_smart_sort(this, lsort_time=.TRUE.)
IF (PRESENT(start)) THEN
  lstart = start
ELSE
  lstart = this%time(1)
ENDIF
IF (PRESENT(stopp)) THEN
  lstop = stopp
ELSE
  lstop = this%time(SIZE(this%time))
ENDIF

ALLOCATE(time_mask(SIZE(this%time)))
time_mask(:) = .TRUE.
DO n = 1, SIZE(this%time)
  IF (this%time(n) < lstart .OR. this%time(n) > lstop .OR. &
   MOD(this%time(n) - lstart, step) /= timedelta_0) THEN
    time_mask(n) = .FALSE.
  ENDIF
ENDDO
IF (ALL(time_mask)) THEN
  v7dtmp = this ! do not lose time in a simple common case
  CALL vol7d_fill_time(v7dtmp, that, step, start, stopp)
ELSE
  CALL vol7d_copy(this, v7dtmp, ltime=time_mask)
  CALL vol7d_fill_time(v7dtmp, that, step, start, stopp)
  CALL delete(v7dtmp) ! must be cleaned in this case
ENDIF

END SUBROUTINE vol7d_regularize_time


!> Metodo per normalizzare la coordinata verticale.
!! Per ora la normalizzazione effettuata riporta i valori di pressione
!! nella sezione dati alla coordinata verticale sostituendo quella eventualmente presente.
!! Classicamente serve per i dati con coordinata verticale model layer (105)
!! Essendo che la pressione varia nello spazio orizzontale e nel tempo
!! questo metodo restituisce un solo profilo verticale.
SUBROUTINE vol7d_normalize_vcoord(this,that,ana,time,timerange,network)
TYPE(vol7d),INTENT(INOUT)  :: this !< oggetto da normalizzare
TYPE(vol7d),INTENT(OUT) :: that !< oggetto normalizzato
integer,intent(in)   :: time,ana,timerange,network !< indici dell'elemento da estrarre

character(len=1) :: type
integer :: ind
TYPE(vol7d_var) ::  var
LOGICAL,allocatable :: ltime(:),ltimerange(:),lana(:),lnetwork(:)

allocate(ltime(size(this%time)))
allocate(ltimerange(size(this%timerange)))
allocate(lana(size(this%ana)))
allocate(lnetwork(size(this%network)))

ltime=.false.
ltimerange=.false.
lana=.false.
lnetwork=.false.

ltime(time)=.true.
ltimerange(timerange)=.true.
lana(ana)=.true.
lnetwork(network)=.true.

call vol7d_copy(this, that,unique=.true.,&
 ltime=ltime,ltimerange=ltimerange,lana=lana,lnetwork=lnetwork )

call init(var, btable="B10004")    ! Pressure
type=cmiss
!type="i"
ind = index(that%dativar, var, type=type)

select case (type)

case("d")
  
  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatid(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = integerdat(that%voldatid(1,1,:,1,ind,1),that%dativar%d(ind))*10
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("r")

  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatir(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = integerdat(that%voldatir(1,1,:,1,ind,1),that%dativar%r(ind))*10
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("i")
    
  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatii(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = integerdat(that%voldatii(1,1,:,1,ind,1),that%dativar%i(ind))*10
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("b")

  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatib(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = integerdat(that%voldatib(1,1,:,1,ind,1),that%dativar%b(ind))*10
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("c")

  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatic(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = integerdat(that%voldatic(1,1,:,1,ind,1),that%dativar%c(ind))*10
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where
    
end select

deallocate(ltime)
deallocate(ltimerange)
deallocate(lana)
deallocate(lnetwork)

END SUBROUTINE vol7d_normalize_vcoord


!> Metodo per normalizzare la coordinata verticale.
!! Per ora la normalizzazione effettuata riporta i valori di pressione
!! nella sezione dati alla coordinata verticale sostituendo quella eventualmente presente.
!! Classicamente serve per i dati con coordinata verticale model layer (105)
!! Essendo che la pressione varia nello spazio orizzontale e nel tempo
!! questo metodo restituisce un solo profilo verticale.
SUBROUTINE vol7d_compute_var(this,that,var)
TYPE(vol7d),INTENT(INOUT)  :: this !< oggetto da normalizzare
TYPE(vol7d),INTENT(OUT) :: that !< oggetto normalizzato

character(len=1) :: type
integer :: ind
TYPE(vol7d_var),intent(in) ::  var


!!$call init(var, btable="B10004")    ! Pressure
!!$type=cmiss
!!$call vol7d_varvect_index(that%dativar,var , type=type,index_v=ind)
!!$
!!$select case (type)
!!$
!!$case("d")
!!$  
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatid(1,1,:,1,ind,1),that%dativar%d(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$
!!$case("r")
!!$
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatir(1,1,:,1,ind,1),that%dativar%r(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$
!!$case("i")
!!$    
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatii(1,1,:,1,ind,1),that%dativar%i(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$
!!$case("b")
!!$
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatib(1,1,:,1,ind,1),that%dativar%b(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$
!!$case("c")
!!$
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatic(1,1,:,1,ind,1),that%dativar%c(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$    
!!$end select


END SUBROUTINE vol7d_compute_var

END MODULE vol7d_class_compute
