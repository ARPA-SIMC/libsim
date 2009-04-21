!> Module for basic statistical computations taking into account missing data.
!! The functions provided are average, variance, linear correlation
!! coefficient and percentile for a population. Missing data are
!! excluded from computations and from weighing. Currently only \a
!! REAL functions are available, the module should be extended to \a
!! DOUBLEPRECISION with a transparent interface.
!! \ingroup base
MODULE simple_stat
USE missing_values
IMPLICIT NONE

!> Generic interface to the average function.
INTERFACE stat_average
  MODULE PROCEDURE stat_averager
END INTERFACE

!> Generic interface to the variance function.
INTERFACE stat_variance
  MODULE PROCEDURE stat_variancer
END INTERFACE

!> Generic interface to the linear correlation function.
INTERFACE stat_linear_corr
  MODULE PROCEDURE stat_linear_corrr
END INTERFACE

!> Generic interface to the percentile function.
INTERFACE stat_percentile
  MODULE PROCEDURE stat_percentiler
END INTERFACE

PRIVATE
PUBLIC stat_average, stat_variance, stat_linear_corr, stat_percentile

CONTAINS


!> Compute the average of the random variable provided, taking into account missing data.
FUNCTION stat_averager(sample, nomiss) RESULT(average)
REAL, INTENT(in) :: sample(:) !< the variable to be averaged
LOGICAL, OPTIONAL, INTENT(in) :: nomiss

REAL :: average

INTEGER :: sample_count
LOGICAL :: sample_mask(SIZE(sample))

sample_mask = (sample /= rmiss)
sample_count = COUNT(sample_mask)

IF (sample_count > 0) THEN
! compute average
  average = SUM(sample, mask=sample_mask)/sample_count
ELSE
  average = rmiss
ENDIF

END FUNCTION stat_averager


!> Compute the variance of the random variable provided, taking into
!! account missing data.  The average can be returned as an optional
!! parameter since it is computed anyway.
FUNCTION stat_variancer(sample, average, nomiss) RESULT(variance)
REAL, INTENT(in) :: sample(:) !< the variable for which variance has to be computed
REAL, OPTIONAL, INTENT(out) :: average !< the average of the variable can optionally be returned
LOGICAL, OPTIONAL, INTENT(in) :: nomiss

REAL :: variance

REAL :: laverage
INTEGER :: sample_count, i
LOGICAL :: sample_mask(SIZE(sample))

sample_mask = (sample /= rmiss)
sample_count = COUNT(sample_mask)

IF (sample_count > 0) THEN
! compute average
  laverage = SUM(sample, mask=sample_mask)/sample_count
  IF (PRESENT(average)) average = laverage
! compute sum of squares and variance
  variance = 0.
  DO i = 1, SIZE(sample)
    IF (sample_mask(i)) variance = variance + sample(i)**2
  ENDDO
!  variance = SUM(sample**2, mask=sample_mask)/sample_count - laverage
  variance = variance/sample_count - laverage**2
ELSE
  IF (PRESENT(average)) average = rmiss
  variance = rmiss
ENDIF

END FUNCTION stat_variancer


!> Compute the linear correlation coefficient between the two random
!! variables provided, taking into account missing data. Data are
!! considered missing when at least one has variable a missing value.
!! The average and the variance of each variabl can be returned as
!! optional parameters since they are computed anyway.
FUNCTION stat_linear_corrr(sample1, sample2, average1, average2, &
 variance1, variance2, nomiss) RESULT(linear_corr)
REAL, INTENT(in) :: sample1(:) !< the first variable
REAL, INTENT(in) :: sample2(:) !< the second variable
REAL, OPTIONAL, INTENT(out) :: average1 !< the average of the first variable can optionally be returned
REAL, OPTIONAL, INTENT(out) :: average2 !< the average of the second variable can optionally be returned
REAL, OPTIONAL, INTENT(out) :: variance1 !< the variance of the first variable can optionally be returned
REAL, OPTIONAL, INTENT(out) :: variance2 !< the variance of the second variable can optionally be returned
LOGICAL, OPTIONAL, INTENT(in) :: nomiss

REAL :: linear_corr

REAL :: laverage1, laverage2, lvariance1, lvariance2
INTEGER :: sample_count, i
LOGICAL :: sample_mask(SIZE(sample1))

IF (SIZE(sample1) /= SIZE(sample2)) THEN
  IF (PRESENT(average1)) average1 = rmiss
  IF (PRESENT(average2)) average2 = rmiss
  IF (PRESENT(variance1)) variance1 = rmiss
  IF (PRESENT(variance2)) variance2 = rmiss
  linear_corr = rmiss
  RETURN
ENDIF

sample_mask = (sample1 /= rmiss .AND. sample2 /= rmiss)
sample_count = COUNT(sample_mask)
IF (sample_count > 0) THEN
! compute averages
  laverage1 = SUM(sample1, mask=sample_mask)/sample_count
  laverage2 = SUM(sample2, mask=sample_mask)/sample_count
  IF (PRESENT(average1)) average1 = laverage1
  IF (PRESENT(average2)) average2 = laverage2
! compute sum of squares and variances
  lvariance1 = 0.
  lvariance2 = 0.
  DO i = 1, SIZE(sample1)
    IF (sample_mask(i))THEN
      lvariance1 = lvariance1 + sample1(i)**2
      lvariance2 = lvariance2 + sample2(i)**2
    ENDIF
  ENDDO
  lvariance1 =  lvariance1/sample_count - laverage1**2
  lvariance2 =  lvariance2/sample_count - laverage2**2
  IF (PRESENT(variance1)) variance1 = lvariance1
  IF (PRESENT(variance2)) variance2 = lvariance2
! compute correlation
  linear_corr = 0.
  DO i = 1, SIZE(sample1)
    IF (sample_mask(i)) linear_corr = linear_corr + sample1(i)*sample2(i)
  ENDDO
  linear_corr = (linear_corr/sample_count - laverage1*laverage2) / SQRT(lvariance1*lvariance2)
!  linear_corr = (SUM(sample1*sample2, mask=sample_mask)/sample_count - laverage1*laverage2) / &
!   SQRT(lvariance1*lvariance2)
ELSE
  IF (PRESENT(average1)) average1 = rmiss
  IF (PRESENT(average2)) average2 = rmiss
  IF (PRESENT(variance1)) variance1 = rmiss
  IF (PRESENT(variance2)) variance2 = rmiss
  linear_corr = rmiss
ENDIF

END FUNCTION stat_linear_corrr


!> Compute a set of percentiles for a random variable.
!! The result is a vector of the same size as the vector of percentile
!! values provided. Percentiles are computed by linear interpolation.
FUNCTION stat_percentiler(sample, perc_vals, nomiss) RESULT(percentile)
REAL, INTENT(in) :: sample(:) !< the variable for which percentiles are to be computed
REAL, INTENT(in) :: perc_vals(:) !< the percentiles values to be computed, between 0. and 100.
LOGICAL, OPTIONAL, INTENT(in) :: nomiss

REAL :: percentile(SIZE(perc_vals))

REAL :: lsample(SIZE(sample)), v, rindex
INTEGER :: sample_count, i, j, iindex
LOGICAL :: sample_mask(SIZE(sample))

sample_mask = (sample /= rmiss)
sample_count = COUNT(sample_mask)
lsample(1:sample_count) = PACK(sample, mask=sample_mask)

! sort
DO j = 2, sample_count
  v = lsample(j)
  DO i = j-1, 1, -1
    IF (v >= lsample(i)) EXIT
    lsample(i+1) = lsample(i)
  ENDDO
  lsample(i+1) = v
ENDDO

DO j = 1, SIZE(perc_vals)
  IF (perc_vals(j) >= 0. .AND. perc_vals(j) <= 100.) THEN
! compute real index of requested percentile in sample
    rindex = REAL(sample_count-1, kind=KIND(rindex))*perc_vals(j)/100.+1.
! compute integer index of previous element in sample, beware of corner cases
    iindex = MIN(MAX(INT(rindex), 1), sample_count-1)
! compute linearly interpolated percentile
    percentile(j) = lsample(iindex)*(REAL(iindex+1)-rindex) + lsample(iindex+1)*(rindex-REAL(iindex))
  ELSE
    percentile(j) = rmiss
  ENDIF
ENDDO

END FUNCTION stat_percentiler


END MODULE simple_stat
