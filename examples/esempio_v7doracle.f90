PROGRAM v7doracle
USE datetime_class
USE vol7d_oraclesim_class
IMPLICIT NONE

TYPE(vol7d_oraclesim) :: v7d
TYPE(datetime) :: ti, tf

CALL init(ti, year=2007, month=3, day=1, hour=12)
CALL init(tf, year=2007, month=3, day=2, hour=11)


CALL init(v7d)
CALL vol7d_oraclesim_import(v7d, 'B13011', 18, ti, tf)

END PROGRAM v7doracle
