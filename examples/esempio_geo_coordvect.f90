PROGRAM esempio_geo_coordvect
USE geo_coord_class
USE vol7d_utilities
IMPLICIT NONE

TYPE(geo_coordvect),POINTER :: macroa(:)
INTEGER :: un, i
INTEGER(kind=ptr_c) :: shphandle
CHARACTER(len=512) :: filesim

un = open_package_file('polipciv4.dat', filetype_data)
IF (un < 0) STOP 1
INQUIRE(unit=un, name=filesim)
PRINT'(A)',TRIM(filesim)
CLOSE(un)

CALL import(macroa, shpfilesim=filesim)
DO i = 1, SIZE(macroa)
  CALL to_utm(macroa(i), fuso=32, elliss=elliss_intl)
ENDDO

CALL export(macroa, shpfile='macroraree_er')

END PROGRAM esempio_geo_coordvect
