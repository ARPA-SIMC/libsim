PROGRAM esempio_geo_coordvect
USE file_utilities
USE geo_coord_class
IMPLICIT NONE

TYPE(geo_coordvect),POINTER :: macroa(:)
INTEGER :: un, i
INTEGER(kind=ptr_c) :: shphandle
CHARACTER(len=512) :: filesim

filesim=get_package_filepath('polipciv4.dat', filetype_data)

CALL import(macroa, shpfilesim=filesim)
DO i = 1, SIZE(macroa)
  CALL to_utm(macroa(i), fuso=32, elliss=elliss_intl)
ENDDO

CALL export(macroa, shpfile='macroraree_er')

END PROGRAM esempio_geo_coordvect
