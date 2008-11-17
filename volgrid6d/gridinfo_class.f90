module gridinfo_class

USE grid_class
USE datetime_class
USE vol7d_timerange_class
USE vol7d_level_class
USE volgrid6d_var_class
use log4fortran


IMPLICIT NONE

character (len=255),parameter:: subcategory="gridinfo_class"


!> Definisce un oggetto contenente le informazioni relative a un grib
type gridinfo_type


!> descrittore del grigliato
  type(griddim_def) :: griddim
!> descrittore della dimensione tempo
  TYPE(datetime) :: time
!> descrittore della dimensione intervallo temporale (timerange)
  TYPE(vol7d_timerange) :: timerange
!> descrittore della dimensione livello verticale
  TYPE(vol7d_level) :: level
!> vettore descrittore della dimensione variabile di anagrafica
  TYPE(volgrid6d_var) :: var
!> id del grib come da grib_api
  integer ::  gaid
  integer :: category !< log4fortran

end type gridinfo_type



INTERFACE init
  MODULE PROCEDURE init_gridinfo
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_gridinfo
END INTERFACE


!> Import
!! Legge i valori dal grib e li imposta appropriatamente
INTERFACE import
  MODULE PROCEDURE import_time,import_timerange,import_level,import_gridinfo
END INTERFACE

!> Export
!! Imposta i valori nel grib
INTERFACE export
  MODULE PROCEDURE export_time,export_timerange,export_level,export_gridinfo
END INTERFACE


INTERFACE display
  MODULE PROCEDURE display_timerange,display_level,display_gridinfo,display_gridinfov,display_time
END INTERFACE


private

public gridinfo_type,init,delete,import,export,display,decode_gridinfo,encode_gridinfo

contains

!> Inizializza un oggetto di tipo gridinfo_type.
SUBROUTINE init_gridinfo(this,gaid,griddim,time,timerange,level,var,categoryappend)
TYPE(gridinfo_type),intent(out) :: this !< oggetto da inizializzare

!> id del grib come da grib_api
integer,intent(in),optional ::  gaid
!> descrittore del grigliato
type(griddim_def),intent(in),optional :: griddim
!> descrittore della dimensione tempo
TYPE(datetime),intent(in),optional :: time
!> descrittore della dimensione intervallo temporale (timerange)
TYPE(vol7d_timerange),intent(in),optional :: timerange
!> descrittore della dimensione livello verticale
TYPE(vol7d_level),intent(in),optional :: level
!> vettore descrittore della dimensione variabile di anagrafica
TYPE(volgrid6d_var),intent(in),optional :: var

character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
end if
this%category=l4f_category_get(a_name)


if (present(gaid))then
  this%gaid=gaid
else
  this%gaid=imiss
end if

call l4f_category_log(this%category,L4F_DEBUG,"init gridinfo gaid: "//to_char(this%gaid))

if (present(griddim))then
  this%griddim=griddim
else
  call init(this%griddim)
end if

if (present(time))then
  this%time=time
else
  call init(this%time)
end if

if (present(timerange))then
  this%timerange=timerange
else
  call init(this%timerange)
end if

if (present(level))then
  this%level=level
else
  call init(this%level)
end if

if (present(var))then
  this%var=var
else
  call init(this%var)
end if

end SUBROUTINE init_gridinfo


subroutine delete_gridinfo (this)
TYPE(gridinfo_type),intent(out) :: this !< oggetto da eliminare


call delete(this%griddim)
call delete(this%time)
call delete(this%timerange)
call delete(this%level)
call delete(this%var)

call l4f_category_log(this%category,L4F_DEBUG,"cancello gaid" )
this%gaid=imiss

!chiudo il logger
call l4f_category_delete(this%category)


end subroutine delete_gridinfo



subroutine import_gridinfo (this)

TYPE(gridinfo_type),intent(out) :: this !< oggetto da importare


call l4f_category_log(this%category,L4F_DEBUG,"ora provo ad importare da grib " )


call import(this%griddim,this%gaid)
call import(this%time,this%gaid)
call import(this%timerange,this%gaid)
call import(this%level,this%gaid)
call import(this%var,this%gaid)


end subroutine import_gridinfo


subroutine export_gridinfo (this)

TYPE(gridinfo_type),intent(out) :: this !< oggetto da exportare

call l4f_category_log(this%category,L4F_DEBUG,"export to grib" )

!attenzione: exportando da volgrid griddim è già esportato

if ( c_e(this%gaid)) then
  call export(this%griddim,this%gaid)
  call export(this%time,this%gaid)
  call export(this%timerange,this%gaid)
  call export(this%level,this%gaid)
  call export(this%var,this%gaid)
end if

end subroutine export_gridinfo



subroutine import_time(this,gaid)

TYPE(datetime),INTENT(out) :: this
integer,INTENT(in)         :: gaid
integer                    :: EditionNumber
character(len=9)           :: date
character(len=10)           :: time


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or.EditionNumber == 2 )then

  call grib_get(gaid,'dataDate',date )
  call grib_get(gaid,'dataTime',time(:5) )

  call init (this,simpledate=date(:8)//time(:4))

else

  CALL raise_error('GribEditionNumber not supported')

end if

end subroutine import_time



subroutine export_time(this,gaid)

TYPE(datetime),INTENT(in) :: this
integer,INTENT(in)        :: gaid
integer                   :: EditionNumber,date,time
character(len=17)         :: date_time


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or.EditionNumber == 2 )then

! datetime is AAAAMMGGhhmmssmsc
  call getval (this,simpledate=date_time)
  read(date_time(:8),*)date
  read(date_time(9:14),*)time
  call grib_set(gaid,'dataDate',date)
  call grib_set(gaid,'dataTime',time)

else

  CALL raise_error('GribEditionNumber not supported')

end if


end subroutine export_time



subroutine import_level(this,gaid)

TYPE(vol7d_level),INTENT(out) :: this
integer,INTENT(in)          :: gaid
integer                     :: EditionNumber,level1,l1,level2,l2
integer :: ltype,ltype1,scalef1,scalev1,ltype2,scalef2,scalev2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then
  
  call grib_get(gaid,'indicatorOfTypeOfLevel',ltype)
  call grib_get(gaid,'topLevel',l1)
  call grib_get(gaid,'bottomLevel',l2)

  call level_g1_to_g2(ltype,l1,l2,ltype1,scalef1,scalev1,ltype2,scalef1,scalev2)

else if (EditionNumber == 2)then

  call grib_get(gaid,'typeOfFirstFixedSurface',ltype1)
  call grib_get(gaid,'scaleFactorOfFirstFixedSurface',scalef1)
  call grib_get(gaid,'scaledValueOfFirstFixedSurface',scalev1)

  call grib_get(gaid,'typeOfSecondFixedSurface',ltype2)
  call grib_get(gaid,'scaleFactorOfSecondFixedSurface',scalef2)
  call grib_get(gaid,'scaledValueOfSecondFixedSurface',scalev2)

else

  call raise_error('GribEditionNumber not supported')

end if

! Convert missing levels and units m -> mm
call level_g2_to_dballe(ltype1,scalef1,scalev1,ltype2,scalef2,scalev2, &
 level1,l1,level2,l2)

call init (this,level1,l1,level2,l2)


end subroutine import_level



subroutine export_level(this,gaid)

TYPE(vol7d_level),INTENT(in) :: this
integer,INTENT(in) :: gaid

INTEGER :: EditionNumber, ltype1, scalef1, scalev1, ltype2, scalef2, scalev2, &
 ltype, l1, l2

CALL level_dballe_to_g2(this%level1, this%l1, this%level2, this%l2, &
 ltype1, scalef1, scalev1, ltype2, scalef2, scalev2)

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  CALL level_g2_to_g1(ltype1,scalef1,scalev1,ltype2,scalef1,scalev2,ltype,l1,l2)

  call grib_set(gaid,'indicatorOfTypeOfLevel',ltype)
  call grib_set(gaid,'topLevel',l1)
  call grib_set(gaid,'bottomLevel',l2)

else if (EditionNumber == 2)then

  call grib_set(gaid,'typeOfFirstFixedSurface',ltype1)
  call grib_set(gaid,'scaleFactorOfFirstFixedSurface',scalef1)
  call grib_set(gaid,'scaledValueOfFirstFixedSurface',scalev1)

  call grib_set(gaid,'typeOfSecondFixedSurface',ltype2)
  call grib_set(gaid,'scaleFactorOfSecondFixedSurface',scalef2)
  call grib_set(gaid,'scaledValueOfSecondFixedSurface',scalev2)

else

  call raise_error('GribEditionNumber not supported')

end if


end subroutine export_level



subroutine import_timerange(this,gaid)

TYPE(vol7d_timerange),INTENT(out) :: this
integer,INTENT(in)          :: gaid
integer ::EditionNumber,timerange,p1,p2,status

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or. EditionNumber == 2)then
  
  call grib_get(gaid,'typeOfStatisticalProcessing',timerange,status)
  if (status == GRIB_SUCCESS) then
     call grib_get(gaid,'endStepInHours',p1)
     call grib_get(gaid,'lengthOfTimeRange',p2)
  
     call init (this, timerange,p1,p2)
  else

! TODO
! qui forse bisogna capire meglio in quale template siamo
! e come mai grib1 va a finire qui

     call init (this)
     
  end if
else

  call raise_error('GribEditionNumber not supported')

end if

end subroutine import_timerange



subroutine export_timerange(this,gaid)

TYPE(vol7d_timerange),INTENT(in) :: this
integer,INTENT(in)         :: gaid
integer ::EditionNumber,timerange,p1,p2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 ) then

   call raise_error("convert timerange from gridinfo standard to grib1 not managed")

else if (EditionNumber == 2)then

!TODO bisogna gestire anche il template

  call grib_set(gaid,'typeOfStatisticalProcessing',this%timerange)
  call grib_set(gaid,'endStepInHours',this%p1)
  call grib_set(gaid,'lengthOfTimeRange',this%p2)

else

  call raise_error('GribEditionNumber not supported')

end if



end subroutine export_timerange



subroutine display_gridinfo (this)

TYPE(gridinfo_type),intent(in) :: this !< oggetto da stampare


call l4f_category_log(this%category,L4F_DEBUG,"ora mostro gridinfo " )


print*,"----------------------- gridinfo display ---------------------"
call display(this%griddim)
call display(this%time)
call display(this%timerange)
call display(this%level)
call display(this%var)
print*,"--------------------------------------------------------------"


end subroutine display_gridinfo



subroutine display_gridinfov (this)

TYPE(gridinfo_type),intent(in) :: this(:) !< vettore di oggetti da stampare
integer :: i

print*,"----------------------- gridinfo  vector ---------------------"

do i=1, size(this)

  call l4f_category_log(this(i)%category,L4F_DEBUG,"ora mostro il vettore gridinfo " )

  call display(this(i))

end do
print*,"--------------------------------------------------------------"

end subroutine display_gridinfov



subroutine display_timerange(this)

TYPE(vol7d_timerange),INTENT(in) :: this
integer ::EditionNumber,timerange,p1,p2

print*,"TIMERANGE: ",this%timerange,this%p1,this%p2

end subroutine display_timerange



subroutine display_level(this)

TYPE(vol7d_level),INTENT(in) :: this

print*,"LEVEL: ",this%level1,this%l1,this%level2,this%l2
  
end subroutine display_level



subroutine display_time(this)

TYPE(datetime),INTENT(in) :: this
character(len=17)         :: date_time

call getval (this,simpledate=date_time)

print*,"TIME: ",date_time


end subroutine display_time




function decode_gridinfo(this) result (field)
TYPE(gridinfo_type),INTENT(in) :: this      !< oggetto da decodificare
integer :: EditionNumber
integer :: alternativeRowScanning,iScansNegatively,jScansPositively,jPointsAreConsecutive
integer :: numberOfValues,numberOfPoints
real :: field (this%griddim%dim%nx,this%griddim%dim%ny)

!TODO costretto a usare doubleprecision in quanto float non va (riportato bug grib_api)
doubleprecision :: vector (this%griddim%dim%nx * this%griddim%dim%ny)
doubleprecision,allocatable :: lats (:),lons(:)
integer ::x1,x2,xs,y1,y2,ys,ord(2)


call grib_get(this%gaid,'GRIBEditionNumber',EditionNumber)

call l4f_category_log(this%category,L4F_INFO,'Edition Number: '//to_char(EditionNumber))

if (EditionNumber == 2)then

  call grib_get(this%gaid,'alternativeRowScanning',alternativeRowScanning)
  if (alternativeRowScanning /= 0)then
    call l4f_category_log(this%category,L4F_ERROR,"alternativeRowScanning not supported: "//trim(to_char(alternativeRowScanning)))
    call raise_error('alternativeRowScanning not supported')
  end if

else if( EditionNumber /= 1)then

  call l4f_category_log(this%category,L4F_ERROR,"GribEditionNumber not supported: "//trim(to_char(EditionNumber)))
  call raise_error('GribEditionNumber not supported')

end if


call grib_get(this%gaid,'iScansNegatively',iScansNegatively)
call grib_get(this%gaid,'jScansPositively',jScansPositively)
call grib_get(this%gaid,'jPointsAreConsecutive',jPointsAreConsecutive)

call grib_set(this%gaid,'missingValue',rmiss)
call grib_get(this%gaid,'numberOfPoints',numberOfPoints)
call grib_get(this%gaid,'numberOfValues',numberOfValues)


if (numberOfPoints /= (this%griddim%dim%nx * this%griddim%dim%ny))then
!if (numberOfValues /= (this%griddim%dim%nx * this%griddim%dim%ny))then

  CALL l4f_category_log(this%category,L4F_ERROR, &
   'dencode_gridinfo: numberOfPoints and gridinfo size different. numberOfPoints: ' &
   //trim(to_char(numberOfPoints))//', nx,ny:'&
   //TRIM(to_char(this%griddim%dim%nx))//' '//trim(to_char(this%griddim%dim%ny)))
  call raise_fatal_error( &
   'dencode_gridinfo: numberOfPoints and gridinfo size different')

end if

                            !     get data values
call l4f_category_log(this%category,L4F_INFO,'number of values: '//to_char(numberOfValues))
call l4f_category_log(this%category,L4F_INFO,'number of points: '//to_char(numberOfPoints))

allocate(lats(numberOfPoints))
allocate(lons(numberOfPoints))
call grib_get_data(this%gaid,lats,lons,vector)
call l4f_category_log(this%category,L4F_INFO,'decoded')

deallocate(lats)
deallocate(lons)

!call grib_get(this%gaid,'values',vector)


! Transfer data field changing scanning mode to 64
IF (iScansNegatively  == 0) THEN
  x1 = 1
  x2 = this%griddim%dim%nx
  xs = 1
ELSE
  x1 = this%griddim%dim%nx
  x2 = 1
  xs = -1
ENDIF
IF (jScansPositively == 0) THEN
  y1 = this%griddim%dim%ny
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = this%griddim%dim%ny
  ys = 1
ENDIF

IF ( jPointsAreConsecutive == 0) THEN
  ord = (/1,2/)
ELSE
  ord = (/2,1/)
ENDIF


field(x1:x2:xs,y1:y2:ys) = &
 RESHAPE(vector, &
 (/this%griddim%dim%nx,this%griddim%dim%ny/), ORDER=ord)


end function decode_gridinfo



subroutine encode_gridinfo(this,field)
TYPE(gridinfo_type),INTENT(inout) :: this !< oggetto in cui codificare
REAL,intent(in) :: field (:,:) !< matrice dei dati da scrivere

integer :: EditionNumber
integer :: alternativeRowScanning,iScansNegatively,jScansPositively,jPointsAreConsecutive
integer :: numberOfValues,nx,ny
doubleprecision :: vector (this%griddim%dim%nx * this%griddim%dim%ny)
integer :: x1,x2,xs,y1,y2,ys,ord(2)

IF (SIZE(field,1) /= this%griddim%dim%nx &
 .OR. SIZE(field,2) /= this%griddim%dim%ny) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   'encode_gridinfo: field and gridinfo object non conformal, field: ' &
   //TRIM(to_char(SIZE(field,1)))//' '//TRIM(to_char(SIZE(field,1)))//', nx,ny:' &
   //TRIM(to_char(this%griddim%dim%nx))//' ' &
       //trim(to_char(this%griddim%dim%ny)))
  call raise_fatal_error('encode_gridinfo: field and gridinfo object non conformal')
ENDIF

if (.not. c_e(this%gaid))return


call grib_get(this%gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 2) then

  call grib_get(this%gaid,'alternativeRowScanning',alternativeRowScanning)
  if (alternativeRowScanning /= 0)then
    call l4f_category_log(this%category,L4F_ERROR,"alternativeRowScanning not supported: "//trim(to_char(alternativeRowScanning)))
    call raise_error('alternativeRowScanning not supported')
  end if

else if( EditionNumber /= 1) then

  call l4f_category_log(this%category,L4F_ERROR,"GribEditionNumber not supported: "//trim(to_char(EditionNumber)))
  call raise_error('GribEditionNumber not supported')

end if

call grib_get(this%gaid,'iScansNegatively',iScansNegatively)
call grib_get(this%gaid,'jScansPositively',jScansPositively)
call grib_get(this%gaid,'jPointsAreConsecutive',jPointsAreConsecutive)

call grib_get(this%gaid,'numberOfPointsAlongAParallel', nx)
call grib_get(this%gaid,'numberOfPointsAlongAMeridian',ny)

numberOfValues=nx*ny

if (numberOfValues /= (this%griddim%dim%nx * this%griddim%dim%ny))then

  CALL l4f_category_log(this%category,L4F_ERROR, &
   'encode_gridinfo: numberOfValues and gridinfo size different. numberOfValues: ' &
   //trim(to_char(numberOfValues))//', nx,ny:'&
   //TRIM(to_char(this%griddim%dim%nx))//' '//trim(to_char(this%griddim%dim%ny)))
  call raise_fatal_error( &
   'encode_gridinfo: numberOfValues and gridinfo size different')

end if

call l4f_category_log(this%category,L4F_INFO,'number of values: '//to_char(numberOfValues))


! Transfer data field changing scanning mode from 64
IF (iScansNegatively  == 0) THEN
  x1 = 1
  x2 = this%griddim%dim%nx
  xs = 1
ELSE
  x1 = this%griddim%dim%nx
  x2 = 1
  xs = -1
ENDIF
IF (jScansPositively == 0) THEN
  y1 = this%griddim%dim%ny
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = this%griddim%dim%ny
  ys = 1
ENDIF


if (any(field== rmiss)) then

  call grib_set(this%gaid,'missingValue',rmiss)
  
  call grib_get(this%gaid,'editionNumber',editionNumber);
  if (editionNumber == 1) then
                                ! enable bitmap in a grib1
    call grib_set(this%gaid,"bitmapPresent",1)
  else
                                ! enable bitmap in a grib2
    call grib_set(this%gaid,"bitMapIndicator",0)
  endif
end if

IF ( jPointsAreConsecutive == 0) THEN
  CALL grib_set(this%gaid,'values', PACK(field(x1:x2:xs,y1:y2:ys), .TRUE.))
ELSE
  CALL grib_set(this%gaid,'values', PACK(TRANSPOSE(field(x1:x2:xs,y1:y2:ys)), .TRUE.))
ENDIF

end subroutine encode_gridinfo




!derived from a work  Gilbert        ORG: W/NP11  SUBPROGRAM:    cnvlevel   DATE: 2003-06-12


SUBROUTINE level_g2_to_dballe(ltype1,scalef1,scalev1,ltype2,scalef2,scalev2, lt1,l1,lt2,l2)
integer,intent(in) :: ltype1,scalef1,scalev1,ltype2,scalef2,scalev2
integer,intent(out) ::lt1,l1,lt2,l2


CALL g2_to_dballe(ltype1, scalef1, scalev1, lt1, l1)
CALL g2_to_dballe(ltype2, scalef2, scalev2, lt2, l2)

CONTAINS

SUBROUTINE g2_to_dballe(ltype, scalef, scalev, lt, l)
integer,intent(in) :: ltype,scalef,scalev
integer,intent(out) ::lt,l

integer,parameter :: height(5)=(/102,103,106,117,160/)
doubleprecision:: sl

if (ltype == 255) then
  lt = imiss
  l = imiss
else
  lt = ltype
  sl = scalev*(10.D0**(-scalef))

  if (any(ltype == height)) then
    l=sl*1000.D0
  else
    l=sl
  end if
end if

END SUBROUTINE g2_to_dballe

END SUBROUTINE level_g2_to_dballe


SUBROUTINE level_dballe_to_g2(lt1,l1,lt2,l2, ltype1,scalef1,scalev1,ltype2,scalef2,scalev2)
integer,intent(in) ::lt1,l1,lt2,l2
integer,intent(out) :: ltype1,scalef1,scalev1,ltype2,scalef2,scalev2


CALL dballe_to_g2(lt1, l1, ltype1, scalef1, scalev1)
CALL dballe_to_g2(lt2, l2, ltype2, scalef2, scalev2)

CONTAINS

SUBROUTINE dballe_to_g2(lt, l, ltype, scalef, scalev)
integer,intent(in) ::lt,l
integer,intent(out) :: ltype,scalef,scalev

integer,parameter :: height(5)=(/102,103,106,117,160/)
doubleprecision:: sl

if (lt == imiss) then
  ltype = 255
  scalev = 0
  scalef = 0
else
  ltype = lt
  scalev = l
  if (any(ltype == height)) then
    scalef = 3
  else
    scalef = 0
  end if
endif

!Caso generale reale
!IF (ANY(ltype == height)) THEN
!  sl=l/1000.D0
!ELSE
!  sl=l
!END IF
!IF (ABS(sl) < PRECISION) THEN
!  scalef = 0
!  scalev = 0
!ELSE
!  magn = LOG10(ABS(sl))
!  DO scalef = magn, MAX(magn, 20)
!    IF (ABS((sl*10.D0**(scalef) - ANINT(sl*10.D0**(scalef))) / &
!     sl*10.D0**(scalef)) < PRECISION) EXIT
!  ENDDO
!  sl = scalev*(10.D0**(-scalef))
!ENDIF

END SUBROUTINE dballe_to_g2

END SUBROUTINE level_dballe_to_g2



SUBROUTINE level_g1_to_g2(ltype,l1,l2,ltype1,scalef1,scalev1,ltype2,scalef2,scalev2)
integer,intent(in) :: ltype,l1,l2
integer,intent(out) :: ltype1,scalef1,scalev1,ltype2,scalef2,scalev2

ltype1=255
scalef1=0
scalev1=0
ltype2=255
scalef2=0
scalev2=0

if (ltype > 0 .and. ltype <= 9)then 
   ltype1=ltype
else if (ltype == 20) then
  ltype1=20
  scalev1=l1
  scalef1=2
else if (ltype == 100) then
  ltype1=100
  scalev1=l1*100
else if (ltype == 101) then
  ltype1=100
  scalev1=l1*1000
  ltype2=100
  scalev2=l2*1000
else if (ltype == 102) then
  ltype1=101
else if (ltype == 103) then
  ltype1=102
  scalev1=l1
else if (ltype == 104) then
  ltype1=102
  scalev1=l1*100
  ltype2=102
  scalev2=l2*100
else if (ltype == 105) then
  ltype1=103
  scalev1=l1
else if (ltype == 106) then
  ltype1=103
  scalev1=l1*100
  ltype2=103
  scalev2=l2*100
else if (ltype == 107) then
  ltype1=104
  scalef1=4
  scalev1=l1
else if (ltype == 108) then
  ltype1=104
  scalef1=2
  scalev1=l1
  ltype2=104
  scalef2=2
  scalev2=l2
else if (ltype == 109) then
  ltype1=105
  scalev1=l1
else if (ltype == 110) then
  ltype1=105
  scalev1=l1
  ltype2=105
  scalev2=l2
else if (ltype == 111) then
  ltype1=106
  scalef1=2
  scalev1=l1
else if (ltype == 112) then
  ltype1=106
  scalef1=2
  scalev1=l1
  ltype2=106
  scalef2=2
  scalev2=l2
else if (ltype == 113) then
  ltype1=107
  scalev1=l1
else if (ltype == 114) then
  ltype1=107
  scalev1=475+l1
  ltype2=107
  scalev2=475+l2
else if (ltype == 115) then
  ltype1=108
  scalev1=l1*100
else if (ltype == 116) then
  ltype1=108
  scalev1=l1*100
  ltype2=108
  scalev2=l2*100
else if (ltype == 117) then
  ltype1=109
  scalef1=9
  scalev1=l1
  if ( btest(l1,15) ) then
    scalev1=-1*mod(l1,32768)
  endif
else if (ltype == 119) then
  ltype1=111
  scalef1=4
  scalev1=l1
else if (ltype == 120) then
  ltype1=111
  scalef1=2
  scalev1=l1
  ltype2=111
  scalef2=2
  scalev2=l2
else if (ltype == 121) then
  ltype1=100
  scalev1=(1100+l1)*100
  ltype2=100
  scalev2=(1100+l2)*100
else if (ltype == 125) then
  ltype1=103
  scalef1=2
  scalev1=l1
else if (ltype == 128) then
  ltype1=104
  scalef1=3
  scalev1=1100+l1
  ltype2=104
  scalef2=3
  scalev2=1100+l2
else if (ltype == 141) then
  ltype1=100
  scalev1=l1*100
  ltype2=100
  scalev2=(1100+l2)*100
else if (ltype == 160) then
  ltype1=160
  scalev1=l1
else

  call raise_error('level_g1_to_g2: GRIB1 Level '//TRIM(to_char(ltype))//' not recognized.')

endif

return
END SUBROUTINE level_g1_to_g2


SUBROUTINE level_g2_to_g1(ltype1,scalef1,scalev1,ltype2,scalef2,scalev2,ltype,l1,l2)
integer,intent(in) :: ltype1,scalef1,scalev1,ltype2,scalef2,scalev2
integer,intent(out) :: ltype,l1,l2

if (ltype1 > 0 .and. ltype1 <= 9 .and. ltype2 == 255) then ! simple
  ltype = ltype1
  l1 = 0
  l2 = 0
else if (ltype1 == 20 .and. ltype2 == 255) then ! isothermal
  ltype = 2
  l1 = rescale2(scalef1,scalev1-2)!*100
  l2 = 0
else if (ltype1 == 100 .and. ltype2 == 255) then ! isobaric
  ltype = 100
  l1 = rescale2(scalef1,scalev1+2)!/100
  l2 = 0
else if (ltype1 == 100 .and. ltype2 == 100) then
  ltype = 101
  l1 = rescale1(scalef1,scalev1+3)!/1000
  l2 = rescale1(scalef2,scalev2+3)!/1000
else if (ltype1 == 101 .and. ltype2 == 255) then
  ltype = 102
  l1 = 0
  l2 = 0
else if (ltype1 == 102 .and. ltype2 == 255) then ! altitude over sea level
  ltype = 103
  l1 = rescale2(scalef1,scalev1)
  l2 = 0
else if (ltype1 == 102 .and. ltype2 == 102) then
  ltype = 104
  l1 = rescale1(scalef1,scalev1+2)!/100
  l2 = rescale1(scalef2,scalev2+2)!/100
else if (ltype1 == 103 .and. ltype2 == 255) then ! height over ground
  ltype = 105
  l1 = rescale2(scalef1,scalev1)
  l2 = 0
else if (ltype1 == 103 .and. ltype2 == 103) then
  ltype = 106
  l1 = rescale1(scalef1,scalev1+2)!/100
  l2 = rescale1(scalef2,scalev2+2)!/100
else if (ltype1 == 104 .and. ltype2 == 255) then ! sigma
  ltype = 107
  l1 = rescale2(scalef1,scalev1-4)!*10000
  l2 = 0
else if (ltype1 == 104 .and. ltype2 == 104) then
  ltype = 108
  l1 = rescale1(scalef1,scalev1-2)!*100
  l2 = rescale1(scalef2,scalev2-2)!*100
else if (ltype1 == 105 .and. ltype2 == 255) then ! hybrid
  ltype = 109
  l1 = rescale2(scalef1,scalev1)
  l2 = 0
else if (ltype1 == 105 .and. ltype2 == 105) then
  ltype = 110
  l1 = rescale1(scalef1,scalev1)
  l2 = rescale1(scalef2,scalev2)
else if (ltype1 == 106 .and. ltype2 == 255) then ! depth
  ltype = 111
  l1 = rescale2(scalef1,scalev1-2)!*100
  l2 = 0
else if (ltype1 == 106 .and. ltype2 == 106) then
  ltype = 112
  l1 = rescale1(scalef1,scalev1-2)!*100
  l2 = rescale1(scalef2,scalev2-2)!*100
else ! mi sono rotto per ora

  ltype = 255
  l1 = 0
  l2 = 0
  call raise_error('level_g2_to_g1: GRIB2 Levels '//TRIM(to_char(ltype1))//' ' &
   //TRIM(to_char(ltype2))//' cannot be converted to GRIB1.')

endif

CONTAINS

FUNCTION rescale1(scalef, scalev) RESULT(rescale)
INTEGER,INTENT(in) :: scalef, scalev
INTEGER :: rescale

rescale = MIN(255, INT(scalev*10.0D0**(-scalef)))

END FUNCTION rescale1

FUNCTION rescale2(scalef, scalev) RESULT(rescale)
INTEGER,INTENT(in) :: scalef, scalev
INTEGER :: rescale

rescale = MIN(65535, INT(scalev*10.0D0**(-scalef)))

END FUNCTION rescale2

END SUBROUTINE level_g2_to_g1


end module gridinfo_class
