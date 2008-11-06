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

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

!call l4f_category_log(this%category,L4F_DEBUG,"init grid type: "//this%grid%type%type )


if (present(gaid))then
  this%gaid=gaid
else
  this%gaid=imiss
end if

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


call l4f_category_log(this%category,L4F_DEBUG,"ora provo ad exportare da grib " )


call export(this%griddim,this%gaid)
call export(this%time,this%gaid)
call export(this%timerange,this%gaid)
call export(this%level,this%gaid)
call export(this%var,this%gaid)


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
integer                   :: EditionNumber
character(len=17)         :: date_time


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or.EditionNumber == 2 )then

! datetime is AAAAMMGGhhmmssmsc
  call getval (this,simpledate=date_time)
  call grib_set(gaid,'dataDate',date_time(:8))
  call grib_set(gaid,'dataTime',date_time(9:14))

else

  CALL raise_error('GribEditionNumber not supported')

end if


end subroutine export_time



subroutine import_level(this,gaid)

TYPE(vol7d_level),INTENT(out) :: this
integer,INTENT(in)          :: gaid
integer                     :: EditionNumber,level1,l1,level2,l2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then
  
  call grib_get(gaid,'indicatorOfTypeOfLevel',level1)
  call grib_get(gaid,'topLevel',l1)
  level2=imiss
  call grib_get(gaid,'bottomLevel',l2)

  call init (this,level1,l1,level2,l2)

else if (EditionNumber == 2)then

  call grib_get(gaid,'typeOfFirstFixedSurface',level1)
  call grib_get(gaid,'scaledValueOfFirstFixedSurface',l1)
  call grib_get(gaid,'typeOfSecondFixedSurface',level2)     ! (missing=255)
  call grib_get(gaid,'scaledValueOfSecondFixedSurface',l2)

  call init (this,level1,l1,level2,l2)
  
else

  call raise_error('GribEditionNumber not supported')

end if

end subroutine import_level



subroutine export_level(this,gaid)

TYPE(vol7d_level),INTENT(in) :: this
integer,INTENT(in)         :: gaid
integer                     :: EditionNumber,level1,l1,level2,l2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  call grib_set(gaid,'indicatorOfTypeOfLevel',this%level1)
  call grib_set(gaid,'topLevel',this%l1)
  call grib_set(gaid,'bottomLevel',this%l2)


else if (EditionNumber == 2)then

  call grib_set(gaid,'typeOfFirstFixedSurface',this%level1)     ! (missing=255)
  call grib_set(gaid,'scaledValueOfFirstFixedSurface',this%l1)
  call grib_set(gaid,'typeOfSecondFixedSurface',this%level2)
  call grib_set(gaid,'scaledValueOfSecondFixedSurface',this%l2)

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

if (EditionNumber == 1 .or. EditionNumber == 2)then

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

TYPE(gridinfo_type),INTENT(in)  :: this      !< oggetto da decodificare
integer                     :: EditionNumber
integer  :: alternativeRowScanning,iScansNegatively,jScansPositively,jPointsAreConsecutive
integer :: numberOfValues
real  :: field (this%griddim%dim%nx,this%griddim%dim%ny)
real  :: vector (this%griddim%dim%nx * this%griddim%dim%ny)
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

call grib_get(this%gaid,'numberOfValues',numberOfValues)


if (numberOfValues /= (this%griddim%dim%nx * this%griddim%dim%ny))then

  call l4f_category_log(this%category,L4F_INFO,'nx: '//to_char(this%griddim%dim%nx)&
       //' ny: '//to_char(this%griddim%dim%ny)//to_char(this%griddim%dim%nx*this%griddim%dim%ny))
  call l4f_category_log(this%category,L4F_ERROR,'number of values disagree with nx,ny: '//to_char(numberOfValues))
  call raise_error('number of values disagree with nx,ny')

end if

                            !     get data values
call l4f_category_log(this%category,L4F_INFO,'number of values: '//to_char(numberOfValues))

call grib_set(this%gaid,'missingValue',rmiss)
call grib_get(this%gaid,'values',vector)


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

TYPE(gridinfo_type),INTENT(in)  :: this      !< oggetto in cui codificare
real  :: field (this%griddim%dim%nx,this%griddim%dim%ny) !< matrice dei dati da scrivere

integer                     :: EditionNumber
integer  :: alternativeRowScanning,iScansNegatively,jScansPositively,jPointsAreConsecutive
integer :: numberOfValues,nx,ny
real  :: vector (this%griddim%dim%nx * this%griddim%dim%ny)
integer ::x1,x2,xs,y1,y2,ys,ord(2)


call grib_get(this%gaid,'GRIBEditionNumber',EditionNumber)


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

call grib_get(this%gaid,"numberOfPointsAlongAParallel", nx)
call grib_get(this%gaid,"numberOfPointsAlongAMeridian",ny)

numberOfValues=nx*ny

if (numberOfValues /= (this%griddim%dim%nx * this%griddim%dim%ny))then

  call l4f_category_log(this%category,L4F_INFO,'nx: '//to_char(this%griddim%dim%nx)&
       //' ny: '//to_char(this%griddim%dim%ny))
  call l4f_category_log(this%category,L4F_ERROR,'number of values different nx,ny: '//to_char(numberOfValues))
  call raise_error('number of values different nx,ny')

end if

call l4f_category_log(this%category,L4F_INFO,'number of values: '//to_char(numberOfValues))


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

call grib_set(this%gaid,'missingValue',rmiss)
call grib_set(this%gaid,'values',pack(field,mask=.true.))


end subroutine encode_gridinfo



end module gridinfo_class
