#include "config.h"
!> \brief Classe per la gestione delle informazioni di griglia dei grib.
!!
!! Questo modulo definisce gli oggetti e i metodi per gestire dati grib.
!! Viene definito un oggetto che associa ad un oggetto grib id delle grib_api una serie di informazioni
!! molto utili :
!! - area geografiche in proiezione e non, associate a dati su grigliato (gridded).
!! - descrittore della dimensione tempo
!! - descrittore della dimensione intervallo temporale (timerange)
!! - descrittore della dimensione livello verticale
!! - descrittore della dimensione variabile
!!
!! I metodi principali permettono di :
!! - estrarre dal grib queste informazioni  
!! - inserire nel grib queste informazioni
!! - estrarre i dati codificati dal grib
!! - codificare i dati nel grib
!!
!! Programma esempio semplice \include example_vg6d_2.f90
!! Programma esempio complesso \include example_vg6d_4.f90
!! \ingroup volgrid6d

module gridinfo_class

USE grid_class
USE datetime_class
USE vol7d_timerange_class
USE vol7d_level_class
USE volgrid6d_var_class
use grib_api
use log4fortran
use optional_values


IMPLICIT NONE

character (len=255),parameter:: subcategory="gridinfo_class"


!> Definisce un oggetto contenente le informazioni relative a un grib
type gridinfo_def


!> descrittore del grigliato
  type(griddim_def) :: griddim
!> descrittore della dimensione tempo
  TYPE(datetime) :: time
!> descrittore della dimensione intervallo temporale (timerange)
  TYPE(vol7d_timerange) :: timerange
!> descrittore della dimensione livello verticale
  TYPE(vol7d_level) :: level
!> vettore descrittore della dimensione variabile
  TYPE(volgrid6d_var) :: var
  integer ::  gaid !< grib_api id of the grib loaded in memory
  integer :: category !< log4fortran

end type gridinfo_def


!> \brief Costructor
!!
!! create a new istanze of object
INTERFACE init
  MODULE PROCEDURE init_gridinfo
END INTERFACE

!> \brief destructor
!!
!! delete object 
INTERFACE delete
  MODULE PROCEDURE delete_gridinfo
END INTERFACE

!> \brief clone object
!!
!! create a new istanze of object equal to the starting one 
INTERFACE clone
  MODULE PROCEDURE clone_gridinfo
END INTERFACE


!> \brief Import
!!
!! Legge i valori dal grib e li imposta appropriatamente
INTERFACE import
  MODULE PROCEDURE import_time,import_timerange,import_level,import_gridinfo, &
   import_volgrid6d_var
END INTERFACE

!> \brief Export
!!
!! Imposta i valori nel grib
INTERFACE export
  MODULE PROCEDURE export_time,export_timerange,export_level,export_gridinfo, &
   export_volgrid6d_var
END INTERFACE

!> \brief  Display object on screen
!!
!! show brief content on screen
INTERFACE display
  MODULE PROCEDURE display_gridinfo,display_gridinfov,display_gaid
END INTERFACE


private

public gridinfo_def,init,delete,import,export,clone
public display,decode_gridinfo,encode_gridinfo

contains

!> Inizializza un oggetto di tipo gridinfo.
SUBROUTINE init_gridinfo(this,gaid,griddim,time,timerange,level,var,clone,categoryappend)
TYPE(gridinfo_def),intent(out) :: this !< oggetto da inizializzare
integer,intent(in),optional ::  gaid !< grib_api id of the grib loaded in memory
!> descrittore del grigliato
type(griddim_def),intent(in),optional :: griddim
!> descrittore della dimensione tempo
TYPE(datetime),intent(in),optional :: time
!> descrittore della dimensione intervallo temporale (timerange)
TYPE(vol7d_timerange),intent(in),optional :: timerange
!> descrittore della dimensione livello verticale
TYPE(vol7d_level),intent(in),optional :: level
!> descrittore della dimensione variabile di anagrafica
TYPE(volgrid6d_var),intent(in),optional :: var
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona gaid to gridinfo
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< accoda questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
end if
this%category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start init gridinfo")
#endif

if (present(gaid))then

  if (optio_log(clone))then
    this%gaid=-1
    call grib_clone(gaid,this%gaid)
  else
    this%gaid=gaid
  end if
else
  this%gaid=imiss
end if

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"gaid present: "&
 //to_char(c_e(this%gaid))//" value: "//to_char(this%gaid))
#endif

if (present(griddim))then
  call copy(griddim,this%griddim)
else
  call init(this%griddim,categoryappend=categoryappend)
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


!> \brief destructor
!!
!! delete gridinfo object
!! relase memory and delete category for logging
subroutine delete_gridinfo (this)
TYPE(gridinfo_def),intent(out) :: this !< oggetto da eliminare

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start delete_gridinfo" )
#endif

call delete(this%griddim)
call delete(this%time)
call delete(this%timerange)
call delete(this%level)
call delete(this%var)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"delete gaid" )
#endif
call grib_release (this%gaid) 
this%gaid=imiss

!chiudo il logger
call l4f_category_delete(this%category)


end subroutine delete_gridinfo


!> \brief  clone gridinfo object
!!
!! create a new istanze of object equal to the starting one 
SUBROUTINE clone_gridinfo(this,that,categoryappend)
TYPE(gridinfo_def),intent(in) :: this !< oggetto da clonare
TYPE(gridinfo_def),intent(out) :: that !< oggetto clonato
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

that%gaid=-1
call grib_clone(this%gaid,that%gaid)

call copy(this%griddim,that%griddim)

that%time=this%time
that%timerange=this%timerange
that%level=this%level
that%var=this%var

!new category
call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
that%category=l4f_category_get(a_name)

end SUBROUTINE clone_gridinfo

!> Importa nell'oggetto gridinfo \a this il contenuto del messaggio grib
!! identificato da \a this%gaid precedentemente impostato.
!! una serie di informazioni vengono estratte dal grid e dopo qualche elaborazione 
!! vengono memorizzate nella struttura.
subroutine import_gridinfo (this)

TYPE(gridinfo_def),intent(out) :: this !< oggetto in cui importare


#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"import from gaid")
#endif

call import(this%griddim,this%gaid)
call import(this%time,this%gaid)
call import(this%timerange,this%gaid)
call import(this%level,this%gaid)
call import(this%var,this%gaid)

call normalize_gridinfo(this)

end subroutine import_gridinfo

!> Esporta il contenuto dell'oggetto gridinfo \a this in un messaggio grib
!! identificato da \a this%gaid precedentemente impostato.
!! Le informazioni contenute nella struttura dopo elcune elaborazioni vengono
!! forzate nel contenuto del grib 
subroutine export_gridinfo (this)

TYPE(gridinfo_def),intent(out) :: this !< oggetto da esportare

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"export to gaid" )
#endif

!attenzione: exportando da volgrid griddim è già esportato

if ( c_e(this%gaid)) then

!!$! ponghino per gribapi ....
!  call grib_set(this%gaid,"PVPresent",0)
!  call grib_set(this%gaid,"PLPresent",0)
!  call grib_set(this%gaid,"pvlLocation",255)
!!$! end ponghino

  call unnormalize_gridinfo(this)

  call export(this%griddim,this%gaid)
  call export(this%time,this%gaid)
  call export(this%timerange,this%gaid)
  call export(this%level,this%gaid)
  call export(this%var,this%gaid)

end if

end subroutine export_gridinfo


!> Importa nell'oggetto datetime \a this il contenuto del messaggio grib
!! identificato da \a gaid.
!! Le informazioni relative alla data vengono estratte dal grid e dopo qualche elaborazione 
!! vengono memorizzate nella struttura.
subroutine import_time(this,gaid)

TYPE(datetime),INTENT(out) :: this !< oggetto datetime in cui importare
integer,INTENT(in)         :: gaid !< grib_api id of the grib loaded in memory to import 

integer                    :: EditionNumber
character(len=9)           :: date
character(len=10)          :: time


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or.EditionNumber == 2 )then

  call grib_get(gaid,'dataDate',date )
  call grib_get(gaid,'dataTime',time(:5) )

  call init (this,simpledate=date(:8)//time(:4))

else

  CALL raise_error('GribEditionNumber not supported')

end if

end subroutine import_time



!> Esporta il contenuto dell'oggetto datetime \a this in un messaggio grib
!! identificato da \a gaid precedentemente impostato.
!! Le informazioni contenute nella struttura dopo alcune elaborazioni vengono
!! forzate nel contenuto del grib 
subroutine export_time(this,gaid)

TYPE(datetime),INTENT(in) :: this !< oggetto datetime da cui exportare
integer,INTENT(in)        :: gaid !< grib_api id of the grib loaded in memory to export

integer                   :: EditionNumber,date,time
character(len=17)         :: date_time


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or.EditionNumber == 2 )then

! datetime is AAAAMMGGhhmmssmsc
  call getval (this,simpledate=date_time)
  read(date_time(:8),*)date
  read(date_time(9:12),*)time
  call grib_set(gaid,'dataDate',date)
  call grib_set(gaid,'dataTime',time)

else

  CALL raise_error('GribEditionNumber not supported')

end if


end subroutine export_time



!> Importa nell'oggetto vol7d_level \a this il contenuto del messaggio grib
!! identificato da \a gaid.
!! Le informazioni relative al livello vengono estratte dal grid e dopo qualche elaborazione 
!! vengono memorizzate nella struttura.
subroutine import_level(this,gaid)

TYPE(vol7d_level),INTENT(out) :: this !< oggetto vol7d_level in cui importare
integer,INTENT(in)          :: gaid !< grib_api id of the grib loaded in memory to import

integer                     :: EditionNumber,level1,l1,level2,l2
integer :: ltype,ltype1,scalef1,scalev1,ltype2,scalef2,scalev2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then
  
  call grib_get(gaid,'indicatorOfTypeOfLevel',ltype)
  call grib_get(gaid,'topLevel',l1)
  call grib_get(gaid,'bottomLevel',l2)

  call level_g1_to_g2(ltype,l1,l2,ltype1,scalef1,scalev1,ltype2,scalef2,scalev2)
  IF (ltype == 111 .OR. ltype == 112) THEN
!    PRINT*,'import level',ltype1,scalef1,scalev1,ltype2,scalef2,scalev2,ltype,l2,l1
  ENDIF
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



!> Esporta il contenuto dell'oggetto vol7d_level \a this in un messaggio grib
!! identificato da \a gaid precedentemente impostato.
!! Le informazioni contenute nella struttura dopo alcune elaborazioni vengono
!! forzate nel contenuto del grib 
subroutine export_level(this,gaid)

TYPE(vol7d_level),INTENT(in) :: this !< oggetto vol7d_level da cui exportare
integer,INTENT(in) :: gaid !< grib_api id of the grib loaded in memory to export

INTEGER :: EditionNumber, ltype1, scalef1, scalev1, ltype2, scalef2, scalev2, &
 ltype, l1, l2

CALL level_dballe_to_g2(this%level1, this%l1, this%level2, this%l2, &
 ltype1, scalef1, scalev1, ltype2, scalef2, scalev2)

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  CALL level_g2_to_g1(ltype1,scalef1,scalev1,ltype2,scalef2,scalev2,ltype,l1,l2)
  IF (ltype == 111 .OR. ltype == 112) THEN
!    PRINT*,'export level',ltype1,scalef1,scalev1,ltype2,scalef2,scalev2,ltype,l2,l1
  ENDIF
  call grib_set(gaid,'indicatorOfTypeOfLevel',ltype)
! it is important to set topLevel after, otherwise, in case of single levels
! bottomLevel=0 overwrites topLevel (aliases in grib_api)
  call grib_set(gaid,'bottomLevel',l2)
  call grib_set(gaid,'topLevel',l1)

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



!> Importa nell'oggetto vol7d_timerange \a this il contenuto del messaggio grib
!! identificato da \a gaid.
!! Le informazioni relative al timerange vengono estratte dal grid e dopo qualche elaborazione 
!! vengono memorizzate nella struttura.
subroutine import_timerange(this,gaid)

TYPE(vol7d_timerange),INTENT(out) :: this !< oggetto vol7d_timerange in cui importare
integer,INTENT(in) :: gaid !< grib_api id of the grib loaded in memory to import

INTEGER :: EditionNumber, tri, unit, p1_g1, p2_g1, statproc, p1, p2, status

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1) then

  CALL grib_get(gaid,'timeRangeIndicator',tri)
  CALL grib_get(gaid,'P1',p1_g1)
  CALL grib_get(gaid,'P2',p2_g1)
  CALL grib_get(gaid,'indicatorOfUnitOfTimeRange',unit)
!  CALL grib_get(gaid,'startStepInHours',p1_g1)
!  CALL grib_get(gaid,'endStepInHours',p2_g1)
  CALL timerange_g1_to_g2_second(tri, p1_g1, p2_g1, unit, statproc, p1, p2)

else if (EditionNumber == 2) then
  
!  call grib_get(gaid,'productDefinitionTemplateNumber',)
!  call grib_get(gaid,'endStepInHours',p1)
  CALL grib_get(gaid,'forecastTime',p1)
  CALL grib_get(gaid,'indicatorOfUnitOfTimeRange',unit)
  CALL gribtr_to_second(unit,p1,p1)
  call grib_get(gaid,'typeOfStatisticalProcessing',statproc,status)

  if (status == GRIB_SUCCESS .AND. statproc >= 0 .AND. statproc <= 9) then ! statistical processing
     call grib_get(gaid,'lengthOfTimeRange',p2) 
     CALL grib_get(gaid,'indicatorOfUnitForTimeRange',unit)
     CALL gribtr_to_second(unit,p2,p2)
 
  else ! point in time
     statproc = 254
     p2 = 0
     
  end if
else

  call raise_fatal_error('GribEditionNumber not supported')

end if

call init (this, statproc, p1, p2)

end subroutine import_timerange


!> Esporta il contenuto dell'oggetto vol7d_timerange \a this in un messaggio grib
!! identificato da \a gaid precedentemente impostato.
!! Le informazioni contenute nella struttura dopo alcune elaborazioni vengono
!! forzate nel contenuto del grib 
subroutine export_timerange(this,gaid)

TYPE(vol7d_timerange),INTENT(in) :: this !< oggetto vol7d_timerange da cui exportare
integer,INTENT(in) :: gaid !< grib_api id of the grib loaded in memory to export

INTEGER :: EditionNumber, tri, unit, p1_g1, p2_g1, p1, p2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 ) then
! Convert vol7d_timerange members to grib1 with reasonable time unit
  CALL timerange_g2_to_g1_unit(this%timerange, this%p1, this%p2, &
   tri, p1_g1, p2_g1, unit)
! Set the native keys
  CALL grib_set(gaid,'timeRangeIndicator',tri)
  CALL grib_set(gaid,'P1',p1_g1)
  CALL grib_set(gaid,'P2',p2_g1)
  CALL grib_set(gaid,'indicatorOfUnitOfTimeRange',unit)

else if (EditionNumber == 2) then
! Set reasonable time unit
  CALL second_to_gribtr(this%p1,p1,unit)
! Set the native keys
  CALL grib_set(gaid,'forecastTime',p1)
  CALL grib_set(gaid,'indicatorOfUnitOfTimeRange',unit)
!    CALL grib_set(gaid,'endStepInHours',this%p1)

!TODO bisogna gestire anche il template
  IF (this%timerange >= 0 .AND. this%timerange <= 9) THEN
    CALL grib_set(gaid,'typeOfStatisticalProcessing',this%timerange)
    CALL second_to_gribtr(this%p2,p2,unit)
    CALL grib_set(gaid,'indicatorOfUnitForTimeRange',unit)
    CALL grib_set(gaid,'lengthOfTimeRange',p2)
  ELSE

    call raise_error('typeOfStatisticalProcessing not supported: '//trim(to_char(this%timerange)))
    
  ENDIF

else

  call raise_fatal_error('GribEditionNumber not supported')

end if

end subroutine export_timerange


!> Importa nell'oggetto volgrid6d_var \a this il contenuto del messaggio grib
!! identificato da \a gaid.
!! Le informazioni relative al parametro vengono estratte dal grid e dopo qualche elaborazione 
!! vengono memorizzate nella struttura.
subroutine import_volgrid6d_var(this,gaid)

TYPE(volgrid6d_var),INTENT(out) :: this !< oggetto volgrid6d_var in cui importare
integer,INTENT(in)              :: gaid !< grib_api id of the grib loaded in memory to import

integer ::EditionNumber,centre,discipline,category,number

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  call grib_get(gaid,'identificationOfOriginatingGeneratingCentre',centre)
  call grib_get(gaid,'gribTablesVersionNo',category)
  call grib_get(gaid,'indicatorOfParameter',number)

  call init (this, centre, category, number)

else if (EditionNumber == 2)then

  call grib_get(gaid,'identificationOfOriginatingGeneratingCentre',centre)
  call grib_get(gaid,'discipline',discipline)
  call grib_get(gaid,'parameterCategory',category)
  call grib_get(gaid,'parameterNumber',number)

  call init (this, centre, category, number, discipline)
  
else

  CALL raise_error('GribEditionNumber not supported')

end if
                                ! da capire come ottenere 
!this%description
!this%unit

end subroutine import_volgrid6d_var


!> Esporta il contenuto dell'oggetto volgrid6d_var \a this in un messaggio grib
!! identificato da \a gaid precedentemente impostato.
!! Le informazioni contenute nella struttura dopo alcune elaborazioni vengono
!! forzate nel contenuto del grib 
subroutine export_volgrid6d_var(this,gaid)

TYPE(volgrid6d_var),INTENT(in) :: this !< oggetto volgrid6d_var da cui exportare
integer,INTENT(in)             :: gaid !< grib_api id of the grib loaded in memory to export
integer ::EditionNumber

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  call grib_set(gaid,'identificationOfOriginatingGeneratingCentre',this%centre)
  call grib_set(gaid,'gribTablesVersionNo',this%category)
  call grib_set(gaid,'indicatorOfParameter',this%number)

else if (EditionNumber == 2)then

  call grib_set(gaid,'identificationOfOriginatingGeneratingCentre',this%centre)
  call grib_set(gaid,'discipline',this%discipline)
  call grib_set(gaid,'parameterCategory',this%category)
  call grib_set(gaid,'parameterNumber',this%number)

else

  CALL raise_error('GribEditionNumber not supported')

end if
                                ! da capire come ottenere 
!this%description
!this%unit

end subroutine export_volgrid6d_var

!> \brief Display object on screen
!!
!! Show brief content on screen.
!! All the key names and values will be printed
!! The set of keys returned can be controlled with the input variable namespace.
!! Available namespaces are "ls" (to get the same default keys as the grib_ls and "mars" to get the keys used by mars.
subroutine display_gaid (this,namespace)

integer :: this !< grib_api id of the grib loaded in memory
character (len=*),optional :: namespace !< grib_api namespace of the keys to search for (all the keys if empty)

integer :: kiter,iret
character(len=255) :: key,value,lnamespace

lnamespace=optio_c(namespace,255)
if ( .not. c_e(lnamespace) )then
 lnamespace="ls"
endif

print*,"GRIB_API namesapce:",trim(lnamespace)

call grib_keys_iterator_new(this,kiter,namespace=trim(lnamespace))

iter: do
  call grib_keys_iterator_next(kiter, iret) 
  
  if (iret .ne. 1) then
    exit iter
  end if
  
  call grib_keys_iterator_get_name(kiter,key)
!<\todo bug in grib_api, segmentation fault with computeStatistics key
  if (key == 'computeStatistics') cycle
  call grib_get(this,trim(key),value,iret)
  if (iret == 0)then
    print*, trim(key)//' = '//trim(value)
  else
    print*, trim(key)//' = '//"KEY NOT FOUND, namespace :"//trim(lnamespace)//" ( bug ? )"
  end if
end do iter

call grib_keys_iterator_delete(kiter)

end subroutine display_gaid


!> \brief Display object on screen
!!
!! Show brief content on screen.
!! Also the grib  key names and values will be printed
!! The set of keys returned can be controlled with the input variable namespace.
!! Available namespaces are "ls" (to get the same default keys as the grib_ls and "mars" to get the keys used by mars.
subroutine display_gridinfo (this,namespace)

TYPE(gridinfo_def),intent(in) :: this !< object to display
character (len=*),optional :: namespace !< grib_api namespace of the keys to search for (all the keys if empty)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"ora mostro gridinfo " )
#endif

print*,"----------------------- gridinfo display ---------------------"
call display(this%griddim)
call display(this%time)
call display(this%timerange)
call display(this%level)
call display(this%var)
call display(this%gaid,namespace=namespace)
print*,"--------------------------------------------------------------"


end subroutine display_gridinfo



!> \brief Display vector of object on screen
!!
!! Show brief content on screen.
!! Also the grib  key names and values will be printed
!! The set of keys returned can be controlled with the input variable namespace.
!! Available namespaces are "ls" (to get the same default keys as the grib_ls and "mars" to get the keys used by mars.
subroutine display_gridinfov (this,namespace)

TYPE(gridinfo_def),intent(in) :: this(:) !< vector of object to display
character (len=*),optional :: namespace !< grib_api namespace of the keys to search for (all the keys if empty)
integer :: i

print*,"----------------------- gridinfo  vector ---------------------"

do i=1, size(this)

#ifdef DEBUG
  call l4f_category_log(this(i)%category,L4F_DEBUG,"ora mostro il vettore gridinfo " )
#endif

  call display(this(i),namespace)

end do
print*,"--------------------------------------------------------------"

end subroutine display_gridinfov


!> \brief Decode gridinfo object.
!!
!! Decode from a grib message the data section returning a data matrix.
function decode_gridinfo(this) result (field)
TYPE(gridinfo_def),INTENT(in) :: this      !< oggetto da decodificare
real :: field (this%griddim%dim%nx,this%griddim%dim%ny) !< data matrix of decoded values

integer :: EditionNumber
integer :: alternativeRowScanning,iScansNegatively,jScansPositively,jPointsAreConsecutive
integer :: numberOfValues,numberOfPoints

real :: vector(this%griddim%dim%nx * this%griddim%dim%ny)
integer :: x1,x2,xs,y1,y2,ys,ord(2)


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
   'decode_gridinfo: numberOfPoints and gridinfo size different. numberOfPoints: ' &
   //trim(to_char(numberOfPoints))//', nx,ny:'&
   //TRIM(to_char(this%griddim%dim%nx))//' '//trim(to_char(this%griddim%dim%ny)))
  call raise_fatal_error( &
   'decode_gridinfo: numberOfPoints and gridinfo size different')

end if

                            !     get data values
call l4f_category_log(this%category,L4F_INFO,'number of values: '//to_char(numberOfValues))
call l4f_category_log(this%category,L4F_INFO,'number of points: '//to_char(numberOfPoints))

CALL grib_get(this%gaid,'values',vector)

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



!> \brief Encode gridinfo object.
!!
!! Encode from a data matrix a data section of a grib message.
subroutine encode_gridinfo(this,field)
TYPE(gridinfo_def),INTENT(inout) :: this !< oggetto in cui codificare
REAL,intent(in) :: field (:,:) !< data matrix to encode

integer :: EditionNumber
integer :: alternativeRowScanning,iScansNegatively,jScansPositively,jPointsAreConsecutive
integer :: nx,ny
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

! queste sono gia` fatte in export_gridinfo, si potrebbero evitare?!
call grib_set(this%gaid,'Ni',this%griddim%dim%nx)
call grib_set(this%gaid,'Nj',this%griddim%dim%ny)

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

else

  call grib_get(this%gaid,'editionNumber',editionNumber);
  if (editionNumber == 1) then
                                ! enable bitmap in a grib1
    call grib_set(this%gaid,"bitmapPresent",0)
  else
                                ! enable bitmap in a grib2
    call grib_set(this%gaid,"bitMapIndicator",1)
  endif

end if


!TODO: gestire il caso TUTTI dati mancanti

IF ( jPointsAreConsecutive == 0) THEN
  CALL grib_set(this%gaid,'values', RESHAPE(field(x1:x2:xs,y1:y2:ys), &
   (/this%griddim%dim%nx*this%griddim%dim%ny/)))
ELSE
  CALL grib_set(this%gaid,'values', RESHAPE(TRANSPOSE(field(x1:x2:xs,y1:y2:ys)), &
   (/this%griddim%dim%nx*this%griddim%dim%ny/)))
ENDIF

end subroutine encode_gridinfo




!derived from a work of Gilbert  ORG: W/NP11  SUBPROGRAM:    cnvlevel   DATE: 2003-06-12


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
    l = NINT(sl*1000.D0)
  else
    l = NINT(sl)
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

  call raise_error('level_g1_to_g2: GRIB1 level '//TRIM(to_char(ltype)) &
   //' cannot be converted to GRIB2.')

endif

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
  l1 = rescale2(scalef1-2,scalev1)!*100
  l2 = 0
else if (ltype1 == 100 .and. ltype2 == 255) then ! isobaric
  ltype = 100
  l1 = rescale2(scalef1+2,scalev1)!/100
  l2 = 0
else if (ltype1 == 100 .and. ltype2 == 100) then
  ltype = 101
  l1 = rescale1(scalef1+3,scalev1)!/1000
  l2 = rescale1(scalef2+3,scalev2)!/1000
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
  l1 = rescale1(scalef1+2,scalev1)!/100
  l2 = rescale1(scalef2+2,scalev2)!/100
else if (ltype1 == 103 .and. ltype2 == 255) then ! height over ground
  ltype = 105
  l1 = rescale2(scalef1,scalev1)
  l2 = 0
else if (ltype1 == 103 .and. ltype2 == 103) then
  ltype = 106
  l1 = rescale1(scalef1+2,scalev1)!/100
  l2 = rescale1(scalef2+2,scalev2)!/100
else if (ltype1 == 104 .and. ltype2 == 255) then ! sigma
  ltype = 107
  l1 = rescale2(scalef1,scalev1-4)!*10000
  l2 = 0
else if (ltype1 == 104 .and. ltype2 == 104) then
  ltype = 108
  l1 = rescale1(scalef1-2,scalev1)!*100
  l2 = rescale1(scalef2-2,scalev2)!*100
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
  l1 = rescale2(scalef1-2,scalev1)!*100
  l2 = 0
else if (ltype1 == 106 .and. ltype2 == 106) then
  ltype = 112
  l1 = rescale1(scalef1-2,scalev1)!*100
  l2 = rescale1(scalef2-2,scalev2)!*100
else ! mi sono rotto per ora

  ltype = 255
  l1 = 0
  l2 = 0
  call raise_error('level_g2_to_g1: GRIB2 levels '//TRIM(to_char(ltype1))//' ' &
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


SUBROUTINE timerange_g1_to_g2_second(tri, p1_g1, p2_g1, unit, statproc, p1, p2)
INTEGER,INTENT(in) :: tri, p1_g1, p2_g1, unit
INTEGER,INTENT(out) :: statproc, p1, p2

IF (tri == 0 .OR. tri == 1 .OR. tri == 10) THEN ! point in time
  statproc = 254
  CALL gribtr_to_second(unit, p1_g1, p1)
  p2 = 0
ELSE IF (tri == 2) THEN ! somewhere between p1 and p2, not good for grib2 standard
  statproc = 205
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE IF (tri == 3) THEN ! average
  statproc = 0
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE IF (tri == 4) THEN ! accumulation
  statproc = 1
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE IF (tri == 5) THEN ! difference
  statproc = 4
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE IF (tri == 13) THEN ! nudging - COSMO, use a temporary value then normalize
  statproc = 206 ! check if 206 is legal!
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE
  CALL raise_fatal_error('timerange_g1_to_g2: GRIB1 timerange '//TRIM(to_char(tri)) &
   //' cannot be converted to GRIB2.')
ENDIF

if (statproc == 254 .and. p2 /= 0 ) then
  call l4f_log(L4F_WARN,"inconsistence in timerange:254,"//trim(to_char(p1))//","//trim(to_char(p2)))
end if

END SUBROUTINE timerange_g1_to_g2_second


SUBROUTINE timerange_g2_to_g1_unit(statproc, p1, p2, tri, p1_g1, p2_g1, unit)
INTEGER,INTENT(in) :: statproc, p1, p2
INTEGER,INTENT(out) :: tri, p1_g1, p2_g1, unit

INTEGER :: ptmp

IF (statproc == 0) THEN ! average
  tri = 3
  CALL second_to_gribtr(p1, p2_g1, unit)    ! here and after make sure that
  CALL second_to_gribtr(p1-p2, p1_g1, unit) ! unit is the same between calls
ELSE IF (statproc == 1) THEN ! accumulation
  tri = 4
  CALL second_to_gribtr(p1, p2_g1, unit)
  CALL second_to_gribtr(p1-p2, p1_g1, unit)
ELSE IF (statproc == 4) THEN ! difference
  tri = 5
  CALL second_to_gribtr(p1, p2_g1, unit)
  CALL second_to_gribtr(p1-p2, p1_g1, unit)
ELSE IF (statproc == 205) THEN ! is not good for grib2 standard
  tri = 2
  CALL second_to_gribtr(p1, p2_g1, unit)
  CALL second_to_gribtr(p1-p2, p1_g1, unit)
ELSE IF (statproc == 254) THEN ! point in time
  tri = 0
  CALL second_to_gribtr(p1, p1_g1, unit)
  p2_g1 = 0
ELSE
  CALL raise_fatal_error('timerange_g2_to_g1: GRIB2 statisticalprocessing ' &
   //TRIM(to_char(statproc))//' cannot be converted to GRIB1.')
ENDIF

! p1 < 0 is not allowed, use COSMO trick
IF (p1_g1 < 0) THEN
  ptmp = p1_g1
  p1_g1 = 0
  p2_g1 = p2_g1 - ptmp
  tri = 13
ENDIF

END SUBROUTINE timerange_g2_to_g1_unit


!0       Minute
!1       Hour
!2       Day
!3       Month
!4       Year
!5       Decade (10 years)
!6       Normal (30 years)
!7       Century(100 years)
!8-9     Reserved
!10      3 hours
!11      6 hours
!12      12 hours
!13      Second
SUBROUTINE gribtr_to_second(unit, valuein, valueout)
INTEGER,INTENT(in) :: unit, valuein
INTEGER,INTENT(out) :: valueout

INTEGER,PARAMETER :: unitlist(0:13)=(/60,3600,86400,2592000,31536000, &
 315360000,946080000,imiss,imiss,imiss,10800,21600,43200,1/)

IF (unit >= 0 .AND. unit <= 13) THEN
  valueout = valuein*unitlist(unit)
ELSE
  valueout = imiss
ENDIF

END SUBROUTINE gribtr_to_second


SUBROUTINE second_to_gribtr(valuein, valueout, unit)
INTEGER,INTENT(in) :: valuein
INTEGER,INTENT(out) :: valueout, unit

IF (valuein == imiss) THEN
  valueout = imiss
  unit = 1
ELSE IF (MOD(valuein,3600) == 0) THEN ! prefer hours
  valueout = valuein/3600
  unit = 1
ELSE IF (MOD(valuein,60) == 0) THEN ! then minutes
  valueout = valuein/60
  unit = 0
ELSE ! seconds (not supported in grib1!)
  valueout = valuein
  unit = 13
ENDIF

END SUBROUTINE second_to_gribtr


! Standardize variables and timerange in DB-all.e thinking
subroutine normalize_gridinfo(this)
TYPE(gridinfo_def),intent(inout) :: this

if (this%timerange%timerange == 205) then

!tmin
  if (this%var == volgrid6d_var_new(255,2,16,255)) then
    this%var%number=11
    this%timerange%timerange=3
    return
  end if

!tmax
  if (this%var == volgrid6d_var_new(255,2,15,255)) then
    this%var%number=11
    this%timerange%timerange=2
    return
  end if

! wind max DWD
  if (this%var == volgrid6d_var_new(78,201,187,255)) then
    this%var%category=2
    this%var%number=32
    this%timerange%timerange=2
    return
  end if

! wind max SIMC
  if (this%var == volgrid6d_var_new(200,201,187,255)) then
    this%var%category=2
    this%var%number=32
    this%timerange%timerange=2
    return
  end if

else if (this%timerange%timerange == 206) then ! COSMO specific (any center)

  if (this%timerange%p1 == 0 .AND. this%timerange%p2 == 0) then ! instantaneous
    this%timerange%timerange=254

  else ! guess timerange according to parameter
    this%timerange%p1=0 ! correct back to present

! table 2
    if (this%var == volgrid6d_var_new(this%var%centre,2,11,255)) then ! T
      this%timerange%timerange=0 ! average

    else if (this%var == volgrid6d_var_new(this%var%centre,2,15,255)) then ! T
      this%var%number=11 ! reset also parameter
      this%timerange%timerange=2 ! maximum

    else if (this%var == volgrid6d_var_new(this%var%centre,2,16,255)) then ! T
      this%timerange%timerange=3 ! minimum
      this%var%number=11 ! reset also parameter
      return

    else if (this%var == volgrid6d_var_new(this%var%centre,2,17,255)) then ! TD
      this%timerange%timerange=0 ! average

    else if (this%var == volgrid6d_var_new(this%var%centre,2,33,255)) then ! U
      this%timerange%timerange=0 ! average

    else if (this%var == volgrid6d_var_new(this%var%centre,2,34,255)) then ! V
      this%timerange%timerange=0 ! average

    else if (this%var == volgrid6d_var_new(this%var%centre,2,57,255)) then ! evaporation
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,2,61,255)) then ! TOT_PREC
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,2,78,255)) then ! SNOW_CON
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,2,79,255)) then ! SNOW_GSP
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,2,90,255)) then ! RUNOFF
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,2,111,255)) then ! fluxes
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,112,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,113,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,114,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,121,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,122,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,124,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,125,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,126,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,2,127,255)) then
      this%timerange%timerange=0 ! average

! table 201
    else if (this%var == volgrid6d_var_new(this%var%centre,201,5,255)) then ! photosynthetic flux
      this%timerange%timerange=0 ! average

    else if (this%var == volgrid6d_var_new(this%var%centre,201,20,255)) then ! SUN_DUR
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,201,22,255)) then ! radiation fluxes
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,201,23,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,201,24,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,201,25,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,201,26,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,201,27,255)) then
      this%timerange%timerange=0 ! average

    else if (this%var == volgrid6d_var_new(this%var%centre,201,42,255)) then ! water divergence
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,201,102,255)) then ! RAIN_GSP
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,201,113,255)) then ! RAIN_CON
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,201,132,255)) then ! GRAU_GSP
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,201,135,255)) then ! HAIL_GSP
      this%timerange%timerange=1 ! accumulated

    else if (this%var == volgrid6d_var_new(this%var%centre,201,187,255)) then ! UVMAX
      this%var%category=2 ! really reset also parameter?
      this%var%number=32
      this%timerange%timerange=2 ! maximum

    else if (this%var == volgrid6d_var_new(this%var%centre,201,218,255)) then ! maximum 10m dynamical gust
      this%timerange%timerange=2 ! maximum

    else if (this%var == volgrid6d_var_new(this%var%centre,201,219,255)) then ! maximum 10m convective gust
      this%timerange%timerange=2 ! maximum

! table 202
    else if (this%var == volgrid6d_var_new(this%var%centre,202,231,255)) then ! sso parameters
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,202,232,255)) then
      this%timerange%timerange=0 ! average
    else if (this%var == volgrid6d_var_new(this%var%centre,202,233,255)) then
      this%timerange%timerange=0 ! average

    else ! parameter not recognized, set instantaneous?

      call l4f_category_log(this%category,L4F_WARN, &
       'normalize_gridinfo: found COSMO non instantaneous analysis 13/206,'//&
       TRIM(to_char(this%timerange%p1))//','//TRIM(to_char(this%timerange%p1)))
      call l4f_category_log(this%category,L4F_WARN, &
       'associated to an apparently instantaneous parameter '//&
       TRIM(to_char(this%var%centre))//','//TRIM(to_char(this%var%category))//','//&
       TRIM(to_char(this%var%number))//','//TRIM(to_char(this%var%discipline)))
      call l4f_category_log(this%category,L4F_WARN, 'forcing to instantaneous')

      this%timerange%p1 = 0
      this%timerange%p2 = 0
      this%timerange%timerange = 254

    end if
  end if ! guess timerange
end if ! 206

end subroutine normalize_gridinfo



! Destandardize variables and timerange from DB-all.e thinking
subroutine unnormalize_gridinfo(this)
TYPE(gridinfo_def),intent(inout) :: this
type (volgrid6d_var)::var

if (this%timerange%timerange == 3 )then

!tmin
  call init (var,255,2,11,255)
  if (var == this%var ) then
    this%var%number=16
    this%timerange%timerange=205
    return
  end if

else if (this%timerange%timerange == 2 )then

!tmax
  call init (var,255,2,11,255)
  if (var == this%var ) then
    this%var%number=15
    this%timerange%timerange=205
    return
  end if

! wind max DWD
  call init (var,78,2,32,255)
  if (var == this%var ) then
    this%var%category=201
    this%var%number=187
    this%timerange%timerange=205
    return
  end if

! wind max SIMC
  call init (var,200,2,32,255)
  if (var == this%var ) then
    this%var%category=201
    this%var%number=187
    this%timerange%timerange=205
    return
  end if

end if


end subroutine unnormalize_gridinfo



end module gridinfo_class


!>\example example_vg6d_2.f90
!!\brief Programma esempio semplice per la lettura di file grib.
!!
!! Programma che legge i grib contenuti in un file e li organizza in un vettore di oggetti gridinfo


!>\example example_vg6d_4.f90
!!\brief Programma esempio semplice per la elaborazione di file grib.
!!
!! Programma che legge uno ad uno i grid contenuti in un file e li
!! elabora producendo un file di output contenente ancora grib
