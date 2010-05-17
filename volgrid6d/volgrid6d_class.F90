#include "config.h"
!> \defgroup volgrid6d Libsim package, volgrid6d library.

!> \brief Questo modulo definisce gli oggetti e i metodi per gestire dati in proiezione e non su grigliato regolare(gridded).
!! I dati vengono organizzati in una matrice mutidimensionale le cui dimensioni sono definite in modo standard.
!! Vengono gestiti differenti sistemi di coordinate geografiche e proiezioni.
!! L'oggetto principale volgrid6d contiene le informazioni e i dati relativi a un grigliato omogeneo. 
!! Un vettore di oggetti volgrid6d è in grado di contenere ogni miscela di dati.
!! I dati possono essere importati ed exportati da grib edition 1 e 2.
!!
!! Programma esempio semplice \include example_vg6d_3.f90
!! Programma trasformazione da volgrid6d a volgrid6d \include example_vg6d_5.f90
!! Programma scrittura su file vettore di anagrafica \include example_vg6d_8.f90
!! Programma trasformazione da volgrid6d a vol7d \include example_vg6d_6.f90
!! Programma trasformazione da vol7d a volgrid7d \include example_vg6d_7.f90
!!
!!\ingroup volgrid6d

module volgrid6d_class

USE grid_class
USE grid_transform_class
USE datetime_class
USE vol7d_timerange_class
USE vol7d_level_class
USE volgrid6d_var_class
use log4fortran
USE vol7d_utilities
#ifdef HAVE_LIBGRIBAPI
use gridinfo_class
use grib_api
#endif
use optional_values
use vol7d_class
use file_utilities

IMPLICIT NONE

character (len=255),parameter:: subcategory="volgrid6d_class"


!> Definisce un oggetto contenente le informazioni e i dati relativi a un grigliato omogeneo
type volgrid6d

!> descrittore del grigliato
  type(griddim_def) :: griddim
!> descrittore della dimensione tempo
  TYPE(datetime),pointer :: time(:)
!> descrittore della dimensione intervallo temporale (timerange)
  TYPE(vol7d_timerange),pointer :: timerange(:)
!> descrittore della dimensione livello verticale
  TYPE(vol7d_level),pointer :: level(:)
!> vettore descrittore della dimensione variabile
  TYPE(volgrid6d_var),pointer :: var(:)
!> matrix of grib_api id of the grib loaded in memory;
!! index are: (level,time,timerange,var)
  integer,pointer :: gaid(:,:,:,:)
!> data matrix; index are: (x,y,level,time,timerange,var)
  real,pointer :: voldati(:,:,:,:,:,:)
!> time definition; 0=time is reference time ; 1=time is validity time
  integer :: time_definition

  integer :: category !< log4fortran

end type volgrid6d

TYPE conv_func
  REAL :: a, b
END TYPE conv_func

TYPE(conv_func), PARAMETER :: conv_func_miss=conv_func(rmiss,rmiss)

TYPE vg6d_v7d_var_conv
  TYPE(volgrid6d_var) :: vg6d_var
  TYPE(vol7d_var) :: v7d_var
  TYPE(conv_func) :: c_func
! aggiungere informazioni ad es. su rotazione del vento
END TYPE vg6d_v7d_var_conv

TYPE(vg6d_v7d_var_conv), PARAMETER :: vg6d_v7d_var_conv_miss= &
 vg6d_v7d_var_conv(volgrid6d_var_miss, vol7d_var_miss, conv_func_miss)

TYPE(vg6d_v7d_var_conv), ALLOCATABLE :: conv_fwd(:), conv_bwd(:)

INTERFACE varbufr2vargrib
  MODULE PROCEDURE varbufr2vargrib_s, varbufr2vargrib_v
END INTERFACE

INTERFACE vargrib2varbufr
  MODULE PROCEDURE vargrib2varbufr_s, vargrib2varbufr_v
END INTERFACE

!> \brief Costructor
!!
!! create a new instance of object
INTERFACE init
  MODULE PROCEDURE init_volgrid6d
END INTERFACE

!> \brief destructor
!!
!! delete object 
INTERFACE delete
  MODULE PROCEDURE delete_volgrid6d,delete_volgrid6dv
END INTERFACE

!> Importazione.
INTERFACE import
  MODULE PROCEDURE volgrid6d_read_from_file
#ifdef HAVE_LIBGRIBAPI
  MODULE PROCEDURE import_from_gridinfo, import_from_gridinfovv, &
   volgrid6d_import_from_grib
#endif
END INTERFACE


!> Exportazione
INTERFACE export
  MODULE PROCEDURE volgrid6d_write_on_file
#ifdef HAVE_LIBGRIBAPI
  MODULE PROCEDURE export_to_gridinfo, export_to_gridinfov, export_to_gridinfovv,&
   volgrid6d_export_to_grib
#endif
END INTERFACE

!> \brief Calcola i nuovi dati secondo la trasformazione specificata
!!
!! Deve essere fornito l'oggetto di trasformazione
INTERFACE compute
  MODULE PROCEDURE volgrid6d_transform_compute,volgrid6d_v7d_transform_compute,&
   v7d_volgrid6d_transform_compute
END INTERFACE

!> \brief Trasforma i dati secondo gli oggetti forniti
!!
!! L'oggetto trasformazione viene creato e distrutto automaticamete
INTERFACE transform
  MODULE PROCEDURE volgrid6d_transform,volgrid6dv_transform,&
   volgrid6d_v7d_transform, volgrid6dv_v7d_transform, v7d_volgrid6d_transform
END INTERFACE

INTERFACE wind_rot
  MODULE PROCEDURE vg6d_wind_rot
END INTERFACE

INTERFACE wind_unrot
  MODULE PROCEDURE vg6d_wind_unrot
END INTERFACE

!> \brief  Display object on screen
!!
!! show brief content on screen
INTERFACE display
  MODULE PROCEDURE display_volgrid6d,display_volgrid6dv
END INTERFACE

private

PUBLIC volgrid6d,init,delete,export,import,compute,transform, &
 wind_rot,wind_unrot,vg6d_c2a,display

integer stallo

contains


!> \brief Costructor
!!
!! create a new istanze of object
subroutine init_volgrid6d (this,griddim,time_definition,categoryappend)
type(volgrid6d) :: this !< object to create
type(griddim_def),optional :: griddim !< descrittore del grigliato
integer,INTENT(IN),OPTIONAL :: time_definition !< 0=time is reference time ; 1=time is validity time
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
this%category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"init")
#endif

call init(this%griddim)

if (present(griddim))then
  call copy (griddim,this%griddim)
end if

 ! call init(this%time)         
 ! call init(this%timerange)    
 ! call init(this%level)        
 ! call init(this%var)          

if(present(time_definition)) then
  this%time_definition=time_definition
else
    this%time_definition=0  !default to reference time
end if

nullify (this%time,this%timerange,this%level,this%var)
nullify (this%gaid,this%voldati)          

end subroutine init_volgrid6d


!> \brief alloca i descrittori dell'oggeto volgrid6d
!!
!! Questo metodo alloca i vettori dei descrittori dell'oggetto volgrid6d:
!! - descrittore del grigliato
!! - descrittore della dimensione tempo
!! - descrittore della dimensione livello verticale
!! - descrittore della dimensione intervallo temporale (timerange)
!! - vettore descrittore della dimensione variabile
SUBROUTINE volgrid6d_alloc(this, dim, ntime, nlevel, ntimerange, nvar, ini)

TYPE(volgrid6d),INTENT(inout) :: this !< oggetto di cui allocare i descrittori
type(grid_dim),INTENT(in),OPTIONAL :: dim !< dimensioni delle dimensioni X,Y orizzontali
INTEGER,INTENT(in),OPTIONAL :: ntime !< dimensione della dimensione tempo
INTEGER,INTENT(in),OPTIONAL :: nlevel !< dimensione della dimensione livello varticale
INTEGER,INTENT(in),OPTIONAL :: ntimerange !< dimensione della dimensione intervallo temporale (timerange)
INTEGER,INTENT(in),OPTIONAL :: nvar !< dimensione della dimensione variabile
LOGICAL,INTENT(in),OPTIONAL :: ini !< se fornito e vale \c .TRUE., viene chiamato il costruttore, senza parametri opzionali, per ogni elemento di tutti i descrittori allocati, inizializzandolo quindi a valore mancante

INTEGER :: i
LOGICAL :: linit

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"alloc")
#endif

IF (PRESENT(ini)) THEN
  linit = ini
ELSE
  linit = .FALSE.
ENDIF


if (present(dim)) call copy (dim,this%griddim%dim)


IF (PRESENT(ntime)) THEN
  IF (ntime >= 0) THEN
    IF (ASSOCIATED(this%time)) DEALLOCATE(this%time)
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"alloc ntime "//to_char(ntime))
#endif
    ALLOCATE(this%time(ntime),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(this%category,L4F_ERROR,"allocating memory")
      CALL raise_fatal_error("allocating memory")
    end if
    IF (linit) THEN
      DO i = 1, ntime
        this%time(i) = datetime_miss
!        CALL init(this%time(i)) ! senza argomento inizializza a zero non missing
                                 ! baco di datetime?
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nlevel)) THEN
  IF (nlevel >= 0) THEN
    IF (ASSOCIATED(this%level)) DEALLOCATE(this%level)
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"alloc nlevel "//to_char(nlevel))
#endif
    ALLOCATE(this%level(nlevel),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(this%category,L4F_ERROR,"allocating memory")
      CALL raise_fatal_error("allocating memory")
    end if
    IF (linit) THEN
      DO i = 1, nlevel
        CALL init(this%level(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(ntimerange)) THEN
  IF (ntimerange >= 0) THEN
    IF (ASSOCIATED(this%timerange)) DEALLOCATE(this%timerange)
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"alloc ntimerange "//to_char(ntimerange))
#endif
    ALLOCATE(this%timerange(ntimerange),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(this%category,L4F_ERROR,"allocating memory")
      CALL raise_fatal_error("allocating memory")
    end if
    IF (linit) THEN
      DO i = 1, ntimerange
        CALL init(this%timerange(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvar)) THEN
  IF (nvar >= 0) THEN
    IF (ASSOCIATED(this%var)) DEALLOCATE(this%var)
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"alloc nvar "//to_char(nvar))
#endif
    ALLOCATE(this%var(nvar),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(this%category,L4F_ERROR,"allocating memory")
      CALL raise_fatal_error("allocating memory")
    end if
    IF (linit) THEN
      DO i = 1, nvar
        CALL init(this%var(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF

end SUBROUTINE volgrid6d_alloc


!> \brief alloca le matrici dati dell'oggeto volgrid6d
!!
!! Questo metodo alloca le matrici dati dell'oggetto volgrid6d oltre alla matrice gaid di oggetti grib id della grib_api
SUBROUTINE volgrid6d_alloc_vol(this, ini, inivol,decode)
TYPE(volgrid6d),INTENT(inout) :: this !< oggetto di cui allocare i volumi
LOGICAL,INTENT(in),OPTIONAL :: ini !< se fornito e vale \c .TRUE., viene chiamato il costruttore per i descrittori non ancora allocati con dimensione pari a 1 e ini=.TRUE.
LOGICAL,INTENT(in),OPTIONAL :: inivol !< se fornito e vale \c .FALSE., i volumi allocati non saranno inizializzati a valore mancante
LOGICAL,INTENT(in),OPTIONAL :: decode !< se fornito e vale \c .FALSE., i volumi dati non saranno allocati (this%gaid si comunque)


LOGICAL :: linivol,ldecode

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start alloc_vol")
#endif
IF (PRESENT(inivol)) THEN
  linivol = inivol
ELSE
  linivol = .TRUE.
ENDIF

IF (PRESENT(decode)) THEN
  ldecode = decode
ELSE
  ldecode = .TRUE.
ENDIF


IF (this%griddim%dim%nx > 0 .and. this%griddim%dim%ny > 0 .and. &
 .NOT.ASSOCIATED(this%voldati)) THEN
! Alloco i descrittori minimi per avere un volume di dati
  IF (.NOT. ASSOCIATED(this%var)) CALL volgrid6d_alloc(this, nvar=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%time)) CALL volgrid6d_alloc(this, ntime=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%level)) CALL volgrid6d_alloc(this, nlevel=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%timerange)) CALL volgrid6d_alloc(this, ntimerange=1, ini=ini)
  
  if (ldecode) then

#ifdef DEBUG
     call l4f_category_log(this%category,L4F_DEBUG,"alloc voldati volume")
#endif

     ALLOCATE(this%voldati( this%griddim%dim%nx,this%griddim%dim%ny,&
      SIZE(this%level), SIZE(this%time), &
      SIZE(this%timerange), SIZE(this%var)),stat=stallo)
     if (stallo /=0)then
       call l4f_category_log(this%category,L4F_ERROR,"allocating memory")
       CALL raise_fatal_error("allocating memory")
     end if
  
     IF (linivol) this%voldati = rmiss

  end if

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,"alloc gaid volume")
#endif
  ALLOCATE(this%gaid( SIZE(this%level),&
   SIZE(this%time), &
   SIZE(this%timerange), SIZE(this%var)),stat=stallo)
  if (stallo /=0)then
    call l4f_category_log(this%category,L4F_ERROR,"allocating memory")
    CALL raise_fatal_error("allocating memory")
  end if

  IF (linivol) this%gaid  = imiss
  
end if


END SUBROUTINE volgrid6d_alloc_vol


!> \brief destructor
!!
!! delete volgrid6d object
!! relase memory and delete category for logging
subroutine delete_volgrid6d(this)
type(volgrid6d) :: this

integer ::i,ii,iii,iiii

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"delete")
#endif

if (associated(this%gaid))then

  do i=1 ,size(this%gaid,1)
    do ii=1 ,size(this%gaid,2)
      do iii=1 ,size(this%gaid,3)
        do iiii=1 ,size(this%gaid,4)
          if (c_e(this%gaid(i,ii,iii,iiii))) call grib_release(this%gaid(i,ii,iii,iiii))
        end do
      end do
    end do
  end do
  
  deallocate(this%gaid)

end if

                                !chiudo il logger

call delete(this%griddim)

!  call delete(this%time)
!  call delete(this%timerange)
!  call delete(this%level)
!  call delete(this%var)

if (associated( this%time )) deallocate(this%time)
if (associated( this%timerange )) deallocate(this%timerange)
if (associated( this%level )) deallocate(this%level)
if (associated( this%var )) deallocate(this%var)

if (associated(this%voldati))deallocate(this%voldati)


                                !chiudo il logger
call l4f_category_delete(this%category)
  
end subroutine delete_volgrid6d


!>\brief Scrittura su file di un volume Volgrid6d.
!! Scrittura su file unformatted di un intero volume Volgrid6d.
!! Il volume viene serializzato e scritto su file.
!! Il file puo' essere aperto opzionalmente dall'utente. Si possono passare
!! opzionalmente unità e nome del file altrimenti assegnati internamente a dei default; se assegnati internamente 
!! tali parametri saranno in output.
!! Se non viene fornito il nome file viene utilizzato un file di default con nome pari al nome del programma in 
!! esecuzione con postfisso ".vg6d".
!! Come parametro opzionale c'è la description che insieme alla data corrente viene inserita nell'header del file.
subroutine volgrid6d_write_on_file (this,unit,description,filename,filename_auto)

TYPE(volgrid6d),INTENT(IN) :: this !< volume volgrid6d da scrivere 
integer,optional,intent(inout) :: unit !< unità su cui scrivere; se passata =0 ritorna il valore rielaborato (default =rielaborato internamente con getlun )
character(len=*),intent(in),optional :: filename !< nome del file su cui scrivere
character(len=*),intent(out),optional :: filename_auto !< nome del file generato se "filename" è omesso
character(len=*),INTENT(IN),optional :: description !< descrizione del volume

integer :: lunit
character(len=254) :: ldescription,arg,lfilename
integer :: ntime, ntimerange, nlevel, nvar
integer :: tarray(8)
logical :: opened,exist

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"write on file")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0

!call idate(im,id,iy)
call date_and_time(values=tarray)
call getarg(0,arg)

if (present(description))then
  ldescription=description
else
  ldescription="Volgrid6d generated by: "//trim(arg)
end if

if (.not. present(unit))then
  lunit=getunit()
else
  if (unit==0)then
    lunit=getunit()
    unit=lunit
  else
    lunit=unit
  end if
end if

lfilename=trim(arg)//".vg6d"
if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if

if (present(filename_auto))filename_auto=lfilename


inquire(unit=lunit,opened=opened)
if (.not. opened) then 
  inquire(file=lfilename,EXIST=exist)
  if (exist) CALL raise_error('file exist; cannot open new file')
  if (.not.exist) open (unit=lunit,file=lfilename,form="UNFORMATTED")
  !print *, "opened: ",lfilename
end if

if (associated(this%time)) ntime=size(this%time)
if (associated(this%timerange)) ntimerange=size(this%timerange)
if (associated(this%level)) nlevel=size(this%level)
if (associated(this%var)) nvar=size(this%var)


write(unit=lunit)ldescription
write(unit=lunit)tarray

call write_unit( this%griddim,lunit)
write(unit=lunit) ntime, ntimerange, nlevel, nvar

!! prime 4 dimensioni
if (associated(this%time))      call write_unit(this%time, lunit)
if (associated(this%level))     write(unit=lunit)this%level
if (associated(this%timerange)) write(unit=lunit)this%timerange
if (associated(this%var))       write(unit=lunit)this%var    


!! Volumi di valori dati

if (associated(this%voldati))     write(unit=lunit)this%voldati

if (.not. present(unit)) close(unit=lunit)

end subroutine volgrid6d_write_on_file


!>\brief Lettura da file di un volume Volgrid6d.
!! Lettura da file unformatted di un intero volume Volgrid6d.
!! Questa subroutine comprende volgrid6d_alloc e volgrid6d_alloc_vol.
!! Il file puo' essere aperto opzionalmente dall'utente. Si possono passare
!! opzionalmente unità e nome del file altrimenti assegnati internamente a dei default; se assegnati internamente 
!! tali parametri saranno in output.
subroutine volgrid6d_read_from_file (this,unit,filename,description,tarray,filename_auto)

TYPE(volgrid6d),INTENT(OUT) :: this !< Volume volgrid6d da leggere
integer,intent(inout),optional :: unit !< unità su cui è stato aperto un file; se =0 rielaborato internamente (default = elaborato internamente con getunit)
character(len=*),INTENT(in),optional :: filename !< nome del file eventualmente da aprire (default = (nome dell'eseguibile)//.v7d )
character(len=*),intent(out),optional :: filename_auto !< nome del file generato se "filename" è omesso
character(len=*),INTENT(out),optional :: description !< descrizione del volume letto
integer,intent(out),optional :: tarray(8) !< vettore come definito da "date_and_time" della data di scrittura del volume

integer :: ntime, ntimerange, nlevel, nvar

character(len=254) :: ldescription,lfilename,arg
integer :: ltarray(8),lunit
logical :: opened,exist

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"read from file")
#endif

call getarg(0,arg)

if (.not. present(unit))then
  lunit=getunit()
else
  if (unit==0)then
    lunit=getunit()
    unit=lunit
  else
    lunit=unit
  end if
end if

lfilename=trim(arg)//".vg6d"
if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if

if (present(filename_auto))filename_auto=lfilename


inquire(unit=lunit,opened=opened)
if (.not. opened) then 
  inquire(file=lfilename,EXIST=exist)
  IF (.NOT. exist) CALL raise_fatal_error('file '//TRIM(lfilename)//' does not exist, cannot open')
  open (unit=lunit,file=lfilename,form="UNFORMATTED")
end if

read(unit=lunit)ldescription
read(unit=lunit)ltarray

print *,"Info: reading volgrid6d from file: "//trim(lfilename)
print *,"Info: description: ",trim(ldescription)
print *,"Info: written on ",ltarray

if (present(description))description=ldescription
if (present(tarray))tarray=ltarray


call read_unit( this%griddim,lunit)
read(unit=lunit) ntime, ntimerange, nlevel, nvar


call volgrid6d_alloc (this, &
 ntime=ntime, ntimerange=ntimerange, nlevel=nlevel, nvar=nvar)

call volgrid6d_alloc_vol (this)

if (associated(this%time))      call read_unit(this%time, lunit)
if (associated(this%level))     read(unit=lunit)this%level
if (associated(this%timerange)) read(unit=lunit)this%timerange
if (associated(this%var))       read(unit=lunit)this%var    


!! Volumi di valori 

if (associated(this%voldati))     read(unit=lunit)this%voldati

if (.not. present(unit)) close(unit=lunit)

end subroutine volgrid6d_read_from_file

#ifdef HAVE_LIBGRIBAPI
!> \brief import from gridinfo object to volgrid6d
!!
!! Un oggetto gridinfo che al suo interno contiene un id di un grib delle grib_api e una sua sufficiente descrizione
!! viene importato nella struttura volgrid6d organizzandolo nella sua forma multidimensionale.
!! I descrittori di volgrid6d devono essere stati opportunamente inizializzati per poter permettere una corretta importazione.
SUBROUTINE import_from_gridinfo (this,gridinfo,force,clone,categoryappend)

TYPE(volgrid6d),INTENT(OUT) :: this !< Volume volgrid6d da leggere
type(gridinfo_def),intent(in) :: gridinfo !< gridinfo 
LOGICAL,INTENT(in),OPTIONAL :: force !< se fornito e \c .TRUE., incastra a forza il gridinfo in un elemento libero di \a this (se c'è)
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid da gridinfo
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

character(len=255)   :: type
integer :: ilevel,itime,itimerange,ivar
logical :: lforce

if (present(force)) then
  lforce = force
else
  lforce = .false.
endif

call get_val(this%griddim,type=type)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"import_from_gridinfo: "//trim(type))
#endif

if (.not. c_e(type))then
  call copy(gridinfo%griddim, this%griddim)
! ho gia` fatto init, altrimenti non potrei fare la get_val(this%griddim)
! per cui meglio non ripetere
!   call init(this,gridinfo%griddim,categoryappend)
  CALL volgrid6d_alloc_vol(this, ini=.TRUE.) ! decode?

else if (.not. (this%griddim == gridinfo%griddim ))then

   call l4f_category_log(this%category,L4F_ERROR,"volgrid6d: grid or dim are different and this is not possible")
   call raise_fatal_error ("volgrid6d: grid or dim are different and this is not possible")

end if

! Cerco gli indici del campo da inserire, li invento se necessario
ilevel = index(this%level, gridinfo%level)
IF (ilevel == 0 .AND. lforce) THEN
  ilevel = index(this%level, vol7d_level_miss)
  IF (ilevel /= 0) this%level(ilevel) = gridinfo%level
ENDIF
IF (ilevel == 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "volgrid6d: level not valid for volume")
  CALL raise_fatal_error("volgrid6d: level not valid for volume")
ENDIF

itime = index(this%time, gridinfo%time)
IF (itime == 0 .AND. lforce) THEN
  itime = index(this%time, datetime_miss)
  IF (itime /= 0) this%time(itime) = gridinfo%time
ENDIF
IF (itime == 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "volgrid6d: time not valid for volume")
  CALL raise_fatal_error("volgrid6d: time not valid for volume")
ENDIF

itimerange = index(this%timerange,gridinfo%timerange)
IF (itimerange == 0 .AND. lforce) THEN
  itimerange = index(this%timerange, vol7d_timerange_miss)
  IF (itimerange /= 0) this%timerange(itimerange) = gridinfo%timerange
ENDIF
IF (itimerange == 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "volgrid6d: timerange not valid for volume")
  CALL raise_fatal_error("volgrid6d: timerange not valid for volume")
ENDIF

ivar = index(this%var, gridinfo%var)
IF (ivar == 0 .AND. lforce) THEN
  ivar = index(this%var, volgrid6d_var_miss)
  IF (ivar /= 0) this%var(ivar) = gridinfo%var
ENDIF
IF (ivar == 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "volgrid6d: var not valid for volume")
  CALL raise_fatal_error("volgrid6d: var not valid for volume")
ENDIF

if (associated (this%gaid))then

  if ( c_e (this%gaid(ilevel,itime,itimerange,ivar)))then

   call l4f_category_log(this%category,L4F_WARN,"gaid exist: grib duplicated")
!   call raise_error ("volgrid6d: gaid exist: grib duplicated")

  end if

  if (optio_log(clone))then
    this%gaid(ilevel,itime,itimerange,ivar)=-1
    call grib_clone(gridinfo%gaid,this%gaid(ilevel,itime,itimerange,ivar))
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"clone gaid from: "//TRIM(to_char(gridinfo%gaid))//&
     " to : "//TRIM(to_char(this%gaid(ilevel,itime,itimerange,ivar))))
#endif
  else
    this%gaid(ilevel,itime,itimerange,ivar)=gridinfo%gaid
  end if
  
else

  call l4f_category_log(this%category,L4F_ERROR,&
   "gaid non allocato: chiama volgrid6d_alloc_vol")
  call raise_error("gaid non allocato: chiama volgrid6d_alloc_vol")
  
end if


if (associated (this%voldati))then

  this%voldati(:,:,ilevel,itime,itimerange,ivar)&
   = decode_gridinfo (gridinfo)
  
else
  
  call l4f_category_log(this%category,L4F_INFO,"non decodifico i dati")

end if

end subroutine import_from_gridinfo


!> \brief export  from volgrid6d object to gridinfo
!!
!! Dalla struttura volgrid6d organizzata nella sua forma multidimensionale viene esportato, specificando l'elemento richiesto,
!! a un  oggetto gridinfo che al suo interno contiene un id di un grib delle grib_api e/o una sua sufficiente descrizione.
subroutine export_to_gridinfo (this,gridinfo,itime,itimerange,ilevel,ivar,gaid_template,clone)

TYPE(volgrid6d),INTENT(in) :: this !< Volume volgrid6d da leggere
type(gridinfo_def),intent(out) :: gridinfo !< gridinfo 
integer ::itime,itimerange,ilevel,ivar,gaid
integer, optional :: gaid_template
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid to gridinfo

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"export_to_gridinfo")
#endif

if (present(gaid_template)) then
  gaid=-1
  call grib_clone(gaid_template,gaid)
#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,&
   "clone to a new gaid from:"//trim(to_char(gaid_template))//" to: "//trim(to_char(gaid)))
#endif
else

  gaid=gridinfo%gaid

end if


if (.not. c_e(gaid))then

  if (c_e(this%gaid(ilevel,itime,itimerange,ivar)))then

    if (optio_log(clone))then
      gaid=-1
      call grib_clone(this%gaid(ilevel,itime,itimerange,ivar),gaid)
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,&
       "clone to a new gaid from:"//trim(to_char(this%gaid(ilevel,itime,itimerange,ivar)))//&
       " to: "//trim(to_char(gaid)))
#endif
    else
      gaid = this%gaid(ilevel,itime,itimerange,ivar)
    end if
  else
 
    gaid=imiss
    call l4f_category_log(this%category,L4F_INFO,&
     "mancano tutti i gaid; export impossible, no not warry, sometime will be normaly ")
    !call raise_error("mancano tutti i gaid; export impossibile")

  end if
end if


call init(gridinfo,gaid,&
 this%griddim,&
 this%time(itime),&
 this%timerange(itimerange),&
 this%level(ilevel),&
 this%var(ivar))

!questo non è bellissimo: devo rendere coerenti dati e loro descrizione
call export(gridinfo%griddim,gridinfo%gaid)

call encode_gridinfo(gridinfo,this%voldati(:,:,&
 ilevel,itime,itimerange,ivar))


end subroutine export_to_gridinfo



!> \brief import from a vector of gridinfo object to volgrid6d
!! 
!! Come import_from_gridinfo ma con un vettore di gridinfo.
subroutine import_from_gridinfovv (this,gridinfov,clone,categoryappend)

TYPE(volgrid6d),pointer :: this(:) !< Vettore Volume volgrid6d da leggere
type(gridinfo_def),intent(in) :: gridinfov(:) !< vettore gridinfo 
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid to gridinfo
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< accoda questo suffisso al namespace category di log4fortran

integer :: i,j
integer :: ngrid,ntime,ntimerange,nlevel,nvar
integer :: category
character(len=512) :: a_name

! category temporanea (altrimenti non possiamo loggare)
if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(category,L4F_DEBUG,"start import_from_gridinfovv")
#endif

!type(gridinfo_def),allocatable :: gridinfovtmp(:)
!allocate(gridinfovtmp(size(gridinfov)))
!gridinfovtmp=(this%griddim(i) == gridinfov%griddim(j))
!deallocate(gridinfovtmp)

ngrid=count_distinct(gridinfov%griddim,back=.true.)
call l4f_category_log(category,L4F_INFO,&
     "numero delle aree differenti: "//to_char(ngrid))

allocate (this(ngrid),stat=stallo)
if (stallo /=0)then
  call l4f_category_log(category,L4F_ERROR,"allocating memory")
  CALL raise_fatal_error("allocating memory")
end if
do i=1,ngrid
   if (present(categoryappend))then
      call init (this(i), categoryappend=trim(categoryappend)//"-"//to_char(ngrid))
   else
      call init (this(i), categoryappend=to_char(ngrid))
   end if
end do

this(:)%griddim=pack_distinct(gridinfov%griddim,ngrid,back=.true.)


do i=1,ngrid
   ntime = count_distinct(gridinfov%time,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   ntimerange = count_distinct(gridinfov%timerange,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   nlevel = count_distinct(gridinfov%level,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   nvar = count_distinct(gridinfov%var,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   

!   CALL l4f_category_log(this(i)%category,L4F_DEBUG,'volgrid6d_alloc '//TRIM(to_char(nlevel))//' '//TRIM(to_char(nvar)))
!   call init (this(i),this(i)%griddim, categoryappend)

#ifdef DEBUG
   call l4f_category_log(this(i)%category,L4F_DEBUG,"alloc volgrid6d index: "//trim(to_char(i)))
#endif
   
   call volgrid6d_alloc(this(i),this(i)%griddim%dim,ntime=ntime,ntimerange=ntimerange,nlevel=nlevel,nvar=nvar)
   
   this(i)%time=pack_distinct(gridinfov%time,ntime,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   this(i)%timerange=pack_distinct(gridinfov%timerange,ntimerange,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   this(i)%level=pack_distinct(gridinfov%level,nlevel,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   this(i)%var=pack_distinct(gridinfov%var,nvar,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)

#ifdef DEBUG
   call l4f_category_log(this(i)%category,L4F_DEBUG,"alloc_vol volgrid6d index: "//trim(to_char(i)))
#endif
   call volgrid6d_alloc_vol(this(i)) 

end do


do i=1,size(gridinfov)

!   call display(gridinfov(i)%griddim)
!   call display(this(index(this%griddim,gridinfov(i)%griddim))%griddim)

!  print *,"lavoro sull'area: ",index(this%griddim,gridinfov(i)%griddim)

#ifdef DEBUG
  call l4f_category_log(category,L4F_DEBUG,"import from gridinfov index: "//trim(to_char(i)))
#endif
  call l4f_category_log(category,L4F_INFO,&
   "to volgrid6d index: "//to_char(index(this%griddim,gridinfov(i)%griddim)))

  CALL import (this(index(this%griddim,gridinfov(i)%griddim)), &
   gridinfov(i),clone=clone,categoryappend=categoryappend)

end do

                                !chiudo il logger temporaneo
call l4f_category_delete(category)

end subroutine import_from_gridinfovv


!> \brief export  from volgrid6d object to a vector of gridinfo
!!
!! Dalla struttura volgrid6d organizzata nella sua forma multidimensionale viene esportato
!! a un  vettore di oggetti gridinfo l'intero contenuto.
subroutine export_to_gridinfov (this,gridinfov,gaid_template,clone)

TYPE(volgrid6d),INTENT(in) :: this !< Volume volgrid6d da exportare
type(gridinfo_def),intent(out) :: gridinfov(:) !< vettore gridinfo 
integer, optional :: gaid_template !< eventuale template sul quale sovrascrivere i descrittori dimenticandosi del'eventuale grib originale 
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid to gridinfo invece di copiare il puntatore alla struttura

integer :: i,itime,itimerange,ilevel,ivar
integer :: ngridinfo,ntime,ntimerange,nlevel,nvar

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start export_to_gridinfov")
#endif

ngridinfo=size(gridinfov)

if (ngridinfo /= size(this%gaid))then
   call l4f_category_log(this%category,L4F_ERROR,&
        "dimension mismach"//to_char(ngridinfo)//to_char(size(this%gaid)))
   call raise_error("dimension mismach")
end if

i=0
ntime=size(this%time)
ntimerange=size(this%timerange)
nlevel=size(this%level)
nvar=size(this%var)

do itime=1,ntime
  do itimerange=1,ntimerange
    do ilevel=1,nlevel
      do ivar=1,nvar
        
        i=i+1
        if (i > ngridinfo) &
         call raise_error ("errore stranuccio in export_to_gridinfo:"//&
         "avevo già testato le dimensioni che ora sono sbagliate")
        call export (this,gridinfov(i),itime,itimerange,ilevel,ivar,gaid_template,clone=clone)
        
      end do
    end do
  end do
end do

end subroutine export_to_gridinfov


!> \brief export  from a vector of volgrid6d object to a vector of gridinfo
!!
!! Dalla struttura volgrid6d organizzata nella sua forma multidimensionale viene esportato
!! a un  vettore di oggetti gridinfo l'intero contenuto.
!! l'imput a vettore permette la gestione di qualsiasi mix di dati
subroutine export_to_gridinfovv (this,gridinfov,gaid_template,clone,categoryappend)

TYPE(volgrid6d),INTENT(in)  :: this(:)      !< vettore volume volgrid6d da exportare
type(gridinfo_def),pointer :: gridinfov(:) !< vettore gridinfo in cui exportare
integer, optional :: gaid_template
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid to gridinfo invece di copiare semplicemente i puntatori alla struttura
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

integer :: i,igrid,ngrid,start,end,ngridinfo,ngridinfoin
integer :: category
character(len=512) :: a_name


! category temporanea (altrimenti non possiamo loggare)
if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
category=l4f_category_get(a_name)

ngrid=size(this)

ngridinfo=0
if (.not. associated(gridinfov))then
  do igrid=1,ngrid

    ngridinfo=ngridinfo+size(this(igrid)%gaid)

  end do
  
  allocate (gridinfov(ngridinfo),stat=stallo)
  if (stallo /=0)then
    call l4f_category_log(category,L4F_ERROR,"allocating memory")
    CALL raise_fatal_error("allocating memory")
  end if
  do i=1,ngridinfo
     if (present(categoryappend))then
        call init(gridinfov(i),categoryappend=trim(categoryappend)//"-"//to_char(i))
     else
        call init(gridinfov(i),categoryappend=to_char(i))
     end if
  enddo
else

   ngridinfoin=size(gridinfov)

   if (ngridinfo /= ngridinfoin)then
      call l4f_category_log(category,L4F_ERROR,&
           "dimension mismach"//to_char(ngridinfo)//to_char(ngridinfoin))
      call raise_error("dimension mismach")
   end if
   
end if


end=0
do igrid=1,ngrid

  start=end+1
  end=end+size(this(igrid)%gaid)

#ifdef DEBUG
  call l4f_category_log(this(igrid)%category,L4F_DEBUG,"export to gridinfo grid index: "//&
   trim(to_char(igrid))//" from "//trim(to_char(start))//" to "//trim(to_char(end)))
#endif
  call export (this(igrid),gridinfov(start:end),gaid_template,clone=clone)

end do

                                !chiudo il logger
call l4f_category_delete(category)
  

end subroutine export_to_gridinfovv

!> \brief importa da un file grib
!!
!! Questo è un metodo di alto livello. Da un file grib comunque
!! ordinato e qualsiasi contenuto vengono importati descrittori e dati
!! organizzandoli nella struttura ordinata di un vettore di oggetti
!! volgrid6d. In realtà il contenuto dei grib deve essere gestito dal
!! software di importazione limitando ad esempio i grigliati ammessi
!! ai tipi previsti dalla classe grid. Ogni oggetto necessario viene
!! inizializzato e allocato automaticamente.
subroutine volgrid6d_import_from_grib (this,unit,filename,categoryappend)

TYPE(volgrid6d),pointer :: this(:) !< Volume volgrid6d da leggere
!> unità su cui è stato aperto un file; se =0 rielaborato internamente (default = elaborato internamente con getunit) 
!! e restituito in output; se non fornito o =0 il file viene aperto automaticamente
integer,intent(inout),optional :: unit 
!> nome del file eventualmente da aprire (default = (nome dell'eseguibile)//.v7d ); se = "" viene restituito in output
character(len=*),INTENT(in),optional :: filename
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

type(gridinfo_def),allocatable :: gridinfo(:)
integer::ngrib,gaid,iret,category,lunit,ier
character(len=254) :: arg,lfilename

character(len=512) :: a_name

if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append= &
    trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(category,L4F_DEBUG,"import from grib")
#endif

call getarg(0,arg)

lfilename=trim(arg)//".grb"
if (index(arg,'/',back=.true.) > 0) lfilename=&
 lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if


if (.not. present(unit))then
  call grib_open_file(lunit,lfilename,'r')
else
  if (unit==0)then
    call grib_open_file(lunit,lfilename,'r')
    unit=lunit
  else
    lunit=unit
  end if
end if

call l4f_category_log(category,L4F_INFO,"reading volgrid6d from grib: "//trim(lfilename))

ngrib=0

call grib_count_in_file(lunit,ngrib)

call l4f_category_log(category,L4F_INFO,&
         "Numero totale di grib: "//to_char(ngrib))

if (ngrib > 0 ) then

  allocate (gridinfo(ngrib),stat=stallo)
  if (stallo /=0)then
    call l4f_category_log(category,L4F_ERROR,"allocating memory")
    CALL raise_fatal_error("allocating memory")
  end if

  ngrib=0
  
                                ! Loop on all the messages in a file.
  
                                !     a new grib message is loaded from file
                                !     gaid is the grib id to be used in subsequent calls
  
  gaid=-1
  call  grib_new_from_file(lunit,gaid,iret) 


  LOOP: DO WHILE (iret == GRIB_SUCCESS)

    call l4f_category_log(category,L4F_INFO,"import gridinfo")
    
    ngrib=ngrib+1
    call init (gridinfo(ngrib),gaid=gaid,categoryappend=trim(categoryappend)//to_char(ngrib))
    call import(gridinfo(ngrib))
    
    gaid=-1
    call grib_new_from_file(lunit,gaid, iret)
    
  end do LOOP
  
  call l4f_category_log(category,L4F_INFO,"import")
  
                                !TODO attenzione qui si potrebbe non clonare e poi non deletare

  call import (this,gridinfo,clone=.true.,categoryappend=categoryappend)
    
  call l4f_category_log(category,L4F_INFO,"delete gridinfo")
  
  do ngrib=1,size(gridinfo)
    call delete (gridinfo(ngrib))
  enddo
  
  deallocate(gridinfo)

else

  call l4f_category_log(category,L4F_INFO,"file do not contains grib: return")

end if

call l4f_category_log(category,L4F_INFO,"last operations for a clean enviroment")

if (.not. present(unit)) call grib_close_file(lunit)

!chiudo il logger
call l4f_category_delete(category)

end subroutine volgrid6d_import_from_grib



!> \brief exporta a un file grib
!!
!! Questo è un metodo di alto livello.  Da un vettore di oggetti
!! volgrid6d, descrittori e dati vengono exportati a un file grib. Un
!! eventuale template determinerà molte delle informazione contenute
!! nel grib; altrimenti verranno utilizzati i gaid interni a volgrid6d
!! per mantenere le informazioni del grib le piu' coerenti
!! possibili. Ogni oggetto necessario viene inizializzato e allocato
!! automaticamente.
subroutine volgrid6d_export_to_grib (this,unit,filename,gaid_template,categoryappend)

TYPE(volgrid6d),pointer :: this(:) !< Volumi volgrid6d da exportare
!> unità su cui è stato aperto un file; se =0 rielaborato internamente (default = elaborato internamente con getunit)
!! e restituito in output; se non fornito o =0 il file viene aperto automaticamente
integer,intent(inout),optional :: unit
!> nome del file eventualmente da aprire (default = (nome dell'eseguibile)//.v7d );
!! se = "" viene restituito in output
character(len=*),INTENT(in),optional :: filename 
integer,INTENT(in),OPTIONAL :: gaid_template !< grib id template; se fornito ignora l'eventuale gaid di volgrid6d utilizzando il template al suo posto
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

type(gridinfo_def),pointer :: gridinfo(:)
integer::ngrib,gaid,category,lunit,ier
character(len=254) :: arg,lfilename

character(len=512) :: a_name

if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif

category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(category,L4F_DEBUG,"start export to grib")
#endif

call getarg(0,arg)


lfilename=trim(arg)//".grb"
if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if


if (.not. present(unit))then
  call grib_open_file(lunit,lfilename,'w')
else
  if (unit==0)then
    call grib_open_file(lunit,lfilename,'w')
    unit=lunit
  else
    lunit=unit
  end if
end if

call l4f_category_log(category,L4F_INFO,"writing volgrid6d to grib file: "//trim(lfilename))

if ( associated(this))then

  call export (this,gridinfo,gaid_template=gaid_template,clone=.true.)

  do ngrib=1,size(gridinfo)
                                !     write the new message to a file
    if(c_e(gridinfo(ngrib)%gaid)) then
      call export (gridinfo(ngrib))
      call grib_write(gridinfo(ngrib)%gaid,lunit)
      call delete (gridinfo(ngrib))
    end if
  end do

else

  call l4f_category_log(category,L4F_INFO,"volume volgrid6d is not associated: return")
  return
  
end if


call l4f_category_log(category,L4F_INFO,"last operations for a clean enviroment")

if (.not. present(unit))call grib_close_file(lunit)

deallocate(gridinfo)

!chiudo il logger
call l4f_category_delete(category)


end subroutine volgrid6d_export_to_grib
#endif


!> \brief destructor
!!
!! delete vector of volgrid6d object
!! relase memory and delete category for logging
subroutine delete_volgrid6dv(this)
TYPE(volgrid6d),POINTER :: this(:) !< vector of volgrid6d object

integer :: i

IF (ASSOCIATED(this)) THEN
  DO i=1,SIZE(this)

#ifdef DEBUG
    CALL l4f_category_log(this(i)%category,L4F_DEBUG, &
     "delete volgrid6d vector index: "//TRIM(to_char(i)))
#endif
    CALL delete(this(i))

  ENDDO
  DEALLOCATE(this)
ENDIF

end subroutine delete_volgrid6dv


!> \brief Calcola i nuovi dati secondo la trasformazione specificata
!!
!! Deve essere fornito l'oggetto di trasformazione e oggetti completi
SUBROUTINE volgrid6d_transform_compute(this, volgrid6d_in, volgrid6d_out,clone)
TYPE(grid_transform),INTENT(in) :: this !< oggetto di trasformazione per il grigliato
type(volgrid6d), INTENT(in) :: volgrid6d_in !< oggetto da trasformare
type(volgrid6d), INTENT(out) :: volgrid6d_out !< oggetto trasformato; deve essere completo (init, alloc, alloc_vol)
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid da volgrid6d_in a volgrid6d_out

integer :: ntime, ntimerange, nlevel, nvar
integer :: itime, itimerange, ilevel, ivar


#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_transform_compute")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (associated(volgrid6d_in%time))then
  ntime=size(volgrid6d_in%time)
  volgrid6d_out%time=volgrid6d_in%time
end if

if (associated(volgrid6d_in%timerange))then
  ntimerange=size(volgrid6d_in%timerange)
  volgrid6d_out%timerange=volgrid6d_in%timerange
end if

if (associated(volgrid6d_in%level))then
  nlevel=size(volgrid6d_in%level)
  volgrid6d_out%level=volgrid6d_in%level
end if

if (associated(volgrid6d_in%var))then
  nvar=size(volgrid6d_in%var)
  volgrid6d_out%var=volgrid6d_in%var
end if

do itime=1,ntime
  do itimerange=1,ntimerange
    do ilevel=1,nlevel
      do ivar=1,nvar
        
        call compute(this, &
         volgrid6d_in%voldati(:,:,ilevel,itime,itimerange,ivar),&
         volgrid6d_out%voldati(:,:,ilevel,itime,itimerange,ivar))

                                ! if present gaid copy it
        if (c_e(volgrid6d_in%gaid(ilevel,itime,itimerange,ivar)) .and. .not. &
            c_e(volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))) then

          if (optio_log(clone))then

            volgrid6d_out%gaid(ilevel,itime,itimerange,ivar)=-1
            call grib_clone(volgrid6d_in%gaid(ilevel,itime,itimerange,ivar),&
             volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))

#ifdef DEBUG
            call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"clone gaid from: "//&
             trim(to_char(volgrid6d_in%gaid(ilevel,itime,itimerange,ivar)))//&
             " to: "//trim(to_char(volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))))
#endif


          else
            volgrid6d_out%gaid(ilevel,itime,itimerange,ivar)=volgrid6d_in%gaid(ilevel,itime,itimerange,ivar)
          end if

        end if

      end do
    end do
  end do
end do

end SUBROUTINE volgrid6d_transform_compute


!> \brief Trasforma i dati secondo gli oggetti forniti
!!
!! L'oggetto trasformazione su grigliato viene creato e distrutto automaticamete
!! L'oggetto trasformato viene creato automaticamente
subroutine volgrid6d_transform(this,griddim, volgrid6d_in, volgrid6d_out,clone,categoryappend)
type(transform_def),intent(in) :: this !< oggetto che specifica la trasformazione
type(griddim_def),intent(in),optional :: griddim !< griddim che specifica la trasformazione
! TODO ripristinare intent(in) dopo le opportune modifiche in grid_class.F90
type(volgrid6d), INTENT(inout) :: volgrid6d_in !< oggetto da trasformare
type(volgrid6d), INTENT(out) :: volgrid6d_out !< oggetto trasformato
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid da volgrid6d_in a volgrid6d_out
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

type(grid_transform) :: grid_trans
integer :: ntime, ntimerange, nlevel, nvar

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_transform")
#endif

call init (volgrid6d_out,griddim,categoryappend=categoryappend)

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (associated(volgrid6d_in%time)) ntime=size(volgrid6d_in%time)
if (associated(volgrid6d_in%timerange)) ntimerange=size(volgrid6d_in%timerange)
if (associated(volgrid6d_in%level)) nlevel=size(volgrid6d_in%level)
if (associated(volgrid6d_in%var)) nvar=size(volgrid6d_in%var)


! Ensure wind components are referred to geographical system
call vg6d_wind_unrot(volgrid6d_in)

call init(grid_trans, this, in=volgrid6d_in%griddim, out=volgrid6d_out%griddim, &
 categoryappend=categoryappend)

call volgrid6d_alloc(volgrid6d_out, griddim%dim, ntime, nlevel, ntimerange, nvar)
call volgrid6d_alloc_vol(volgrid6d_out)

!ensure unproj was called
!call griddim_unproj(volgrid6d_out%griddim)

call compute(grid_trans, volgrid6d_in, volgrid6d_out,clone)
call delete (grid_trans)

end subroutine volgrid6d_transform


!> \brief Trasforma i dati secondo gli oggetti forniti (vettori)
!!
!! L'oggetto trasformazione su grigliato viene creato e distrutto automaticamete
!! L'oggetto trasformato viene creato automaticamente
subroutine volgrid6dv_transform(this,griddim, volgrid6d_in, volgrid6d_out,clone,categoryappend)
type(transform_def),intent(in) :: this !< oggetto che specifica la trasformazione
type(griddim_def),intent(in),optional :: griddim !< griddim che specifica la trasformazione
! TODO ripristinare intent(in) dopo le opportune modifiche in grid_class.F90
type(volgrid6d), INTENT(inout) :: volgrid6d_in(:) !< vettore oggetti da trasformare
type(volgrid6d), pointer :: volgrid6d_out(:) !< vettore oggetti trasformati
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid da volgrid6d_in a volgrid6d_out
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

integer :: i,n
n=size(volgrid6d_in)

allocate( volgrid6d_out(n),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if


do i=1,n

  call transform (this,griddim, volgrid6d_in(i), volgrid6d_out(i),clone,categoryappend=categoryappend)

end do

end subroutine volgrid6dv_transform



!> \brief Calcola i nuovi dati secondo la trasformazione specificata
!!
!! Deve essere fornito l'oggetto di trasformazione e oggetti completi
SUBROUTINE volgrid6d_v7d_transform_compute(this, volgrid6d_in, vol7d_out,networkname)
TYPE(grid_transform),INTENT(in) :: this !< oggetto di trasformazione per grigliato
type(volgrid6d), INTENT(in) :: volgrid6d_in !< oggetto da trasformare
type(vol7d), INTENT(out) :: vol7d_out !< oggetto trasformato
character(len=network_name_len),optional,intent(in) :: networkname !< imposta il network in vol7d_out (default='generic')

integer :: nntime, nana, ntime, ntimerange, nlevel, nvar
integer :: itime, itimerange, ilevel, ivar, inetwork
real,allocatable :: voldatir_out(:,:)
TYPE(conv_func), pointer :: c_func(:)
type(datetime),allocatable ::validitytime(:,:)

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_v7d_transform_compute")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (present(networkname))then
  call init(vol7d_out%network(1),name=networkname)
else
  call init(vol7d_out%network(1),name='generic')
end if

if (associated(volgrid6d_in%timerange))then
  ntimerange=size(volgrid6d_in%timerange)
  vol7d_out%timerange=volgrid6d_in%timerange
end if

if (associated(volgrid6d_in%time))then
  ntime=size(volgrid6d_in%time)

  if (vol7d_out%time_definition == volgrid6d_in%time_definition) then

                                ! i time sono definiti uguali: assegno
    vol7d_out%time=volgrid6d_in%time

  else
                                ! converto reference in validity
    allocate (validitytime(ntime,ntimerange),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(volgrid6d_in%category,L4F_ERROR,"allocating memory")
      call raise_fatal_error("allocating memory")
    end if

    do itime=1,ntime
      do itimerange=1,ntimerange
        if (vol7d_out%time_definition > volgrid6d_in%time_definition) then
          validitytime(itime,itimerange) = &
           volgrid6d_in%time(itime) + timedelta_new(msec=volgrid6d_in%timerange(itimerange)%p1*1000)
        else
          validitytime(itime,itimerange) = &
           volgrid6d_in%time(itime) - timedelta_new(msec=volgrid6d_in%timerange(itimerange)%p1*1000)
        end if
      end do
    end do

    nntime = count_distinct(reshape(validitytime,(/ntime*ntimerange/)), back=.TRUE.)
    vol7d_out%time=pack_distinct(reshape(validitytime,(/ntime*ntimerange/)), nntime,back=.TRUE.)
  
  end if
end if

if (associated(volgrid6d_in%level))then
  nlevel=size(volgrid6d_in%level)
  vol7d_out%level=volgrid6d_in%level
end if

if (associated(volgrid6d_in%var))then
  nvar=size(volgrid6d_in%var)
  CALL vargrib2varbufr(volgrid6d_in%var, vol7d_out%dativar%r, c_func)
end if

nana=size(vol7d_out%ana)

allocate(voldatir_out(nana,1),stat=stallo)
if (stallo /=0)then
  call l4f_category_log(volgrid6d_in%category,L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if

inetwork=1
do itime=1,ntime
  do itimerange=1,ntimerange
    do ilevel=1,nlevel
      do ivar=1,nvar
        
                                !non è chiaro se questa sezione è utile o no
                                !ossia il compute sotto sembra prevedere voldatir_out solo in out
!!$        if (vol7d_out%time_definition == volgrid6d_in%time_definition) then
!!$          voldatir_out=reshape(vol7d_out%voldatir(:,itime,ilevel,itimerange,ivar,inetwork),(/nana,1/))
!!$        else
!!$          voldatir_out=reshape(vol7d_out%voldatir(:,index(vol7d_out%time,validitytime(itime,ilevel)),ilevel,itimerange,ivar,inetwork),(/nana,1/))
!!$        end if

        call compute(this, &
         volgrid6d_in%voldati(:,:,ilevel,itime,itimerange,ivar),&
         voldatir_out)

        if (vol7d_out%time_definition == volgrid6d_in%time_definition) then
          vol7d_out%voldatir(:,itime,ilevel,itimerange,ivar,inetwork)=reshape(voldatir_out,(/nana/))
        else
          vol7d_out%voldatir(:,index(vol7d_out%time,validitytime(itime,itimerange)),ilevel,itimerange,ivar,inetwork)=&
           reshape(voldatir_out,(/nana/))
        end if

! 1 indice della dimensione "anagrafica"
! 2 indice della dimensione "tempo"
! 3 indice della dimensione "livello verticale"
! 4 indice della dimensione "intervallo temporale"
! 5 indice della dimensione "variabile"
! 6 indice della dimensione "rete"

      end do
    end do
  end do
end do

deallocate(voldatir_out)
if (vol7d_out%time_definition /= volgrid6d_in%time_definition) deallocate(validitytime)

! Rescale valid data according to variable conversion table
IF (ASSOCIATED(c_func)) THEN
  DO ivar = 1, nvar
    if (c_func(ivar)%a /= conv_func_miss%a .or. c_func(ivar)%b /= conv_func_miss%b )then
      WHERE(vol7d_out%voldatir(:,:,:,:,ivar,:) /= rmiss)
        vol7d_out%voldatir(:,:,:,:,ivar,:) = &
         vol7d_out%voldatir(:,:,:,:,ivar,:)*c_func(ivar)%a + c_func(ivar)%b
      END WHERE
    else
      vol7d_out%voldatir(:,:,:,:,ivar,:)=rmiss
    end if
  ENDDO
  DEALLOCATE(c_func)
ENDIF

end SUBROUTINE volgrid6d_v7d_transform_compute


!> \brief Trasforma i dati secondo gli oggetti forniti
!!
!! L'oggetto trasformazione su grigliato viene creato e distrutto automaticamete
!! L'oggetto trasformato viene creato automaticamente
SUBROUTINE volgrid6d_v7d_transform(this, volgrid6d_in, vol7d_out, v7d, poly, &
 networkname, categoryappend)
type(transform_def),intent(in) :: this !< oggetto che specifica la trasformazione
type(volgrid6d), INTENT(inout) :: volgrid6d_in !< oggetto da trasformare
type(vol7d), INTENT(out) :: vol7d_out !< oggetto trasformato
type(vol7d), INTENT(in), OPTIONAL :: v7d !<  anagrafiche su cui effettuare la trasformazione
TYPE(geo_coordvect),INTENT(inout),OPTIONAL :: poly(:) !< array of polygons indicating areas over which to transform
character(len=network_name_len),optional,intent(in) :: networkname !< imposta il network in vol7d_out (default='generic')
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

type(grid_transform) :: grid_trans
integer :: ntime, ntimerange, nlevel, nvar, nana,time_definition,nnetwork
integer :: itime,itimerange,nx,ny
type(datetime),allocatable ::validitytime(:,:)
type(vol7d) :: v7d_locana

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_v7d_transform")
#endif

call vg6d_wind_unrot(volgrid6d_in)

ntime=0
ntimerange=0
nlevel=0
nvar=0
nnetwork=1

call get_val(this,time_definition=time_definition)
if (.not. c_e(time_definition)) then
  time_definition=1  ! default to validity time
endif

if (present(v7d)) then
  v7d_locana = v7d
else
  call init(v7d_locana,time_definition=time_definition)
endif

if (associated(volgrid6d_in%timerange)) ntimerange=size(volgrid6d_in%timerange)

if (associated(volgrid6d_in%time)) then

  ntime=size(volgrid6d_in%time)
  
  if (time_definition /= volgrid6d_in%time_definition) then
    
                                ! converto reference in validity
    allocate (validitytime(ntime,ntimerange),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(volgrid6d_in%category,L4F_ERROR,"allocating memory")
      call raise_fatal_error("allocating memory")
    end if

    do itime=1,ntime
      do itimerange=1,ntimerange
        if (time_definition > volgrid6d_in%time_definition) then
          validitytime(itime,itimerange) = &
           volgrid6d_in%time(itime) + timedelta_new(msec=volgrid6d_in%timerange(itimerange)%p1*1000)
        else
          validitytime(itime,itimerange) = &
           volgrid6d_in%time(itime) - timedelta_new(msec=volgrid6d_in%timerange(itimerange)%p1*1000)
        end if
      end do
    end do

    ntime = count_distinct(reshape(validitytime,(/ntime*ntimerange/)), back=.TRUE.)
    deallocate (validitytime)
  
  end if
end if


if (associated(volgrid6d_in%level)) nlevel=size(volgrid6d_in%level)
if (associated(volgrid6d_in%var)) nvar=size(volgrid6d_in%var)

CALL init(grid_trans, this, volgrid6d_in%griddim, v7d_locana, poly=poly, &
 categoryappend=categoryappend)

!TODO aggiungere categoryappend
call init (vol7d_out,time_definition=time_definition)

nana=size(v7d_locana%ana)
call vol7d_alloc(vol7d_out, nana=nana, ntime=ntime, nlevel=nlevel, ntimerange=ntimerange, ndativarr=nvar, nnetwork=nnetwork)

vol7d_out%ana = v7d_locana%ana

call vol7d_alloc_vol(vol7d_out)

!ensure unproj was called
!call griddim_unproj(volgrid6d_out%griddim)

call compute(grid_trans, volgrid6d_in, vol7d_out, networkname)

call delete (grid_trans)

if (.not. present(v7d)) then
  call delete(v7d_locana)
endif

end subroutine volgrid6d_v7d_transform


!> \brief Trasforma i dati secondo gli oggetti forniti (vettori)
!!
!! L'oggetto trasformazione su grigliato viene creato e distrutto automaticamete
!! L'oggetto trasformato viene creato automaticamente
SUBROUTINE volgrid6dv_v7d_transform(this, volgrid6d_in, vol7d_out, v7d, poly, &
 networkname,categoryappend)
type(transform_def),intent(in) :: this !< oggetto che specifica la trasformazione
type(volgrid6d), INTENT(inout) :: volgrid6d_in(:) !< vettore di oggetti da trasformare
type(vol7d), pointer :: vol7d_out(:) !< vettore di oggetti trasformati
type(vol7d),intent(in),optional :: v7d !<  anagrafiche su cui effettuare la trasformazione
TYPE(geo_coordvect),INTENT(inout),OPTIONAL :: poly(:) !< array of polygons indicating areas over which to transform
character(len=network_name_len),optional,intent(in) :: networkname !< imposta il network in vol7d_out (default='generic')
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

integer :: i,n

n=size(volgrid6d_in)

allocate(vol7d_out(n),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if

do i=1,n
  CALL transform(this, volgrid6d_in(i), vol7d_out(i), v7d=v7d, poly=poly, &
   networkname=networkname, categoryappend=categoryappend)
end do

end subroutine volgrid6dv_v7d_transform


!> \brief Calcola i nuovi dati secondo la trasformazione specificata
!!
!! Deve essere fornito l'oggetto di trasformazione e oggetti completi
SUBROUTINE v7d_volgrid6d_transform_compute(this, vol7d_in, volgrid6d_out, networkname)
TYPE(grid_transform),INTENT(in) :: this !< oggetto di trasformazione per grigliato
type(vol7d), INTENT(in) :: vol7d_in !< oggetto da trasformare
type(volgrid6d), INTENT(out) :: volgrid6d_out !< oggetto trasformato 
character(len=network_name_len),optional,intent(in) :: networkname !< seleziona il network da exportare da vol7d (default=1)

integer :: nana, ntime, ntimerange, nlevel, nvar, nnetwork
integer :: itime, itimerange, ilevel, ivar, inetwork
real,allocatable :: voldatir_out(:,:)
type(vol7d_network) :: network
TYPE(conv_func), pointer :: c_func(:)
!TODO category sarebbe da prendere da vol7d
#ifdef DEBUG
call l4f_category_log(volgrid6d_out%category,L4F_DEBUG,"start v7d_volgrid6d_transform_compute")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (present(networkname))then
  call init(network,name=networkname)
  inetwork= index(vol7d_in%network,network)
else
  inetwork=1
end if

if (associated(vol7d_in%time))then
  ntime=size(vol7d_in%time)
                                !TODO tramutare in copy
  volgrid6d_out%time=vol7d_in%time

!!! #### convertire reference in validity

end if

if (associated(vol7d_in%timerange))then
  ntimerange=size(vol7d_in%timerange)
                                !TODO tramutare in copy
  volgrid6d_out%timerange=vol7d_in%timerange
end if

if (associated(vol7d_in%level))then
  nlevel=size(vol7d_in%level)
                                !TODO tramutare in copy
  volgrid6d_out%level=vol7d_in%level
end if

if (associated(vol7d_in%dativar%r))then
  nvar=size(vol7d_in%dativar%r)
  CALL varbufr2vargrib(vol7d_in%dativar%r, volgrid6d_out%var, c_func)
end if

nana=SIZE(vol7d_in%voldatir, 1)

do itime=1,ntime
  do itimerange=1,ntimerange
    do ilevel=1,nlevel
      do ivar=1,nvar

        call compute(this, &
         vol7d_in%voldatir(:,itime,ilevel,itimerange,ivar,inetwork),&
         volgrid6d_out%voldati(:,:,ilevel,itime,itimerange,ivar))
! in vol7d:
! 1 indice della dimensione "anagrafica"
! 2 indice della dimensione "tempo"
! 3 indice della dimensione "livello verticale"
! 4 indice della dimensione "intervallo temporale"
! 5 indice della dimensione "variabile"
! 6 indice della dimensione "rete"

      end do
    end do
  end do
end do

! Rescale valid data according to variable conversion table
IF (ASSOCIATED(c_func)) THEN
  DO ivar = 1, nvar
    if ( c_func(ivar)%a /= conv_func_miss%a .or. c_func(ivar)%b /= conv_func_miss%b )then
      WHERE(volgrid6d_out%voldati(:,:,:,:,:,ivar) /= rmiss )
        volgrid6d_out%voldati(:,:,:,:,:,ivar) = &
         volgrid6d_out%voldati(:,:,:,:,:,ivar)*c_func(ivar)%a + c_func(ivar)%b
      END WHERE
    else
      volgrid6d_out%voldati(:,:,:,:,:,ivar) = rmiss
    end if
  ENDDO
  DEALLOCATE(c_func)
ENDIF

end SUBROUTINE v7d_volgrid6d_transform_compute


!> \brief Trasforma i dati secondo gli oggetti forniti
!!
!! L'oggetto trasformazione su grigliato viene creato e distrutto automaticamete
!! L'oggetto trasformato viene creato automaticamente
subroutine v7d_volgrid6d_transform(this,griddim, vol7d_in, volgrid6d_out, networkname,categoryappend)
type(transform_def),intent(in) :: this !< oggetto che specifica la trasformazione
type(griddim_def),intent(in) :: griddim  !< griddim che specifica la trasformazione
type(vol7d), INTENT(in) :: vol7d_in !< oggetto da trasformare
type(volgrid6d), INTENT(out) :: volgrid6d_out !< oggetto trasformato
character(len=network_name_len),optional,intent(in) :: networkname  !< seleziona il network da exportare da vol7d (default=generic)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

type(grid_transform) :: grid_trans
integer :: ntime, ntimerange, nlevel, nvar

!TODO la category sarebbe da prendere da vol7d
!call l4f_category_log(vol7d_out%category,L4F_DEBUG,"start volgrid6d_transform")

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (associated(vol7d_in%time)) ntime=size(vol7d_in%time)
if (associated(vol7d_in%timerange)) ntimerange=size(vol7d_in%timerange)
if (associated(vol7d_in%level)) nlevel=size(vol7d_in%level)
if (associated(vol7d_in%dativar%r)) nvar=size(vol7d_in%dativar%r)

call init(grid_trans, this, vol7d_in, griddim, categoryappend=categoryappend)

call init (volgrid6d_out, griddim, categoryappend=categoryappend)

call volgrid6d_alloc(volgrid6d_out, griddim%dim, ntime=ntime, nlevel=nlevel, ntimerange=ntimerange, nvar=nvar)

call volgrid6d_alloc_vol(volgrid6d_out)

call compute(grid_trans, vol7d_in, volgrid6d_out, networkname)

call vg6d_wind_rot(volgrid6d_out)

call delete (grid_trans)

end subroutine v7d_volgrid6d_transform


SUBROUTINE vargrib2varbufr_v(vargrib, varbufr, c_func)
TYPE(volgrid6d_var),INTENT(in) :: vargrib(:)
TYPE(vol7d_var),INTENT(out) :: varbufr(:)
TYPE(conv_func), POINTER,optional :: c_func(:)

INTEGER :: i, n

n = SIZE(vargrib)
if ( present(c_func)) ALLOCATE(c_func(n),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if

DO i = 1, n
  if (present(c_func))then
    CALL vargrib2varbufr_s(vargrib(i), varbufr(i), c_func(i))
  else
    CALL vargrib2varbufr_s(vargrib(i), varbufr(i))
  end if
ENDDO

END SUBROUTINE vargrib2varbufr_v


SUBROUTINE vargrib2varbufr_s(vargrib, varbufr, c_func)
TYPE(volgrid6d_var),INTENT(in) :: vargrib
TYPE(vol7d_var),INTENT(out) :: varbufr
TYPE(conv_func),optional :: c_func

INTEGER :: i

IF (.NOT. ALLOCATED(conv_fwd)) CALL vg6d_v7d_var_conv_setup()

!call init(varbufr, btable="B12001")

DO i = 1, SIZE(conv_fwd)
  IF (vargrib == conv_fwd(i)%vg6d_var) THEN
    varbufr = conv_fwd(i)%v7d_var
    if(present(c_func)) c_func = conv_fwd(i)%c_func
    RETURN
  ENDIF
ENDDO
! not found
varbufr = vol7d_var_miss
if(present(c_func)) c_func = conv_func_miss

CALL l4f_log(L4F_WARN, 'vargrib2varbufr: variable '// &
 TRIM(to_char(vargrib%centre))//':'//TRIM(to_char(vargrib%category))//':'// &
 TRIM(to_char(vargrib%number))//':'//TRIM(to_char(vargrib%discipline))// &
 ' not found in table')

END SUBROUTINE vargrib2varbufr_s


SUBROUTINE varbufr2vargrib_v(varbufr, vargrib, c_func)
type(vol7d_var),intent(in) :: varbufr(:)
type(volgrid6d_var),intent(out) :: vargrib(:)
TYPE(conv_func), pointer :: c_func(:)

INTEGER :: i, n

n = SIZE(varbufr)
ALLOCATE(c_func(n),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if

DO i = 1, n
  CALL varbufr2vargrib_s(varbufr(i), vargrib(i), c_func(i))
ENDDO

END SUBROUTINE varbufr2vargrib_v


SUBROUTINE varbufr2vargrib_s(varbufr, vargrib, c_func)
TYPE(vol7d_var),INTENT(in) :: varbufr
TYPE(volgrid6d_var),INTENT(out) :: vargrib
TYPE(conv_func) :: c_func

INTEGER :: i

IF (.NOT. ALLOCATED(conv_bwd)) CALL vg6d_v7d_var_conv_setup()

DO i = 1, SIZE(conv_bwd)
  IF (varbufr == conv_bwd(i)%v7d_var) THEN
    vargrib = conv_bwd(i)%vg6d_var
    c_func = conv_bwd(i)%c_func
    RETURN
  ENDIF
ENDDO
! not found
vargrib = volgrid6d_var_miss
c_func = conv_func_miss
!call init(vargrib, centre=200, category=1, number=11)

CALL l4f_log(L4F_WARN, 'varbufr2vargrib: variable '// &
 varbufr%btable//" : "//trim(varbufr%description)//" : "//trim(varbufr%unit)// &
 ' not found in table')


END SUBROUTINE varbufr2vargrib_s


! Private subroutine for reading forward and backward conversion tables
! todo: better error handling
SUBROUTINE vg6d_v7d_var_conv_setup()
INTEGER :: un, i, n

! forward, grib to bufr
un = open_package_file('vargrib2bufr.csv', filetype_data)
n=0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  n = n + 1
ENDDO

100 CONTINUE

REWIND(un)
ALLOCATE(conv_fwd(n),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if

conv_fwd(:) = vg6d_v7d_var_conv_miss
CALL import_var_conv(un, conv_fwd)
CLOSE(un)

! backward, bufr to grib
un = open_package_file('vargrib2bufr.csv', filetype_data)
! use the same file for now
!un = open_package_file('varbufr2grib.csv', filetype_data)
n=0
DO WHILE(.TRUE.)
  READ(un,*,END=300)
  n = n + 1
ENDDO

300 CONTINUE

REWIND(un)
ALLOCATE(conv_bwd(n),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if

conv_bwd(:) = vg6d_v7d_var_conv_miss
CALL import_var_conv(un, conv_bwd)
DO i = 1, n
  conv_bwd(i)%c_func%a = 1./conv_bwd(i)%c_func%a
  conv_bwd(i)%c_func%b = - conv_bwd(i)%c_func%b
ENDDO
CLOSE(un)

CONTAINS

SUBROUTINE import_var_conv(un, conv_type)
INTEGER, INTENT(in) :: un
TYPE(vg6d_v7d_var_conv), INTENT(out) :: conv_type(:)

INTEGER :: i
TYPE(csv_record) :: csv
CHARACTER(len=1024) :: line
CHARACTER(len=10) :: btable
INTEGER :: centre, category, number, discipline

DO i = 1, SIZE(conv_type)
  READ(un,'(A)',END=200)line
  CALL init(csv, line)
  CALL csv_record_getfield(csv, btable)
  CALL csv_record_getfield(csv) ! skip fields for description and unit,
  CALL csv_record_getfield(csv) ! they correspond to grib information, not bufr Btable
  CALL init(conv_type(i)%v7d_var, btable=btable)

  CALL csv_record_getfield(csv, centre)
  CALL csv_record_getfield(csv, category)
  CALL csv_record_getfield(csv, number)
  CALL csv_record_getfield(csv, discipline)
  CALL init(conv_type(i)%vg6d_var, centre=centre, category=category, &
   number=number, discipline=discipline) ! controllare l'ordine

  CALL csv_record_getfield(csv, conv_type(i)%c_func%a)
  CALL csv_record_getfield(csv, conv_type(i)%c_func%b)
  CALL delete(csv)
ENDDO

200 CONTINUE

END SUBROUTINE import_var_conv

END SUBROUTINE vg6d_v7d_var_conv_setup



!> Unrotate the wind components.
!! It converts u and v components of vector quantities relative to the
!! defined grid in the direction of increasing x and y coordinates to
!! u and v components relative to easterly and notherly direction. The
!! original fields are overwritten.
!! \todo Check and correct wind component flag (to be moved in
!! griddim_def?)
subroutine vg6d_wind_unrot(this)
type(volgrid6d) :: this !< object containing wind to be unrotated

integer :: component_flag

call get_val(this%griddim,component_flag=component_flag)

if (component_flag == 1) then
  call l4f_category_log(this%category,L4F_INFO, &
   "unrotating vector components")
  call vg6d_wind__un_rot(this,.false.)
  call set_val(this%griddim,component_flag=0)
else
  call l4f_category_log(this%category,L4F_INFO, &
   "no need to unrotate vector components")
end if

end subroutine vg6d_wind_unrot


!> Rotate the wind components.
!! It converts u and v components of vector quantities 
!! relative to easterly and notherly direction to
!! defined grid in the direction of increasing x and y coordinates.
!! The original fields are overwritten.
subroutine vg6d_wind_rot(this)
type(volgrid6d) :: this !< object containing wind to be rotated

integer :: component_flag

call get_val(this%griddim,component_flag=component_flag)

if (component_flag == 0) then
  call l4f_category_log(this%category,L4F_INFO, &
   "rotating vector components")
  call vg6d_wind__un_rot(this,.true.)
  call set_val(this%griddim,component_flag=1)
else
  call l4f_category_log(this%category,L4F_INFO, &
   "no need to rotate vector components")
end if

end subroutine vg6d_wind_rot



!> Generic UnRotate the wind components.
subroutine vg6d_wind__un_rot(this,rot)

type(volgrid6d) :: this !< object containing wind to be (un)rotated
logical :: rot !< if .true. rotate else unrotate

INTEGER :: iu,iv,nvar,nvaru,nvarv,i,j,k,a11,a12,a21,a22
type(vol7d_var) :: varu,varv
type(vol7d_var),allocatable ::varbufr(:)
double precision,pointer :: rot_mat(:,:,:)
double precision,allocatable :: tmp_arr(:,:)


IF (.NOT. ALLOCATED(conv_fwd)) CALL vg6d_v7d_var_conv_setup()

call init(varu,btable="B11003")
call init(varv,btable="B11004")

! commentata questa inutile display perche' non compila su debian
! There is no specific subroutine for the generic 'display', capire il motivo
!call display(varu)

! test about presence of u and v in standard table

iu=index(conv_fwd(:)%v7d_var,varu)
iv=index(conv_fwd(:)%v7d_var,varv)

if (iu == 0  .or. iv == 0 )then
  call l4f_category_log(this%category,L4F_ERROR,"B11003 or B11004 not defined by vg6d_v7d_var_conv_setup")
  call raise_fatal_error ("volgrid6d: B11003 or B11004 not defined by vg6d_v7d_var_conv_setup")
end if


nvar=0
if (associated(this%var))then
  nvar=size(this%var)
  allocate(varbufr(nvar),stat=stallo)
  if (stallo /=0)then
    call l4f_log(L4F_ERROR,"allocating memory")
    call raise_fatal_error("allocating memory")
  end if

  CALL vargrib2varbufr(this%var, varbufr)
end if

nvaru=COUNT(varbufr==varu)
nvarv=COUNT(varbufr==varv)

if (nvaru > 1 )then
  call l4f_category_log(this%category,L4F_ERROR,"2 variables refer to u wind component")
  call raise_fatal_error ("volgrid6d:2 variables refer to u wind component")
endif

if (nvarv > 1 )then
  call l4f_category_log(this%category,L4F_ERROR,"2 variables refer to v wind component")
  call raise_fatal_error ("volgrid6d:2 variables refer to v wind component")
endif


if (nvaru == 1 .and. nvarv == 0)then
  call l4f_category_log(this%category,L4F_ERROR,"only u wind component present: unrotation impossible")
  call raise_fatal_error ("volgrid6d: only u wind component present, unrotation impossible")
endif

if (nvaru == 0 .and. nvarv == 1)then
  call l4f_category_log(this%category,L4F_ERROR,"only v wind component present: unrotation impossible")
  call raise_fatal_error ("volgrid6d: only v wind component present, unrotation impossible")
endif

!nothing todo
if (nvaru == 0 .and. nvarv == 0) return

!find index of u and v components
iu=index(varbufr,varu)
iv=index(varbufr,varv)


if ( conv_fwd(iu)%c_func%a /= conv_fwd(iv)%c_func%a .or. &
 conv_fwd(iu)%c_func%b /= conv_fwd(iv)%c_func%b )then
  
  call l4f_category_log(this%category,L4F_WARN,"u and v wind component seam aliens; conversion factor different")
  call l4f_category_log(this%category,L4F_WARN,"convert units of u and v wind component")

! Rescale valid data according to variable conversion table
  WHERE(this%voldati(:,:,:,:,:,iu) /= rmiss)
    this%voldati(:,:,:,:,:,iu) = &
     this%voldati(:,:,:,:,:,iu)*conv_fwd(iu)%c_func%a + conv_fwd(iu)%c_func%b
  END WHERE
  
  WHERE(this%voldati(:,:,:,:,:,iv) /= rmiss)
    this%voldati(:,:,:,:,:,iv) = &
     this%voldati(:,:,:,:,:,iv)*conv_fwd(iv)%c_func%a + conv_fwd(iv)%c_func%b
  END WHERE
  
end if 


! Temporary workspace
ALLOCATE(tmp_arr(this%griddim%dim%nx, this%griddim%dim%ny),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if

CALL griddim_unproj(this%griddim)
CALL wind_unrot(this%griddim, rot_mat)


a11=1

if (rot)then
  a12=2
  a21=3
else
  a12=3
  a21=2
end if

a22=4


DO k = 1, SIZE(this%timerange)
  DO j = 1, SIZE(this%time)
    DO i = 1, SIZE(this%level)

! Multiply wind components by rotation matrix
      WHERE(this%voldati(:,:,i,j,k,iu) /= rmiss .AND. &
       this%voldati(:,:,i,j,k,iv) /= rmiss)

        tmp_arr(:,:) = this%voldati(:,:,i,j,k,iu)*rot_mat(:,:,a11) + &
         this%voldati(:,:,i,j,k,iv)*rot_mat(:,:,a12)

        this%voldati(:,:,i,j,k,iv) = &
         this%voldati(:,:,i,j,k,iu)*rot_mat(:,:,a21) + &
         this%voldati(:,:,i,j,k,iv)*rot_mat(:,:,a22)

        this%voldati(:,:,i,j,k,iu) = tmp_arr(:,:)
      END WHERE
    ENDDO
  ENDDO
ENDDO

DEALLOCATE (rot_mat, tmp_arr)

if ( conv_fwd(iu)%c_func%a /= conv_fwd(iv)%c_func%a .or. &
 conv_fwd(iu)%c_func%b /= conv_fwd(iv)%c_func%b )then
  
  call l4f_category_log(this%category,L4F_WARN,"reconvert units of u and v wind component")

! Re-scale valid data according to variable conversion table
  WHERE(this%voldati(:,:,:,:,:,iu) /= rmiss)
    this%voldati(:,:,:,:,:,iu) = &
     ( this%voldati(:,:,:,:,:,iu) - conv_fwd(iu)%c_func%b ) / conv_fwd(iu)%c_func%a
  END WHERE
  
  WHERE(this%voldati(:,:,:,:,:,iv) /= rmiss)
    this%voldati(:,:,:,:,:,iv) = &
     ( this%voldati(:,:,:,:,:,iv) - conv_fwd(iv)%c_func%b ) / conv_fwd(iv)%c_func%a 
  END WHERE
  
end if 



end subroutine vg6d_wind__un_rot




!!$cerchiamo di capire la logica:
!!$
!!$casi:
!!$
!!$1) abbiamo un solo volume: deve essere fornita la direzione dello shift
!!$                           calcoliamo H e ce lo portiamo
!!$2) abbiamo due volumi:
!!$      1) volume U e volume V: calcoliamo quello H e ce li portiamo
!!$      2) volume U/V e volume H : riportiamo U/V su H
!!$3) abbiamo tre volumi: riportiamo U e V su H
!!$
!!$casi strani:
!!$1) non abbiamo U in volume U
!!$2) non abbiamo V in volume V
!!$3) abbiamo altra roba oltre a U e V in volumi U e V
!!$
!!$
!!$quindi i passi sono:
!!$1) trovare i volumi interessati
!!$2) definire o calcolare griglia H
!!$3) trasformare i volumi in H 

!! TODO per ora la griglia H(t) deve essere fornita


subroutine vg6d_c2a (this)

TYPE(volgrid6d),INTENT(inout)  :: this(:)      !< vettore volume volgrid6d da exportare

integer :: ngrid,igrid,jgrid,ugrid,vgrid,tgrid
doubleprecision :: lon_min, lon_max, lat_min, lat_max
doubleprecision :: lon_min_t, lon_max_t, lat_min_t, lat_max_t
doubleprecision :: step_lon_t,step_lat_t
character(len=80) :: type_t,type

ngrid=size(this)

do igrid=1,ngrid

  ugrid=imiss
  vgrid=imiss

  tgrid=igrid
  call get_val(this(igrid)%griddim,lon_min=lon_min_t, lon_max=lon_max_t, lat_min=lat_min_t, lat_max=lat_max_t,type=type_t)
  step_lon_t=(lon_max_t-lon_min_t)/dble(this(igrid)%griddim%dim%nx-1)
  step_lat_t=(lat_max_t-lat_min_t)/dble(this(igrid)%griddim%dim%ny-1)

  do jgrid=1,ngrid

#ifdef DEBUG
    call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: search U/V/T points:"//to_char(igrid)//to_char(jgrid))
    call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: test delta: "//to_char(step_lon_t)//to_char(step_lat_t))
#endif
    
    if (this(igrid)%griddim == this(jgrid)%griddim ) cycle

    if (this(igrid)%griddim%dim%nx == this(jgrid)%griddim%dim%nx .and. &
     this(igrid)%griddim%dim%ny == this(jgrid)%griddim%dim%ny ) then

      call get_val(this(jgrid)%griddim,lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max,type=type)
      
      if (type_t /= type )cycle

#ifdef DEBUG
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: test U "//&
       to_char(lon_min)//to_char(lon_max)//to_char(lat_min)//to_char(lat_max))

      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"diff coordinate lat"//&
       to_char(abs(lon_min - (lon_min_t+step_lon_t/2.d0)))//&
       to_char(abs(lon_max - (lon_max_t+step_lon_t/2.d0))))
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"diff coordinate lon"//&
       to_char(abs(lat_min - (lat_min_t+step_lat_t/2.d0)))//&
       to_char(abs(lat_max - (lat_max_t+step_lat_t/2.d0))))
#endif

      if ( abs(lon_min - (lon_min_t+step_lon_t/2.d0)) < 1.d-3 .and. abs(lon_max - (lon_max_t+step_lon_t/2.d0)) < 1.d-3 ) then
        if ( abs(lat_min - lat_min_t) < 1.d-3 .and. abs(lat_max - lat_max_t) < 1.d-3 ) then

#ifdef DEBUG
          call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: trovato U")
#endif
          ugrid=jgrid

        end if
      end if

#ifdef DEBUG
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: test V "//&
       to_char(lon_min)//to_char(lon_max)//to_char(lat_min)//to_char(lat_max))
#endif

      if ( abs(lat_min - (lat_min_t+step_lat_t/2.d0)) < 1.d-3 .and. abs(lat_max - (lat_max_t+step_lat_t/2.d0)) < 1.d-3 ) then
        if ( abs(lon_min - lon_min_t) < 1.d-3 .and. abs(lon_max - lon_max_t) < 1.d-3  ) then
          
#ifdef DEBUG
          call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: trovato V")
#endif
          vgrid=jgrid

        end if
      end if
    end if

  end do

  ! abbiamo almeno due volumi: riportiamo U e/o V su T
  if (c_e(ugrid)) then
#ifdef DEBUG
    call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid U points: riportiamo U su T "//to_char(tgrid)//to_char(ugrid))
#endif
    call vg6d_c2a_grid(this(ugrid),this(tgrid),cgrid=1)
    call vg6d_c2a_mat(this(ugrid),cgrid=1)
  end if

  if (c_e(vgrid)) then
#ifdef DEBUG
    call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid V points: riportiamo V su T "//to_char(tgrid)//to_char(vgrid))
#endif
    call vg6d_c2a_grid(this(vgrid),this(tgrid),cgrid=2)
    call vg6d_c2a_mat(this(vgrid),cgrid=2)
  end if

end do
  

end subroutine vg6d_c2a


!> Convert C grid to A grid
subroutine vg6d_c2a_grid(this,vg6d_t,cgrid)

type(volgrid6d),intent(inout) :: this !< object containing fields to be translated (U or V points)
type(volgrid6d),intent(in),optional :: vg6d_t !< object containing T points
integer,intent(in) :: cgrid !< in C grid (Arakawa) we have 0=T,1=U,2=V points

doubleprecision :: lon_min, lon_max, lat_min, lat_max
doubleprecision :: step_lon,step_lat


if (present(vg6d_t)) then

 call get_val(vg6d_t%griddim,lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max)
 call set_val(this%griddim,lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max)

else

  select case (cgrid)

  case(0)

#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"C grid: T points, nothing to do")
#endif
    return

  case (1)

#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"C grid: U points, we need interpolation")
#endif

    call get_val(this%griddim, lon_min=lon_min, lon_max=lon_max)
    step_lon=(lon_max-lon_min)/dble(this%griddim%dim%nx-1)
    lon_min=lon_min-step_lon/2.d0
    lon_max=lon_max-step_lon/2.d0
    call set_val(this%griddim, lon_min=lon_min, lon_max=lon_max)
    
  case (2)

#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"C grid: V points, we need interpolation")
#endif

    call get_val(this%griddim, lat_min=lat_min, lat_max=lat_max)
    step_lat=(lat_max-lat_min)/dble(this%griddim%dim%ny-1)
    lat_min=lat_min-step_lat/2.d0
    lat_max=lat_max-step_lat/2.d0
    call set_val(this%griddim, lat_min=lat_min, lat_max=lat_max)
    
  case default

    call l4f_category_log(this%category,L4F_ERROR,"C grid type not known")
    call raise_fatal_error ("volgrid6d: C grid type not kmow")

  end select

end if


call griddim_unproj(this%griddim)


end subroutine vg6d_c2a_grid

!> Convert C grid to A grid
subroutine vg6d_c2a_mat(this,cgrid)

type(volgrid6d),intent(inout) :: this !< object containing fields to be translated
integer,intent(in) :: cgrid !< in C grid (Arakawa) we have 0=T,1=U,2=V points

INTEGER :: nvar,nvaru,nvarv,i,j,k,iv
type(vol7d_var) :: varu,varv
type(vol7d_var),allocatable ::varbufr(:)
double precision,allocatable :: tmp_arr(:,:)


IF (.NOT. ALLOCATED(conv_fwd)) CALL vg6d_v7d_var_conv_setup()

call init(varu,btable="B11003")
call init(varv,btable="B11004")

! test about presence of u and v in standard table

if ( index(conv_fwd(:)%v7d_var,varu) == 0  .or. index(conv_fwd(:)%v7d_var,varv) == 0 )then
  call l4f_category_log(this%category,L4F_ERROR,"B11003 and/or B11004 not defined by  vg6d_v7d_var_conv_setup")
  call raise_fatal_error ("volgrid6d: B11003 and/or B11004 not defined by  vg6d_v7d_var_conv_setup")
end if

nvar=0
if (associated(this%var))then
  nvar=size(this%var)
  allocate(varbufr(nvar),stat=stallo)
  if (stallo /=0)then
    call l4f_log(L4F_ERROR,"allocating memory")
    call raise_fatal_error("allocating memory")
  end if

  CALL vargrib2varbufr(this%var, varbufr)
end if

nvaru=COUNT(varbufr==varu)
nvarv=COUNT(varbufr==varv)

if (nvaru > 1 )then
  call l4f_category_log(this%category,L4F_WARN,"2 variables refer to u wind component")
  call raise_error ("volgrid6d:2 variables refer to u wind component")
endif

if (nvarv > 1 )then
  call l4f_category_log(this%category,L4F_WARN,"2 variables refer to v wind component")
  call raise_error ("volgrid6d:2 variables refer to v wind component")
endif

if (nvaru == 0 .and. nvarv == 0) then
  call l4f_category_log(this%category,L4F_WARN,"no u or v wind component found")
  call raise_error ("volgrid6d: no u or v wind component found")
endif

if ( COUNT(varbufr/=varu .and. varbufr/=varv) > 0 )then
  call l4f_category_log(this%category,L4F_WARN,"there are variables different from u and v wind component in C grid")
  call raise_error ("volgrid6d: there are variables different from u and v wind component in C grid")
endif

! Temporary workspace
ALLOCATE(tmp_arr(this%griddim%dim%nx, this%griddim%dim%ny),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_ERROR,"allocating memory")
  call raise_fatal_error("allocating memory")
end if

tmp_arr=rmiss

timerange: DO k = 1, SIZE(this%timerange)
  DO j = 1, SIZE(this%time)
    DO i = 1, SIZE(this%level)
      DO iv = 1, SIZE(this%var)
        
        tmp_arr=rmiss

        select case (cgrid)

        case(0)               ! T points; nothing to do

          exit timerange

        case(1)               ! U points to H points

                                ! West boundary
          WHERE(this%voldati(1,:,i,j,k,iv) /= rmiss .AND. &
           this%voldati(2,:,i,j,k,iv) /= rmiss)
            tmp_arr(1,:) = this%voldati(1,:,i,j,k,iv) - (this%voldati(2,:,i,j,k,iv) - this%voldati(1,:,i,j,k,iv)) / 2.
          end WHERE

                                ! Rest of the matrix
          WHERE(this%voldati(1:this%griddim%dim%nx-1,:,i,j,k,iv) /= rmiss .AND. &
           this%voldati(2:this%griddim%dim%nx,:,i,j,k,iv) /= rmiss)

            tmp_arr(2:this%griddim%dim%nx,:) = (this%voldati(1:this%griddim%dim%nx-1,:,i,j,k,iv) + &
             this%voldati(2:this%griddim%dim%nx,:,i,j,k,iv)) / 2.
            
          end WHERE

          this%voldati(:,:,i,j,k,iv)=tmp_arr
            
        CASE (2)              ! V points to H points

                                ! South boundary
          WHERE(this%voldati(:,1,i,j,k,iv) /= rmiss .AND. &
           this%voldati(:,2,i,j,k,iv) /= rmiss)

            tmp_arr(:,1) = this%voldati(:,1,i,j,k,iv) - (this%voldati(:,2,i,j,k,iv) - this%voldati(:,1,i,j,k,iv)) / 2.

          end WHERE

                                ! Rest of the matrix
          WHERE(this%voldati(:,1:this%griddim%dim%ny-1,i,j,k,iv) /= rmiss .AND. &
           this%voldati(:,2:this%griddim%dim%ny,i,j,k,iv) /= rmiss)

            tmp_arr(:,2:this%griddim%dim%ny) = (this%voldati(:,1:this%griddim%dim%ny-1,i,j,k,iv) + &
             this%voldati(:,2:this%griddim%dim%ny,i,j,k,iv)) / 2.
            
          end WHERE

          this%voldati(:,:,i,j,k,iv)=tmp_arr
            

        CASE DEFAULT

          call l4f_category_log(this%category,L4F_ERROR,"C grid type not known")
          call raise_fatal_error ("volgrid6d: C grid type not kmow")

        END select

      ENDDO
    ENDDO
  ENDDO
ENDDO timerange

DEALLOCATE (tmp_arr)

end subroutine vg6d_c2a_mat


!> \brief Display object on screen
!!
!! Show brief content on screen.
subroutine display_volgrid6d (this)

TYPE(volgrid6d),intent(in) :: this !< object to display
integer :: i

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"ora mostro gridinfo " )
#endif

print*,"----------------------- volgrid6d display ---------------------"
call display(this%griddim)

IF (ASSOCIATED(this%time))then
  print*,"---- time vector ----"
  print*,"elements=",size(this%time)
  do i=1, size(this%time)
    call display(this%time(i))
  end do
end IF

IF (ASSOCIATED(this%timerange))then
  print*,"---- timerange vector ----"
  print*,"elements=",size(this%timerange)
  do i=1, size(this%timerange)
    call display(this%timerange(i))
  end do
end IF

IF (ASSOCIATED(this%level))then
  print*,"---- level vector ----"
  print*,"elements=",size(this%level)
  do i=1, size(this%level)
    call display(this%level(i))
  end do
end IF

IF (ASSOCIATED(this%var))then
  print*,"---- var vector ----"
  print*,"elements=",size(this%var)
  do i=1, size(this%var)
    call display(this%var(i))
  end do
end IF

IF (ASSOCIATED(this%gaid))then
  print*,"---- gaid vector (present mask only) ----"
  print*,"elements=",shape(this%gaid)
  print* ,c_e(this%gaid)
end IF

print*,"--------------------------------------------------------------"


end subroutine display_volgrid6d


!> \brief Display vector of object on screen
!!
!! Show brief content on screen.
subroutine display_volgrid6dv (this)

TYPE(volgrid6d),intent(in) :: this(:) !< vector of object to display
integer :: i

print*,"----------------------- volgrid6d  vector ---------------------"

print*,"elements=",size(this)

do i=1, size(this)

#ifdef DEBUG
  call l4f_category_log(this(i)%category,L4F_DEBUG,"ora mostro il vettore volgrid6d" )
#endif

  call display(this(i))

end do
print*,"--------------------------------------------------------------"

end subroutine display_volgrid6dv




end module volgrid6d_class



!>\example example_vg6d_3.f90
!!\brief Programma esempio semplice per gridinfo e volgrid6d.
!!
!! Programma che importa da file un vettore di gridinfo poi lo importa in volgrid6d. Da volgrid6d viene di nuovo creato un vettore di gridinfo per poi exportare su file.

!>\example example_vg6d_5.f90
!!\brief  Programma trasformazione da volgrid6d a volgrid6d
!!
!! Legge grib da un file e li organizza in un vettore di strutture
!! volgrid6d mettendoli a disposizione per eventuali elaborazioni;
!! vengono poi riesportati a un file grib

!>\example  example_vg6d_8.f90
!! \brief Programma scrittura su file vettore di anagrafica

!>\example example_vg6d_6.f90
!! \brief Programma trasformazione da volgrid6d a vol7d

!>\example example_vg6d_7.f90
!! \brief Programma trasformazione da vol7d a volgrid7d
