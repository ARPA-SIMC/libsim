!> \defgroup volgrid6d Pacchetto volgrid6d, libreria volgrid6d

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
USE datetime_class
USE vol7d_timerange_class
USE vol7d_level_class
USE volgrid6d_var_class
use log4fortran
USE vol7d_utilities
use gridinfo_class
use grib_api
use optional_values
use vol7d_class

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


  integer :: category !< log4fortran

end type volgrid6d


!> \brief Costructor
!!
!! create a new istanze of object
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
  MODULE PROCEDURE volgrid6d_read_from_file,import_from_gridinfo,import_from_gridinfovv,&
   volgrid6d_import_from_grib
END INTERFACE


!> Exportazione
INTERFACE export
  MODULE PROCEDURE volgrid6d_write_on_file,export_to_gridinfo,export_to_gridinfov,export_to_gridinfovv,&
   volgrid6d_export_to_grib
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

private

public volgrid6d,init,delete,export,import,compute,transform


contains


!> \brief Costructor
!!
!! create a new istanze of object
subroutine init_volgrid6d (this,griddim,categoryappend)
type(volgrid6d) :: this !< object to create
type(griddim_def),optional :: griddim !< descrittore del grigliato
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
    ALLOCATE(this%time(ntime))
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
    ALLOCATE(this%level(nlevel))
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
    ALLOCATE(this%timerange(ntimerange))
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
    ALLOCATE(this%var(nvar))
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


IF (this%griddim%dim%nx > 0 .and. this%griddim%dim%ny > 0 .and..NOT.ASSOCIATED(this%voldati)) THEN
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
          SIZE(this%level), SIZE(this%time),  &
          SIZE(this%timerange), SIZE(this%var)))
  
     IF (linivol) this%voldati = rmiss

  end if

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,"alloc gaid volume")
#endif
  ALLOCATE(this%gaid( SIZE(this%level),&
   SIZE(this%time), &
   SIZE(this%timerange), SIZE(this%var)))
  
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
  if (.not. exist) CALL raise_error('file do not exist; cannot open file')
  if (exist) open (unit=lunit,file=lfilename,form="UNFORMATTED")
  !print *, "opened: ",lfilename
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

!> \brief import from gridinfo object to volgrid6d
!!
!! Un oggetto gridinfo che al suo interno contiene un id di un grib delle grib_api e una sua sufficiente descrizione
!! viene importato nella struttura volgrid6d organizzandolo nella sua forma multidimensionale.
!! I descrittori di volgrid6d devono essere stati opportunamente inizializzati per poter permettere una corretta importazione.
SUBROUTINE import_from_gridinfo (this,gridinfo,force,clone,categoryappend)

TYPE(volgrid6d),INTENT(OUT) :: this !< Volume volgrid6d da leggere
type(gridinfo_type),intent(in) :: gridinfo !< gridinfo 
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
type(gridinfo_type),intent(out) :: gridinfo !< gridinfo 
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
   "clone to a new gaid")
#endif
else

  gaid=gridinfo%gaid

end if


if (.not. c_e(gaid))then

  if (c_e(this%gaid(ilevel,itime,itimerange,ivar)))then

    if (optio_log(clone))then
      gaid=-1
      call grib_clone(this%gaid(ilevel,itime,itimerange,ivar),gaid)
    else
      gaid = this%gaid(ilevel,itime,itimerange,ivar)
    end if
  else
 
    gaid=imiss
    call l4f_category_log(this%category,L4F_WARN,&
     "mancano tutti i gaid; export impossibile")
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
type(gridinfo_type),intent(in) :: gridinfov(:) !< vettore gridinfo 
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid to gridinfo
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

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

!type(gridinfo_type),allocatable :: gridinfovtmp(:)
!allocate(gridinfovtmp(size(gridinfov)))
!gridinfovtmp=(this%griddim(i) == gridinfov%griddim(j))
!deallocate(gridinfovtmp)

ngrid=count_distinct(gridinfov%griddim,back=.true.)
call l4f_category_log(category,L4F_INFO,&
     "numero delle aree differenti: "//to_char(ngrid))

allocate (this(ngrid))

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
type(gridinfo_type),intent(out) :: gridinfov(:) !< vettore gridinfo 
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
type(gridinfo_type),pointer :: gridinfov(:) !< vettore gridinfo in cui exportare
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
  
  allocate (gridinfov(ngridinfo))

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

type(gridinfo_type),allocatable :: gridinfo(:)
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

  allocate (gridinfo(ngrib))

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

type(gridinfo_type),pointer :: gridinfo(:)
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



!> \brief destructor
!!
!! delete vector of volgrid6d object
!! relase memory and delete category for logging
subroutine delete_volgrid6dv(this)
type(volgrid6d) :: this(:) !< vector of volgrid6d object

integer :: i

do i=1,size(this)

#ifdef DEBUG
  call l4f_category_log(this(i)%category,L4F_DEBUG,"delete volgrid6d vector index: "//trim(to_char(i)))
#endif

  call delete(this(i))

end do

end subroutine delete_volgrid6dv


!> \brief Calcola i nuovi dati secondo la trasformazione specificata
!!
!! Deve essere fornito l'oggetto di trasformazione e oggetti completi
SUBROUTINE volgrid6d_transform_compute(this, volgrid6d_in, volgrid6d_out,clone)
TYPE(grid_transform),INTENT(in) :: this !< oggetto di trasformazione per il grigliato
type(volgrid6d), INTENT(in) :: volgrid6d_in !< oggetto da trasformare
type(volgrid6d), INTENT(out) :: volgrid6d_out !> oggetto trasformato; deve essere completo (init, alloc, alloc_vol)
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

#ifdef DEBUG
            call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"clone gaid "//&
             trim(to_char(volgrid6d_in%gaid(ilevel,itime,itimerange,ivar))))
#endif
            volgrid6d_out%gaid(ilevel,itime,itimerange,ivar)=-1
            call grib_clone(volgrid6d_in%gaid(ilevel,itime,itimerange,ivar),&
             volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))
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
type(volgrid6d), INTENT(in) :: volgrid6d_in !< oggetto da trasformare
type(volgrid6d), INTENT(out) :: volgrid6d_out !< oggetto trasformato
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid da volgrid6d_in a volgrid6d_out
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

type(grid_transform) :: grid_trans
integer :: ntime, ntimerange, nlevel, nvar

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_transform")
#endif

call init (volgrid6d_out,griddim,categoryappend)

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (associated(volgrid6d_in%time)) ntime=size(volgrid6d_in%time)
if (associated(volgrid6d_in%timerange)) ntimerange=size(volgrid6d_in%timerange)
if (associated(volgrid6d_in%level)) nlevel=size(volgrid6d_in%level)
if (associated(volgrid6d_in%var)) nvar=size(volgrid6d_in%var)

call init(grid_trans, this, in=volgrid6d_in%griddim,out=volgrid6d_out%griddim,&
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
type(volgrid6d), INTENT(in) :: volgrid6d_in(:) !< vettore oggetti da trasformare
type(volgrid6d), pointer :: volgrid6d_out(:) !< vettore oggetti trasformati
logical , intent(in),optional :: clone !< se fornito e \c .TRUE., clona i gaid da volgrid6d_in a volgrid6d_out
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

integer :: i,n
n=size(volgrid6d_in)

allocate( volgrid6d_out(n))

do i=1,n

  call transform (this,griddim, volgrid6d_in(i), volgrid6d_out(i),clone,categoryappend=categoryappend)

end do

end subroutine volgrid6dv_transform



!> \brief Calcola i nuovi dati secondo la trasformazione specificata
!!
!! Deve essere fornito l'oggetto di trasformazione e oggetti completi
SUBROUTINE volgrid6d_v7d_transform_compute(this, volgrid6d_in, vol7d_out,networkid)
TYPE(grid_transform),INTENT(in) :: this !< oggetto di trasformazione per grigliato
type(volgrid6d), INTENT(in) :: volgrid6d_in !< oggetto da trasformare
type(vol7d), INTENT(out) :: vol7d_out !< oggetto trasformato
integer,optional,intent(in) :: networkid !< imposta il network in vol7d_out (default=254)

integer :: nana, ntime, ntimerange, nlevel, nvar, nnetwork
integer :: itime, itimerange, ilevel, ivar, inetwork
real,allocatable :: voldatir_out(:,:)

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_v7d_transform_compute")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0
nnetwork=0

nnetwork=size(vol7d_out%network)
if (present(networkid))then
  call init(vol7d_out%network(1),id=networkid)
else
  call init(vol7d_out%network(1),id=254)
end if

if (associated(volgrid6d_in%time))then
  ntime=size(volgrid6d_in%time)
                                !TODO tramutare in copy
  vol7d_out%time=volgrid6d_in%time
end if

if (associated(volgrid6d_in%timerange))then
  ntimerange=size(volgrid6d_in%timerange)
                                !TODO tramutare in copy
  vol7d_out%timerange=volgrid6d_in%timerange
end if

if (associated(volgrid6d_in%level))then
  nlevel=size(volgrid6d_in%level)
                                !TODO tramutare in copy
  vol7d_out%level=volgrid6d_in%level
end if

if (associated(volgrid6d_in%var))then
  nvar=size(volgrid6d_in%var)
  !TODO
  call vargrib2varbufr(volgrid6d_in%var, vol7d_out%dativar%r)
end if

nana=size(vol7d_out%ana)

allocate(voldatir_out(nana,1))

inetwork=1
do itime=1,ntime
  do itimerange=1,ntimerange
    do ilevel=1,nlevel
      do ivar=1,nvar
        
        voldatir_out=reshape(vol7d_out%voldatir(:,itime,ilevel,itimerange,ivar,inetwork),(/nana,1/))

        call compute(this, &
         volgrid6d_in%voldati(:,:,ilevel,itime,itimerange,ivar),&
         voldatir_out)

        vol7d_out%voldatir(:,itime,ilevel,itimerange,ivar,inetwork)=reshape(voldatir_out,(/nana/))

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

end SUBROUTINE volgrid6d_v7d_transform_compute


!> \brief Trasforma i dati secondo gli oggetti forniti
!!
!! L'oggetto trasformazione su grigliato viene creato e distrutto automaticamete
!! L'oggetto trasformato viene creato automaticamente
subroutine volgrid6d_v7d_transform(this,ana, volgrid6d_in, vol7d_out,networkid,categoryappend)
type(transform_def),intent(in) :: this !< oggetto che specifica la trasformazione
type(vol7d_ana),intent(in) :: ana(:) !< vettore di anagrafiche su cui effettuare la trasformazione
type(volgrid6d), INTENT(in) :: volgrid6d_in !< oggetto da trasformare
type(vol7d), INTENT(out) :: vol7d_out !< oggetto trasformato
integer,optional,intent(in) :: networkid !< imposta il network in vol7d_out (default=254)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

type(grid_transform) :: grid_trans
integer :: ntime, ntimerange, nlevel, nvar, nana

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_v7d_transform")
#endif

nana=size(ana)
ntime=0
ntimerange=0
nlevel=0
nvar=0

if (associated(volgrid6d_in%time)) ntime=size(volgrid6d_in%time)
if (associated(volgrid6d_in%timerange)) ntimerange=size(volgrid6d_in%timerange)
if (associated(volgrid6d_in%level)) nlevel=size(volgrid6d_in%level)
if (associated(volgrid6d_in%var)) nvar=size(volgrid6d_in%var)

call init(grid_trans, this, in=volgrid6d_in%griddim,ana=ana,&
 categoryappend=categoryappend)

!TODO aggiungere categoryappend
call init (vol7d_out)

call vol7d_alloc(vol7d_out, nana=nana, ntime=ntime, nlevel=nlevel, ntimerange=ntimerange, ndativarr=nvar, nnetwork=1)

!TODO tramutare in copy
vol7d_out%ana = ana

call vol7d_alloc_vol(vol7d_out)

!ensure unproj was called
!call griddim_unproj(volgrid6d_out%griddim)

call compute(grid_trans, volgrid6d_in, vol7d_out,networkid)

call delete (grid_trans)

end subroutine volgrid6d_v7d_transform


!> \brief Trasforma i dati secondo gli oggetti forniti (vettori)
!!
!! L'oggetto trasformazione su grigliato viene creato e distrutto automaticamete
!! L'oggetto trasformato viene creato automaticamente
subroutine volgrid6dv_v7d_transform(this,ana, volgrid6d_in, vol7d_out,networkid,categoryappend)
type(transform_def),intent(in) :: this !< oggetto che specifica la trasformazione
type(vol7d_ana),intent(in) :: ana(:) !< vettore di anagrafiche su cui effettuare la trasformazione
type(volgrid6d), INTENT(in) :: volgrid6d_in(:) !< vettore di oggetti da trasformare
type(vol7d), pointer :: vol7d_out(:) !< vettore di oggetti trasformati
integer,optional,intent(in) :: networkid !< imposta il network in vol7d_out (default=254)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

integer :: i,n
n=size(volgrid6d_in)

allocate( vol7d_out(n))

do i=1,n

  call transform (this,ana, volgrid6d_in(i), vol7d_out(i),networkid,categoryappend=categoryappend)

end do

end subroutine volgrid6dv_v7d_transform


!> \brief Calcola i nuovi dati secondo la trasformazione specificata
!!
!! Deve essere fornito l'oggetto di trasformazione e oggetti completi
SUBROUTINE v7d_volgrid6d_transform_compute(this, vol7d_in, volgrid6d_out, networkid)
TYPE(grid_transform),INTENT(in) :: this !< oggetto di trasformazione per grigliato
type(vol7d), INTENT(in) :: vol7d_in !< oggetto da trasformare
type(volgrid6d), INTENT(out) :: volgrid6d_out !< oggetto trasformato 
integer,optional,intent(in) :: networkid !< seleziona il network da exportare da vol7d (default=1)

integer :: nana, ntime, ntimerange, nlevel, nvar, nnetwork
integer :: itime, itimerange, ilevel, ivar, inetwork
real,allocatable :: voldatir_out(:,:)
type(vol7d_network) :: network

!TODO category sarebbe da prendere da vol7d
#ifdef DEBUG
call l4f_category_log(volgrid6d_out%category,L4F_DEBUG,"start v7d_volgrid6d_transform_compute")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (present(networkid))then
  call init(network,id=networkid)
  inetwork= index(vol7d_in%network,network)
else
  inetwork=1
end if

if (associated(vol7d_in%time))then
  ntime=size(vol7d_in%time)
                                !TODO tramutare in copy
  volgrid6d_out%time=vol7d_in%time
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
  !TODO
  call varbufr2vargrib(vol7d_in%dativar%r, volgrid6d_out%var)
end if

nana=size(vol7d_in%voldatir(:,1,1,1,1,1))

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

end SUBROUTINE v7d_volgrid6d_transform_compute


!> \brief Trasforma i dati secondo gli oggetti forniti
!!
!! L'oggetto trasformazione su grigliato viene creato e distrutto automaticamete
!! L'oggetto trasformato viene creato automaticamente
subroutine v7d_volgrid6d_transform(this,griddim, vol7d_in, volgrid6d_out, networkid,categoryappend)
type(transform_def),intent(in) :: this !< oggetto che specifica la trasformazione
type(griddim_def),intent(in) :: griddim  !< griddim che specifica la trasformazione
type(vol7d), INTENT(in) :: vol7d_in !< oggetto da trasformare
type(volgrid6d), INTENT(out) :: volgrid6d_out !< oggetto trasformato
integer,optional,intent(in) :: networkid  !< seleziona il network da exportare da vol7d (default=1)
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

call init(grid_trans, this, ana=vol7d_in%ana, griddim=griddim,&
 categoryappend=categoryappend)

call init (volgrid6d_out, griddim, categoryappend=categoryappend)

call volgrid6d_alloc(volgrid6d_out, griddim%dim, ntime=ntime, nlevel=nlevel, ntimerange=ntimerange, nvar=nvar)

call volgrid6d_alloc_vol(volgrid6d_out)

call compute(grid_trans, vol7d_in, volgrid6d_out, networkid)

call delete (grid_trans)

end subroutine v7d_volgrid6d_transform


! TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
elemental subroutine vargrib2varbufr(vargrib, varbufr)

type(volgrid6d_var),intent(in) :: vargrib
type(vol7d_var),intent(out) :: varbufr

call init(varbufr, btable="B12001")

end subroutine vargrib2varbufr


! TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
elemental subroutine varbufr2vargrib(varbufr, vargrib)

type(vol7d_var),intent(in) :: varbufr
type(volgrid6d_var),intent(out) :: vargrib

!SUBROUTINE volgrid6d_var_init(this, centre, category, number, discipline,description,unit)
!TODO cambia da grib1 a grib2
call init(vargrib, centre=200, category=1, number=11)

end subroutine varbufr2vargrib

end module volgrid6d_class



!>\example example_vg6d_3.f90
!!\brief Programma esempio semplice per gridinfo e volgrid6d.
!!
!! Programma che importa da file un vettore di gridinfo poi lo importa in volgrid6d. Da volgrid6d viene di nuovo creato un vettore di gridinfo per poi exportare su file.

!>\example example_vg6d_5.f90
!!\brief  Programma trasformazione da volgrid6d a volgrid6d

!>\example  example_vg6d_8.f90
!! \brief Programma scrittura su file vettore di anagrafica

!>\example example_vg6d_6.f90
!! \brief Programma trasformazione da volgrid6d a vol7d

!>\example example_vg6d_7.f90
!! \brief Programma trasformazione da vol7d a volgrid7d
