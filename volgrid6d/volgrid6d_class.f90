module volgrid6d_class


USE grid_class
USE datetime_class
USE vol7d_timerange_class
USE vol7d_level_class
USE volgrid6d_var_class
use log4fortran
USE vol7d_utilities
use gridinfo_class

IMPLICIT NONE

character (len=255),parameter:: subcategory="volgrid6d_class"


!> Definisce un oggetto contenente le informazioni e i dati relativi a grib
!! su un grigliato omogeneo
type volgrid6d

!> descrittore del grigliato
  type(griddim_def) :: griddim
!> descrittore della dimensione tempo
  TYPE(datetime),pointer :: time(:)
!> descrittore della dimensione intervallo temporale (timerange)
  TYPE(vol7d_timerange),pointer :: timerange(:)
!> descrittore della dimensione livello verticale
  TYPE(vol7d_level),pointer :: level(:)
!> vettore descrittore della dimensione variabile di anagrafica
  TYPE(volgrid6d_var),pointer :: var(:)

  integer,pointer :: gaid(:,:,:,:)
  
  real,pointer :: voldati(:,:,:,:,:,:)


  integer :: category !< log4fortran

end type volgrid6d


INTERFACE init
  MODULE PROCEDURE init_volgrid6d
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_volgrid6d
END INTERFACE

!> Lettura da file.
INTERFACE import
  MODULE PROCEDURE volgrid6d_read_from_file,import_from_gridinfo,import_from_gridinfov
END INTERFACE


!> Scrittura su file.
INTERFACE export
  MODULE PROCEDURE volgrid6d_write_on_file,export_to_gridinfo,export_to_gridinfov
END INTERFACE



private

public volgrid6d,init,delete,export,import


contains


subroutine init_volgrid6d (this,grid,categoryappend)
type(volgrid6d) :: this
!> descrittore del grigliato
type(grid_def),optional :: grid
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

if (present(grid))then
  this%griddim%grid=grid
else
  call init(this%griddim)
end if

 ! call init(this%time)         
 ! call init(this%timerange)    
 ! call init(this%level)        
 ! call init(this%var)          

nullify (this%time,this%timerange,this%level,this%var)
nullify (this%gaid,this%voldati)          

end subroutine init_volgrid6d



SUBROUTINE volgrid6d_alloc(this, dim, ntime, nlevel, ntimerange, nvar, ini)

TYPE(volgrid6d),INTENT(inout) :: this !< oggetto di cui allocare i descrittori
type(grid_dim),INTENT(in),OPTIONAL :: dim !< estensione delle dimensioni X,Y orizzontali
INTEGER,INTENT(in),OPTIONAL :: ntime !< estensione della dimensione tempo
INTEGER,INTENT(in),OPTIONAL :: nlevel !< estensione della dimensione livello varticale
INTEGER,INTENT(in),OPTIONAL :: ntimerange !< estensione della dimensione intervallo temporale (timerange)
INTEGER,INTENT(in),OPTIONAL :: nvar !< estensione della dimensione variabile
LOGICAL,INTENT(in),OPTIONAL :: ini !< se fornito e vale \c .TRUE., viene chiamato il costruttore, senza parametri opzionali, per ogni elemento di tutti i descrittori allocati, inizializzandolo quindi a valore mancante

INTEGER :: i
LOGICAL :: linit

IF (PRESENT(ini)) THEN
  linit = ini
ELSE
  linit = .FALSE.
ENDIF


if (present(dim)) this%griddim%dim=dim


IF (PRESENT(ntime)) THEN
  IF (ntime >= 0) THEN
    IF (ASSOCIATED(this%time)) DEALLOCATE(this%time)
    ALLOCATE(this%time(ntime))
    IF (linit) THEN
      DO i = 1, ntime
        CALL init(this%time(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nlevel)) THEN
  IF (nlevel >= 0) THEN
    IF (ASSOCIATED(this%level)) DEALLOCATE(this%level)
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
    ALLOCATE(this%var(nvar))
    IF (linit) THEN
      DO i = 1, nvar
        CALL init(this%var(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF

end SUBROUTINE volgrid6d_alloc


SUBROUTINE volgrid6d_alloc_vol(this, ini, inivol)
TYPE(volgrid6d),INTENT(inout) :: this !< oggetto di cui allocare i volumi
LOGICAL,INTENT(in),OPTIONAL :: ini !< se fornito e vale \c .TRUE., viene chiamato il costruttore, senza parametri opzionali, per ogni elemento di tutti i descrittori allocati
LOGICAL,INTENT(in),OPTIONAL :: inivol !< se fornito e vale \c .TRUE., i volumi allocati saranno inizializzati a valore mancante

LOGICAL :: linivol


IF (PRESENT(inivol)) THEN
  linivol = inivol
ELSE
  linivol = .TRUE.
ENDIF


IF (this%griddim%dim%nx > 0 .and. this%griddim%dim%ny > 0 .and..NOT.ASSOCIATED(this%voldati)) THEN
                                ! Alloco i descrittori minimi per avere un volume di dati
  IF (.NOT. ASSOCIATED(this%var)) CALL volgrid6d_alloc(this, nvar=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%time)) CALL volgrid6d_alloc(this, ntime=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%level)) CALL volgrid6d_alloc(this, nlevel=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%timerange)) CALL volgrid6d_alloc(this, ntimerange=1, ini=ini)
  
  ALLOCATE(this%voldati( this%griddim%dim%nx,this%griddim%dim%ny,&
   SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%var)))
  
  IF (linivol) this%voldati(:,:,:,:,:,:) = rmiss
  
end if


END SUBROUTINE volgrid6d_alloc_vol




subroutine delete_volgrid6d(this)
type(volgrid6d) :: this

call delete(this%griddim)

!  call delete(this%time)
!  call delete(this%timerange)
!  call delete(this%level)
!  call delete(this%var)

if (associated( this%time )) deallocate(this%time)
if (associated( this%timerange )) deallocate(this%timerange)
if (associated( this%level )) deallocate(this%level)
if (associated( this%var )) deallocate(this%var)

if (associated(this%gaid))deallocate(this%gaid)
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
  print *, "opened: ",lfilename
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
  print *, "opened: ",lfilename
end if


read(unit=lunit)ldescription
read(unit=lunit)ltarray

print *,"Info: reading volgrid6d from file"
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


end subroutine volgrid6d_read_from_file




subroutine import_from_gridinfo (this,gridinfo,categoryappend)

TYPE(volgrid6d),INTENT(OUT) :: this !< Volume volgrid6d da leggere
type(gridinfo_type),intent(in) :: gridinfo !< gridinfo 
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran
character(len=255)   :: type


call get_val(this%griddim,type=type)

if (type == cmiss)then

   call init(this,gridinfo%griddim%grid,categoryappend)

else if (.not. (this%griddim == gridinfo%griddim ))then

!TODO inserire log4f
   
   call raise_error ("volgrid6d: grid or dim are different and this is not possible")

end if

!TODO
! cosa torna index se notfound ?


this%gaid(&
     index(this%time,     gridinfo%time),&
     index(this%timerange,gridinfo%timerange),&
     index(this%level,    gridinfo%level),&
     index(this%var,      gridinfo%var)&
     )=gridinfo%gaid


end subroutine import_from_gridinfo


subroutine export_to_gridinfo (this,gridinfo,itime,itimerange,ilevel,ivar)

TYPE(volgrid6d),INTENT(in) :: this !< Volume volgrid6d da leggere
type(gridinfo_type),intent(out) :: gridinfo !< gridinfo 
integer ::itime,itimerange,ilevel,ivar

gridinfo%griddim   =this%griddim
gridinfo%time      =this%time(itime)
gridinfo%timerange =this%timerange(itimerange)
gridinfo%level     =this%level(ilevel)
gridinfo%var       =this%var(ivar)
gridinfo%gaid      =this%gaid(itime,itimerange,ilevel,ivar)

end subroutine export_to_gridinfo



subroutine import_from_gridinfov (this,gridinfov,categoryappend)

TYPE(volgrid6d),INTENT(OUT) :: this(:) !< Vettore Volume volgrid6d da leggere
type(gridinfo_type),intent(in) :: gridinfov(:) !< vettore gridinfo 
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran
integer :: i

!!$call count_distinct(gridinfov%grid)
!!$
!!$
!!$
!!$call pack_distinct
!!$


!TODO
!contare le aree e tutto il resto
! init alloc e quant'altro


do i=1,size(gridinfov)

   call import (this(i),gridinfov(i),categoryappend)

end do


end subroutine import_from_gridinfov


subroutine export_to_gridinfov (this,gridinfov,categoryappend)

TYPE(volgrid6d),INTENT(in) :: this !< Volume volgrid6d da leggere
type(gridinfo_type),intent(out) :: gridinfov(:) !< vettore gridinfo 
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran
integer :: i,itime,itimerange,ilevel,ivar


if (size(gridinfov) /= size(this%gaid))then

   !TODO
   !log4f

   call raise_error("dimension mismach")
end if


! TODO
! questo loop è da riscrivere per fare lo scan del volume

do i=1,size(gridinfov)

   call export (this,gridinfov(i),itime,itimerange,ilevel,ivar)

end do


end subroutine export_to_gridinfov


end module volgrid6d_class
