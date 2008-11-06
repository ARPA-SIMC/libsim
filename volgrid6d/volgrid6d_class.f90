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
  MODULE PROCEDURE delete_volgrid6d,delete_volgrid6dv
END INTERFACE

!> Lettura da file.
INTERFACE import
  MODULE PROCEDURE volgrid6d_read_from_file,import_from_gridinfo,import_from_gridinfovv
END INTERFACE


!> Scrittura su file.
INTERFACE export
  MODULE PROCEDURE volgrid6d_write_on_file,export_to_gridinfo,export_to_gridinfov,export_to_gridinfovv
END INTERFACE



private

public volgrid6d,init,delete,export,import


contains


subroutine init_volgrid6d (this,griddim,categoryappend)
type(volgrid6d) :: this
!> descrittore del grigliato
type(griddim_def),optional :: griddim
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

call l4f_category_log(this%category,L4F_DEBUG,"init")

call init(this%griddim)

if (present(griddim))then
  this%griddim=griddim
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

call l4f_category_log(this%category,L4F_DEBUG,"alloc")


IF (PRESENT(ini)) THEN
  linit = ini
ELSE
  linit = .FALSE.
ENDIF


if (present(dim)) this%griddim%dim=dim


IF (PRESENT(ntime)) THEN
  IF (ntime >= 0) THEN
    IF (ASSOCIATED(this%time)) DEALLOCATE(this%time)
    call l4f_category_log(this%category,L4F_DEBUG,"alloc ntime "//to_char(ntime))
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
    call l4f_category_log(this%category,L4F_DEBUG,"alloc nlevel "//to_char(nlevel))
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
    call l4f_category_log(this%category,L4F_DEBUG,"alloc ntimerange "//to_char(ntimerange))
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
    call l4f_category_log(this%category,L4F_DEBUG,"alloc nvar "//to_char(nvar))
    ALLOCATE(this%var(nvar))
    IF (linit) THEN
      DO i = 1, nvar
        CALL init(this%var(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF

end SUBROUTINE volgrid6d_alloc


SUBROUTINE volgrid6d_alloc_vol(this, ini, inivol,decode)
TYPE(volgrid6d),INTENT(inout) :: this !< oggetto di cui allocare i volumi
LOGICAL,INTENT(in),OPTIONAL :: ini !< se fornito e vale \c .TRUE., viene chiamato il costruttore, senza parametri opzionali, per ogni elemento di tutti i descrittori allocati
LOGICAL,INTENT(in),OPTIONAL :: inivol !< se fornito e vale \c .FALSE., i volumi allocati non saranno inizializzati a valore mancante
LOGICAL,INTENT(in),OPTIONAL :: decode !< se fornito e vale \c .FALSE., i volumi dati non saranno allocati (gaid si comunque)


LOGICAL :: linivol,ldecode

call l4f_category_log(this%category,L4F_DEBUG,"alloc_vol")

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

     call l4f_category_log(this%category,L4F_DEBUG,"alloco voldati")

     ALLOCATE(this%voldati( this%griddim%dim%nx,this%griddim%dim%ny,&
          SIZE(this%time), SIZE(this%level), &
          SIZE(this%timerange), SIZE(this%var)))
  
     IF (linivol) this%voldati = rmiss

  end if

  call l4f_category_log(this%category,L4F_DEBUG,"alloco gaid")
  ALLOCATE(this%gaid( &
   SIZE(this%time), SIZE(this%level), &
   SIZE(this%timerange), SIZE(this%var)))
  
  IF (linivol) this%gaid  = imiss


  
end if


END SUBROUTINE volgrid6d_alloc_vol




subroutine delete_volgrid6d(this)
type(volgrid6d) :: this

call l4f_category_log(this%category,L4F_DEBUG,"delete")

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

call l4f_category_log(this%category,L4F_DEBUG,"write on file")

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

call l4f_category_log(this%category,L4F_DEBUG,"read from file")

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

call l4f_category_log(this%category,L4F_DEBUG,"import_from_gridinfo: "//trim(type))

if (.not. c_e(type))then

   call init(this,gridinfo%griddim,categoryappend)

else if (.not. (this%griddim == gridinfo%griddim ))then

   call l4f_category_log(this%category,L4F_DEBUG,"volgrid6d: grid or dim are different and this is not possible")
   call raise_error ("volgrid6d: grid or dim are different and this is not possible")

end if

!TODO
! cosa torna index se notfound ?

if (associated (this%gaid))then

this%gaid(&
 index(this%time,     gridinfo%time),&
 index(this%timerange,gridinfo%timerange),&
 index(this%level,    gridinfo%level),&
 index(this%var,      gridinfo%var)&
 )=gridinfo%gaid

else

  call l4f_category_log(this%category,L4F_ERROR,&
   "gaid non allocato: chiama volgrid6d_alloc_vol")
  call raise_error("gaid non allocato: chiama volgrid6d_alloc_vol")

end if


if (associated (this%voldati))then

  this%voldati(:,:,&
   index(this%time,     gridinfo%time),&
   index(this%timerange,gridinfo%timerange),&
   index(this%level,    gridinfo%level),&
   index(this%var,      gridinfo%var)&
   ) = decode_gridinfo (gridinfo)

else

  call l4f_category_log(this%category,L4F_INFO,"non decodifico i dati")

end if

end subroutine import_from_gridinfo


subroutine export_to_gridinfo (this,gridinfo,itime,itimerange,ilevel,ivar,gaid_template)

TYPE(volgrid6d),INTENT(in) :: this !< Volume volgrid6d da leggere
type(gridinfo_type),intent(out) :: gridinfo !< gridinfo 
integer, optional :: gaid_template
integer ::itime,itimerange,ilevel,ivar

call l4f_category_log(this%category,L4F_DEBUG,"export_to_gridinfo")


if (present(gaid_template)) call grib_clone(gaid_template,gridinfo%gaid)

gridinfo%griddim   =this%griddim
gridinfo%time      =this%time(itime)
gridinfo%timerange =this%timerange(itimerange)
gridinfo%level     =this%level(ilevel)
gridinfo%var       =this%var(ivar)


if (.not. c_e(gridinfo%gaid))then

  if (c_e(this%gaid(itime,itimerange,ilevel,ivar)))then

    gridinfo%gaid = this%gaid(itime,itimerange,ilevel,ivar)

  else
 
    call l4f_category_log(this%category,L4F_ERROR,&
     "mancano tutti i gaid; export impossibile")
    call raise_error("mancano tutti i gaid; export impossibile")

  end if
end if


call encode_gridinfo(gridinfo,this%voldati(:,:,&
 itime,itimerange,ilevel,ivar))


end subroutine export_to_gridinfo



subroutine import_from_gridinfovv (this,gridinfov,categoryappend)

TYPE(volgrid6d),pointer :: this(:) !< Vettore Volume volgrid6d da leggere
type(gridinfo_type),intent(in) :: gridinfov(:) !< vettore gridinfo 
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

integer :: i,j
integer :: ngrid,ntime,ntimerange,nlevel,nvar
integer :: category
character(len=512) :: a_name

! category temporanea (altrimenti non possiamo loggare)
call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
category=l4f_category_get(a_name)

!type(gridinfo_type),allocatable :: gridinfovtmp(:)
!allocate(gridinfovtmp(size(gridinfov)))
!gridinfovtmp=(this%griddim(i) == gridinfov%griddim(j))
!deallocate(gridinfovtmp)

ngrid=count_distinct(gridinfov%griddim,back=.true.)
call l4f_category_log(category,L4F_INFO,&
     "numero delle aree differenti: "//to_char(ngrid))

allocate (this(ngrid))

do i=1,ngrid
   call init (this(i), categoryappend=categoryappend)
end do

this%griddim=pack_distinct(gridinfov%griddim,ngrid,back=.true.)


do i=1,ngrid
   
   ntime = count_distinct(gridinfov%time,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   ntimerange = count_distinct(gridinfov%timerange,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   nlevel = count_distinct(gridinfov%level,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   nvar = count_distinct(gridinfov%var,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   
!   call init (this(i),this(i)%griddim, categoryappend)
   call l4f_category_log(this(i)%category,L4F_DEBUG,"import from gridinfo vettori")
   
   call volgrid6d_alloc(this(i),this(i)%griddim%dim,ntime=ntime,ntimerange=ntimerange,nlevel=nlevel,nvar=nvar)
   
   this(i)%time=pack_distinct(gridinfov%time,ntime,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   this(i)%timerange=pack_distinct(gridinfov%timerange,ntimerange,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   this(i)%level=pack_distinct(gridinfov%level,nlevel,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)
   this(i)%var=pack_distinct(gridinfov%var,nvar,mask=(this(i)%griddim == gridinfov%griddim),back=.true.)

   call volgrid6d_alloc_vol(this(i)) 

end do


do i=1,size(gridinfov)

!   call l4f_category_log(category,L4F_INFO,&
!        "import volgrid6d numero: "//to_char(index(this%griddim,gridinfov(i)%griddim)))
!   call display(gridinfov(i)%griddim)
!   call display(this(index(this%griddim,gridinfov(i)%griddim))%griddim)

   call import (this(index(this%griddim,gridinfov(i)%griddim)),gridinfov(i),categoryappend)

end do

                                !chiudo il logger temporaneo
call l4f_category_delete(category)

end subroutine import_from_gridinfovv


subroutine export_to_gridinfov (this,gridinfov,gaid_template)

TYPE(volgrid6d),INTENT(in) :: this !< Volume volgrid6d da leggere
type(gridinfo_type),intent(out) :: gridinfov(:) !< vettore gridinfo 
integer, optional :: gaid_template

integer :: i,itime,itimerange,ilevel,ivar
integer :: ngridinfo,ntime,ntimerange,nlevel,nvar

call l4f_category_log(this%category,L4F_DEBUG,"export to gridinfo singola area")

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
        
        call export (this,gridinfov(i),itime,itimerange,ilevel,ivar,gaid_template)
        
      end do
    end do
  end do
end do

end subroutine export_to_gridinfov


subroutine export_to_gridinfovv (this,gridinfov,gaid_template,categoryappend)

TYPE(volgrid6d),INTENT(in)  :: this(:)      !< vettore volume volgrid6d da leggere
type(gridinfo_type),pointer :: gridinfov(:) !< vettore gridinfo 
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran
integer, optional :: gaid_template

integer :: i,igrid,ngrid,start,end,ngridinfo,ngridinfoin
integer :: category
character(len=512) :: a_name

! category temporanea (altrimenti non possiamo loggare)
call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
category=l4f_category_get(a_name)

ngrid=size(this)

ngridinfo=0
if (.not. associated(gridinfov))then
  do igrid=1,ngrid

    ngridinfo=ngridinfo+size(this(igrid)%gaid)

  end do
  
  allocate (gridinfov(ngridinfo))

  do i=1,ngridinfo
    call init(gridinfov(i),categoryappend=categoryappend)
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

  call l4f_category_log(this(igrid)%category,L4F_DEBUG,"export to gridinfo vettori")

  call export (this(igrid),gridinfov(start:end),gaid_template)

end do

                                !chiudo il logger
call l4f_category_delete(category)
  

end subroutine export_to_gridinfovv




subroutine delete_volgrid6dv(this)
type(volgrid6d) :: this(:)
integer :: i

do i=1,size(this)

  call l4f_category_log(this(i)%category,L4F_DEBUG,"delete vettori")

  call delete(this(i))

end do

end subroutine delete_volgrid6dv




end module volgrid6d_class
