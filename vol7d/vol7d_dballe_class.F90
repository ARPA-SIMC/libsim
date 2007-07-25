MODULE vol7d_dballe_class

USE char_utilities
USE vol7d_class
USE vol7d_utilities

IMPLICIT NONE

include "dballef.h"

TYPE vol7d_dballe
  TYPE(vol7d) :: vol7d
  !character(len=19) :: dsn,user,password
  integer :: idbhandle,handle,handle_staz,handle_err
  integer :: debug = 1

END TYPE vol7d_dballe


type record

  !! prime 5 dimensioni
  !integer :: ana_id
  TYPE(vol7d_ana) :: ana
  TYPE(datetime) :: time
  TYPE(vol7d_level) :: level
  TYPE(vol7d_timerange) :: timerange
  TYPE(vol7d_network) :: network
  TYPE(vol7d_var) ::  dativar

  !! Volumi di valori e attributi per  dati
  REAL :: datir
!  REAL(kind=fp_d),POINTER :: voldatid(:,:,:,:,:,:)
!  INTEGER,POINTER :: voldatii(:,:,:,:,:,:)
!  INTEGER(kind=int_b),POINTER :: voldatib(:,:,:,:,:,:)
!  CHARACTER(len=vol7d_cdatalen),POINTER :: voldatic(:,:,:,:,:,:)
  INTEGER(kind=int_b)   :: datiattrb(3)
!  REAL(kind=fp_d),POINTER :: voldatiattrd(:,:,:,:,:,:,:)
!  INTEGER,POINTER :: voldatiattri(:,:,:,:,:,:,:)
!  INTEGER(kind=int_b),POINTER :: voldatiattrb(:,:,:,:,:,:,:)
!  CHARACTER(len=vol7d_cdatalen),POINTER :: voldatiattrc(:,:,:,:,:,:,:)

END TYPE record


PRIVATE
PUBLIC vol7d_dballe, init, delete, import, export

INTERFACE init
  MODULE PROCEDURE vol7d_dballe_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_dballe_delete
END INTERFACE

INTERFACE import
  MODULE PROCEDURE vol7d_dballe_importvsns, vol7d_dballe_importvvns, &
   vol7d_dballe_importvsnv, vol7d_dballe_importvvnv
END INTERFACE

INTERFACE export
  MODULE PROCEDURE vol7d_dballe_export
END INTERFACE

!!$INTERFACE export
!!$  MODULE PROCEDURE vol7d_dballe_export 
!!$END INTERFACE

CONTAINS


SUBROUTINE vol7d_dballe_init(this,dsn,user,password,debug,write,wipe)
TYPE(vol7d_dballe),INTENT(out) :: this
character(len=*), INTENT(in),OPTIONAL :: dsn,user,password
integer,INTENT(in),OPTIONAL :: debug
integer :: ldebug
character(len=50) :: quidsn,quiuser,quipassword
logical,INTENT(in),OPTIONAL :: wipe,write
logical :: quiwrite,quiwipe


IF (PRESENT(debug)) THEN
  ldebug = debug
ELSE
  ldebug = 1
ENDIF

quidsn = "test"
quiuser = "test"
quipassword = ""

IF (PRESENT(dsn))quidsn = dsn
IF (PRESENT(user))quiuser = user
IF (PRESENT(password))quipassword = password

! utilizziamo la routine di default per la gestione dell'errore
call idba_error_set_callback(0,idba_default_error_handler, &
     ldebug,this%handle_err)

!quando scrivo bisogna gestire questo che non è da fare ?
CALL init(this%vol7d)

quiwrite=.false.
if (present(write))then
   quiwrite=write
endif

quiwipe=.false.
if (present(wipe))then
   quiwipe=wipe
endif

!print*,"write=",quiwrite,"wipe=",wipe,"dsn=",quidsn

if(quiwrite)then
   call idba_presentati(this%idbhandle,quidsn,quiuser,quipassword)
   call idba_preparati (this%idbhandle,this%handle,"write","write","write")
   call idba_preparati (this%idbhandle,this%handle_staz,"write","write","write")
else
   call idba_presentati(this%idbhandle,quidsn,quiuser,quipassword)
   call idba_preparati (this%idbhandle,this%handle,"read","read","read")
   call idba_preparati (this%idbhandle,this%handle_staz,"read","read","read")
end if

if (quiwipe)call idba_scopa (this%handle,"")

END SUBROUTINE vol7d_dballe_init



SUBROUTINE vol7d_dballe_importvsns(this, var, network, timei, timef,level,timerange, set_network,attr)
TYPE(vol7d_dballe),INTENT(out) :: this
CHARACTER(len=*),INTENT(in) :: var
INTEGER,INTENT(in) :: network
TYPE(datetime),INTENT(in) :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange

CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:)

CALL import(this, (/var/), network, timei, timef,level,timerange, set_network,attr)

END SUBROUTINE vol7d_dballe_importvsns


SUBROUTINE vol7d_dballe_importvsnv(this, var, network, timei, timef,level,timerange, set_network,attr)
TYPE(vol7d_dballe),INTENT(out) :: this
CHARACTER(len=*),INTENT(in) :: var
INTEGER,INTENT(in) :: network(:)
TYPE(datetime),INTENT(in) :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:)

INTEGER :: i

DO i = 1, SIZE(network)
  CALL import(this, (/var/), network(i), timei, timef, level,timerange,set_network,attr)
ENDDO

END SUBROUTINE vol7d_dballe_importvsnv


SUBROUTINE vol7d_dballe_importvvnv(this, var, network, timei, timef, level,timerange,set_network,attr)
TYPE(vol7d_dballe),INTENT(out) :: this
CHARACTER(len=*),INTENT(in) :: var(:)
INTEGER,INTENT(in) :: network(:)
TYPE(datetime),INTENT(in) :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:)

INTEGER :: i

DO i = 1, SIZE(network)
  CALL import(this, var, network(i), timei, timef, level,timerange,set_network)
ENDDO

END SUBROUTINE vol7d_dballe_importvvnv


SUBROUTINE vol7d_dballe_importvvns(this, var, network, timei, timef,level,timerange, set_network,attr)

TYPE(vol7d_dballe),INTENT(inout) :: this
CHARACTER(len=*),INTENT(in) :: var(:)
INTEGER,INTENT(in) :: network
TYPE(datetime),INTENT(in) :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:)

TYPE(vol7d) :: v7d
CHARACTER(len=SIZE(var)*7) :: varlist
CHARACTER(len=SIZE(attr)*8) :: starvarlist
CHARACTER(len=6) :: btable
CHARACTER(len=7) ::starbtable

LOGICAL ::  ldegnet, lattr
integer :: year,month,day,hour,minute,sec
integer :: rlevel, rl1, rl2
integer :: rtimerange, p1, p2
integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork


integer :: nana,ntime,ntimerange,nlevel,nnetwork
TYPE(vol7d_var) :: var_tmp

INTEGER :: i,ii, iii,j, k,n,nn, nvar,nvarattr,istat,ana_id,ana_id_staz,nstaz,ist,indattr
integer :: ndativarr,ndatiattrb,ndativarattrb,inddatiattr

REAL(kind=fp_geo) :: lat,lon ,dato 
INTEGER(kind=int_b)::attrdatib

!TYPE(datetime) :: odatetime
! nobs, ntime, nana, nvout, nvin, nvbt, &
! datai(3), orai(2), dataf(3), oraf(2),ist
!CHARACTER(len=12),ALLOCATABLE :: tmtmp(:)
!INTEGER,ALLOCATABLE :: anatmp(:), vartmp(:), mapdatao(:)
!LOGICAL :: found, non_valid, varbt_req(SIZE(vartable))

TYPE(vol7d) :: vol7dtmp

type(record),ALLOCATABLE :: buffer(:)

!!!  CALL print_info('Estratte dall''archivio '//TRIM(to_char(nobs)) // ' osservazioni')

IF (PRESENT(set_network)) THEN
   ldegnet = .TRUE.
ELSE
   ldegnet = .FALSE.
ENDIF

IF (PRESENT(attr)) THEN
   lattr = .TRUE.
ELSE
   lattr = .FALSE.
ENDIF


call idba_set (this%handle,"rep_cod",network)
call idba_set (this%handle,"mobile",0)
!print*,"network,mobile",network,0

CALL getval(timei, year=year, month=month, day=day, hour=hour, minute=minute)
call idba_setdatemin(this%handle,year,month,day,hour,minute,0)
!print *,"datemin",year,month,day,hour,minute,0


CALL getval(timef, year=year, month=month, day=day, hour=hour, minute=minute)
call idba_setdatemax(this%handle,year,month,day,hour,minute,0)
!print *,"datemax",year,month,day,hour,minute,0


! creo la stringa con l'elenco
varlist = ''
nvar=0
DO i = 1, SIZE(var)
   nvar = nvar + 1
   IF (nvar > 1) varlist(LEN_TRIM(varlist)+1:) = ',' 
   varlist(LEN_TRIM(varlist)+1:) = TRIM(var(i))
ENDDO
!print *,"varlist",varlist



if (lattr)then
   ! creo la stringa con l'elenco
   starvarlist = ''
   nvarattr=0
   DO i = 1, SIZE(attr)
      nvarattr = nvarattr + 1
      IF (nvarattr > 1) starvarlist(LEN_TRIM(starvarlist)+1:) = ',' 
      starvarlist(LEN_TRIM(starvarlist)+1:) = TRIM(attr(i))
   ENDDO
   !print *,"starvarlist",starvarlist

end if

!print *,"attr",attr

call idba_set(this%handle, "varlist",varlist )

if (present(timerange))then
   call idba_settimerange(this%handle, timerange%timerange, timerange%p1, timerange%p2)
end if

if (present(level))then
   call idba_setlevel(this%handle, level%level, level%l1, level%l2)
end if

call idba_voglioquesto (this%handle,N)
!print*,"numero di dati ",N

!ora che so quanti dati ho alloco la memoria per buffer
allocate(buffer(N),stat=istat)
if (istat/= 0) CALL raise_error('errore allocazione memoria')


! dammi tutti i dati
do i=1,N

   call idba_dammelo (this%handle,btable)

   call idba_enqdate (this%handle,year,month,day,hour,minute,sec)
   call idba_enqlevel(this%handle, rlevel, rl1, rl2)
   call idba_enqtimerange(this%handle, rtimerange, p1, p2)

   !nbtable=btable_numerico(btable)
   ! ind = firsttrue(qccli%v7d%dativar%r(:)%btable == nbtable)
   ! IF (ind<1) cycle ! non c'e'
   call idba_enq (this%handle,btable,dato)
   
   !metto in memoria l'identificatore numerico dei dati
   !call idba_enq (this%handle,"context_id",data_id)


   buffer(i)%datiattrb=DBA_MVB
   if (lattr)then        
      !print *,"starvarlist=",starvarlist
      !per ogni dato ora lavoro sugli attributi
      call idba_set(this%handle, "*varlist",starvarlist )
      call idba_voglioancora (this%handle,nn)
      !print*,"numero attributi",nn
      
      do ii=1,nn ! Se ho piu` di 1 attributo devo forse trovare l'indice (ii)
         call idba_ancora (this%handle,starbtable)
         !print *, starbtable
         indattr = firsttrue(attr == starbtable)
         IF (indattr<1) cycle ! non c'e'
         call idba_enq(this%handle,starbtable,attrdatib)
         !bufferizzo
         buffer(i)%datiattrb(indattr)=attrdatib
         !print *,"attributo",attrdatib
         
      end do
   end if

   !tramite ana_id imposto la stazione letta 
   !nella lettura dei dati in anagrafica 
   call idba_enq (this%handle, "ana_id",ana_id)
   
   if ( ana_id_staz /= ana_id )then 

      !print *,"ana_id",ana_id
      lat=DBA_MVR
      lon=DBA_MVR
      
      call idba_set (this%handle_staz,"ana_id",ana_id)
      
      ! Leggo l'anagrafica per la stazione
      call idba_quantesono(this%handle_staz,nstaz)
      !print *,"numero anagrafiche trovate",nstaz

      ! ciclo su tutte le stazioni (UNA SOLA)
      do  iii=1,nstaz
         call idba_elencamele(this%handle_staz)
         
         !recupero i dati di anagrafica
         call idba_enq (this%handle_staz,"lat",   lat)
         call idba_enq (this%handle_staz,"lon",   lon)
         !call idba_enq (this%handle_staz,"height",alt)
         !call idba_enq (this%handle_staz,"name",  name)
         
         ana_id_staz = ana_id
         
      end do
   
   end if

!bufferizzo

   !print *,"lat,lon",lat,lon
   !print*,year,month,day,hour,minute,sec
   !print*,btable,dato,buffer(i)%datiattrb

   !buffer(i)%ana_id=ana_id
   call init(buffer(i)%ana,lat=lat,lon=lon)
   call init(buffer(i)%time, year=year, month=month, day=day, hour=hour, minute=minute)
   call init(buffer(i)%level, rlevel,rl1,rl2)
   call init(buffer(i)%timerange, rtimerange, p1, p2)
   call init(buffer(i)%network, network)
   call init(buffer(i)%dativar, btable)
   buffer(i)%datir=dato

end do


nana = count_distinct(buffer%ana, back=.TRUE.)
ntime = count_distinct(buffer%time, back=.TRUE.)
ntimerange = count_distinct(buffer%timerange, back=.TRUE.)
nlevel = count_distinct(buffer%level, back=.TRUE.)
nnetwork = count_distinct(buffer%network, back=.TRUE.)

!da fare
!ndativarr= count_distinct(buffer%dativar, back=.TRUE.)
ndativarr= nvar

!print *, "nana=",nana," ntime=",ntime," ntimerange=",ntimerange, &
!" nlevel=",nlevel," nnetwork=",nnetwork," ndativarr=",ndativarr

if (lattr)then
   ndatiattrb=nvarattr
   ndativarattrb=ndativarr
else
   ndatiattrb=0
   ndativarattrb=0
end if

CALL init(vol7dtmp)

call vol7d_alloc (vol7dtmp, &
nana=nana, ntime=ntime, ntimerange=ntimerange, &
nlevel=nlevel, nnetwork=nnetwork, ndativarr=nvar, ndatiattrb=ndatiattrb,ndativarattrb=ndativarattrb )

vol7dtmp%ana=pack_distinct(buffer%ana, back=.TRUE.)
vol7dtmp%time=pack_distinct(buffer%time, back=.TRUE.)
vol7dtmp%timerange=pack_distinct(buffer%timerange, back=.TRUE.)
vol7dtmp%level=pack_distinct(buffer%level, back=.TRUE.)
vol7dtmp%network=pack_distinct(buffer%network, back=.TRUE.)

! qui sarebbe meglio usare la pack
do ii=1,ndativarr
   call init (vol7dtmp%dativar%r(ii), btable=var(ii))
end do
vol7dtmp%dativarattr%b=vol7dtmp%dativar%r

do ii=1,ndatiattrb
   call init (vol7dtmp%datiattr%b(ii), btable=attr(ii))
end do


call vol7d_alloc_vol (vol7dtmp)

vol7dtmp%voldatir=rmiss

do i =1, N

   indana = firsttrue(buffer(i)%ana == vol7dtmp%ana)
   indtime = firsttrue(buffer(i)%time == vol7dtmp%time)
   indtimerange = firsttrue(buffer(i)%timerange == vol7dtmp%timerange)
   indlevel = firsttrue(buffer(i)%level == vol7dtmp%level)
   indnetwork = firsttrue(buffer(i)%network == vol7dtmp%network)
   inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%r)

   !print *, indana,indtime,indlevel,indtimerange,inddativar,indnetwork

   vol7dtmp%voldatir( &
   indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
   ) = buffer(i)%datir

   do ii=1,ndatiattrb

      call init (var_tmp, btable=attr(ii))

      inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%b)
      vol7dtmp%voldatiattrb(indana,indtime,indlevel,indtimerange,&
           inddativar,indnetwork,inddatiattr)=buffer(i)%datiattrb(ii)

   end do


!( voldati*(nana,ntime,nlevel,ntimerange,ndativar*,nnetwork)
!  voldatiattr*(nana,ntime,nlevel,ntimerange,ndativarattr*,network,ndatiattr*) )

end do

! Se l'oggetto ha gia` un volume allocato lo fondo con quello estratto
!TODO manca test su associated dei vol*
IF (ASSOCIATED(this%vol7d%ana) .AND. ASSOCIATED(this%vol7d%time)) THEN
  CALL vol7d_merge(this%vol7d, vol7dtmp, sort=.TRUE.)
ELSE ! altrimenti lo assegno
  this%vol7d = vol7dtmp
ENDIF

call vol7d_set_attr_ind(this%vol7d)

END SUBROUTINE vol7d_dballe_importvvns


SUBROUTINE vol7d_dballe_export(this, network, latmin,latmax,lonmin,lonmax,staz_id,ident,timei, timef,level,timerange,var,attr)

! TODO: gestire staz_id la qual cosa vuol dire aggiungere un id nel type ana

TYPE(vol7d_dballe),INTENT(in) :: this
INTEGER,INTENT(in),optional :: network,staz_id
CHARACTER(len=vol7d_ana_lenident),optional :: ident
TYPE(datetime),INTENT(in),optional :: timei, timef
REAL(kind=fp_geo),INTENT(in),optional :: latmin,latmax,lonmin,lonmax
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:),attr(:)
logical, allocatable :: lnetwork(:),llevel(:),ltimerange(:)
integer,allocatable :: ana_id(:,:)
logical :: write,writeattr

!CHARACTER(len=6) :: btable
!CHARACTER(len=7) ::starbtable

integer :: year,month,day,hour,minute,sec
integer :: nstaz,ntime,ntimerange,nlevel,nnetwork


INTEGER :: i,ii,iii,iiii,iiiii,iiiiii,j,ind,inddatiattr

REAL(kind=fp_geo) :: lat,lon 
!INTEGER(kind=int_b)::attrdatib


integer :: ndativarr,ndatiattrr
integer :: ndativari,ndatiattri
integer :: ndativarb,ndatiattrb
integer :: ndativard,ndatiattrd
integer :: ndativarc,ndatiattrc

logical, allocatable :: lvarr(:),lattrr(:)
logical, allocatable :: lvari(:),lattri(:)
logical, allocatable :: lvarb(:),lattrb(:)
logical, allocatable :: lvard(:),lattrd(:)
logical, allocatable :: lvarc(:),lattrc(:)


nstaz=size(this%vol7d%ana(:))

ntimerange=size(this%vol7d%timerange(:))
allocate (ltimerange(ntimerange))

if (present(timerange))then
      where (timerange == this%vol7d%timerange(:))
         ltimerange(:)=.true.
      end where
else
   ltimerange(:)=.true.
end if

nlevel=size(this%vol7d%level(:))
allocate (llevel(nlevel))

if (present(level))then
      where (level == this%vol7d%level(:))
         llevel(:)=.true.
      end where
else
   llevel(:)=.true.
end if

nnetwork=size(this%vol7d%network(:))
ntime=size(this%vol7d%time(:))

allocate (lnetwork(nnetwork))
allocate (ana_id(nstaz,nnetwork))

if (present(network))then
      where (network == this%vol7d%network(:)%id)
         lnetwork(:)=.true.
      end where
else
   lnetwork(:)=.true.
end if

!!$
!!$!type $$
!!$
!!$if (associated(this%vol7d%dativar%$$))then
!!$   ndativar$$=size(this%vol7d%dativar%$$(:))
!!$   allocate (lvar$$(ndativar$$))
!!$   if (present(var))then
!!$      lvar$$(:)=.false.
!!$      do  i=1,size(var)
!!$         where (var(i) == this%vol7d%dativar%$$(:)%btable)
!!$            lvar$$(:)=.true.
!!$         end where
!!$      end do
!!$   else
!!$      lvar$$(:)=.true.
!!$   end if
!!$end if
!!$
!!$if (associated(this%vol7d%dativarattr%$$))then
!!$   ndatiattr$$=size(this%vol7d%datiattr%$$(:))
!!$   allocate (lattr$$(ndatiattr$$))
!!$   if (present(attr))then
!!$      lattr$$(:)=.false.
!!$      do  i=1,size(attr)
!!$         where (attr(i) == this%vol7d%datiattr%$$(:)%btable)
!!$            lattr$$(:)=.true.
!!$         end where
!!$      end do
!!$   else
!!$      lattr$$(:)=.true.
!!$   end if
!!$end if
!!$
!!$!# end type  $$


!type r

if (associated(this%vol7d%dativar%r))then
   ndativarr=size(this%vol7d%dativar%r(:))
   allocate (lvarr(ndativarr))
   if (present(var))then
      lvarr(:)=.false.
      do  i=1,size(var)
         where (var(i) == this%vol7d%dativar%r(:)%btable)
            lvarr(:)=.true.
         end where
      end do
   else
      lvarr(:)=.true.
   end if
end if

if (associated(this%vol7d%dativarattr%r))then
   ndatiattrr=size(this%vol7d%datiattr%r(:))
   allocate (lattrr(ndatiattrr))
   if (present(attr))then
      lattrr(:)=.false.
      do  i=1,size(attr)
         where (attr(i) == this%vol7d%datiattr%r(:)%btable)
            lattrr(:)=.true.
         end where
      end do
   else
      lattrr(:)=.true.
   end if
end if

!# end type  r



!type c

if (associated(this%vol7d%dativar%c))then
   ndativarc=size(this%vol7d%dativar%c(:))
   allocate (lvarc(ndativarc))
   if (present(var))then
      lvarc(:)=.false.
      do  i=1,size(var)
         where (var(i) == this%vol7d%dativar%c(:)%btable)
            lvarc(:)=.true.
         end where
      end do
   else
      lvarc(:)=.true.
   end if
end if

if (associated(this%vol7d%dativarattr%c))then
   ndatiattrc=size(this%vol7d%datiattr%c(:))
   allocate (lattrc(ndatiattrc))
   if (present(attr))then
      lattrc(:)=.false.
      do  i=1,size(attr)
         where (attr(i) == this%vol7d%datiattr%c(:)%btable)
            lattrc(:)=.true.
         end where
      end do
   else
      lattrc(:)=.true.
   end if
end if

!# end type  c

!type i

if (associated(this%vol7d%dativar%i))then
   ndativari=size(this%vol7d%dativar%i(:))
   allocate (lvari(ndativari))
   if (present(var))then
      lvari(:)=.false.
      do  i=1,size(var)
         where (var(i) == this%vol7d%dativar%i(:)%btable)
            lvari(:)=.true.
         end where
      end do
   else
      lvari(:)=.true.
   end if
end if

if (associated(this%vol7d%dativarattr%i))then
   ndatiattri=size(this%vol7d%datiattr%i(:))
   allocate (lattri(ndatiattri))
   if (present(attr))then
      lattri(:)=.false.
      do  i=1,size(attr)
         where (attr(i) == this%vol7d%datiattr%i(:)%btable)
            lattri(:)=.true.
         end where
      end do
   else
      lattri(:)=.true.
   end if
end if

!# end type  i

!type b

if (associated(this%vol7d%dativar%b))then
   ndativarb=size(this%vol7d%dativar%b(:))
   allocate (lvarb(ndativarb))
   if (present(var))then
      lvarb(:)=.false.
      do  i=1,size(var)
         where (var(i) == this%vol7d%dativar%b(:)%btable)
            lvarb(:)=.true.
         end where
      end do
   else
      lvarb(:)=.true.
   end if
end if

if (associated(this%vol7d%dativarattr%b))then
   ndatiattrb=size(this%vol7d%datiattr%b(:))
   allocate (lattrb(ndatiattrb))
   if (present(attr))then
      lattrb(:)=.false.
      do  i=1,size(attr)
         where (attr(i) == this%vol7d%datiattr%b(:)%btable)
            lattrb(:)=.true.
         end where
      end do
   else
      lattrb(:)=.true.
   end if
end if

!# end type  b

!type d

if (associated(this%vol7d%dativar%d))then
   ndativard=size(this%vol7d%dativar%d(:))
   allocate (lvard(ndativard))
   if (present(var))then
      lvard(:)=.false.
      do  i=1,size(var)
         where (var(i) == this%vol7d%dativar%d(:)%btable)
            lvard(:)=.true.
         end where
      end do
   else
      lvard(:)=.true.
   end if
end if

if (associated(this%vol7d%dativarattr%d))then
   ndatiattrd=size(this%vol7d%datiattr%d(:))
   allocate (lattrd(ndatiattrd))
   if (present(attr))then
      lattrd(:)=.false.
      do  i=1,size(attr)
         where (attr(i) == this%vol7d%datiattr%d(:)%btable)
            lattrd(:)=.true.
         end where
      end do
   else
      lattrd(:)=.true.
   end if
end if

!# end type  d 




call idba_unsetall (this%handle)
     
! vital statistics data
call idba_setcontextana (this%handle)

do iii=1, nnetwork
   if (.not.lnetwork(iii))cycle

   do i=1, nstaz

      ana_id(i,iii)=DBA_MVI

      CALL getval(this%vol7d%ana(i)%coord, lat=lat,lon=lon)

      if (present(latmin).and.present(latmax).and.present(lonmin).and.present(lonmax))then
         if (c_e(latmin) .and. c_e(latmax) .and. c_e(lonmin) .and. c_e(lonmax))then
            if (lat > latmax .or. lat < latmin .or. lon > lonmax .or. lon < lonmin) cycle
         end if
      end if


      call idba_set (this%handle,"lat",lat)
      call idba_set (this%handle,"lon",lon)

      if (present(ident))then
         if (c_e(ident) .and. ident /= this%vol7d%ana(i)%ident ) cycle
      end if

      if ( c_e(this%vol7d%ana(i)%ident)) then
         call idba_set (this%handle,"ident",ident)
         call idba_set (this%handle,"mobile",1)
      else
         call idba_set (this%handle,"mobile",0)
      end if

      call idba_set(this%handle,"rep_cod",this%vol7d%network(iii)%id)
      call idba_set(this%handle,"name","test")

      do ii=1,size(this%vol7d%anavar%r(:))
         
         print*,this%vol7d%anavar%r(ii)%btable , this%vol7d%volanar(i,ii,iii)

         call idba_set (this%handle,this%vol7d%anavar%r(ii)%btable , this%vol7d%volanar(i,ii,iii))
      end do
      do ii=1,size(this%vol7d%anavar%i(:))
         print *,this%vol7d%anavar%i(ii)%btable , this%vol7d%volanai(i,ii,iii)
         call idba_set (this%handle,this%vol7d%anavar%i(ii)%btable , this%vol7d%volanai(i,ii,iii))
      end do
      do ii=1,size(this%vol7d%anavar%c(:))
         print *,this%vol7d%anavar%c(ii)%btable , this%vol7d%volanac(i,ii,iii)
         call idba_set (this%handle,this%vol7d%anavar%c(ii)%btable , this%vol7d%volanac(i,ii,iii))
      end do
      
      call idba_prendilo ((this%handle))
      call idba_enq (this%handle,"ana_id",ana_id(i,iii))
   end do
end do

! data

call idba_unsetall (this%handle)

print *,"nstaz,ntime,nlevel,ntimerange,nnetwork",nstaz,ntime,nlevel,ntimerange,nnetwork

do i=1, nstaz
   do ii=1,ntime
      if (present(timei) .and. present(timef))then
         if (this%vol7d%time(ii) < timei .or. this%vol7d%time(ii) > timef ) cycle
      endif
      do iii=1,nlevel
         if (.not.llevel(iii))cycle
         do iiii=1,ntimerange
            if (.not.ltimerange(iiii))cycle
            do iiiiii=1, nnetwork
               if (.not.lnetwork(iiiiii))cycle

               write=.false.

               if (.not. c_e(ana_id(i,iiiiii))) cycle
               call idba_set (this%handle,"ana_id",ana_id(i,iiiiii))

               call idba_set (this%handle,"rep_cod",this%vol7d%network(iiiiii)%id)
               call idba_setlevel(this%handle, this%vol7d%level(iii)%level, this%vol7d%level(iii)%l1, this%vol7d%level(iii)%l2)
               call idba_settimerange(this%handle, this%vol7d%timerange(iiii)%timerange, &
                    this%vol7d%timerange(iiii)%p1, this%vol7d%timerange(iiii)%p2)

               CALL getval(this%vol7d%time(ii), year=year, month=month, day=day, hour=hour, minute=minute)
               call idba_setdate (this%handle,year,month,day,hour,minute,0)

               ! add or rewrite new data
               !voldati*(nana,ntime,nlevel,ntimerange,ndativar*,nnetwork)


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var.F90"
#undef VOL7D_POLY_TYPES_V


!!$               do iiiii=1,ndativarr
!!$                  if (.not.lvarr(iiiii))cycle
!!$                  if (c_e(this%vol7d%voldatir(i,ii,iii,iiii,iiiii,iiiiii)))then
!!$                     call idba_set (this%handle,this%vol7d%dativar%r(iiiii)%btable , &
!!$                          this%vol7d%voldatir(i,ii,iii,iiii,iiiii,iiiiii)) 
!!$                     if (ndatiattrb > 0 )then 
!!$                        if call idba_prendilo (this%handle)
!!$                        do inddatiattr=1,ndatiattrb
!!$                           call idba_set (this%handle, vol7dtmp%datiattr%b(inddatiattr)%btable,&
!!$                                vol7dtmp%voldatiattrb((i,ii,iii,iiii,iiiii,iiiiii,inddatiattr)) 
!!$                           eriteattr=.true.
!!$                        end do
!!$                        if (writeattr) call idba_critica (this%handle)
!!$                     else
!!$                        write=.true.
!!$                     end if
!!$                  end if
!!$               end do
!!$
!!$               do iiiii=1,size(this%vol7d%dativar%i(:))
!!$                  if (c_e(this%vol7d%voldatii(i,ii,iii,iiii,iiiii,iiiiii)))then
!!$                     call idba_set (this%handle,this%vol7d%dativar%i(iiiii)%btable , &
!!$                          this%vol7d%voldatii(i,ii,iii,iiii,iiiii,iiiiii)) 
!!$                     write=.true.
!!$                  end if
!!$               end do
!!$
!!$               do iiiii=1,size(this%vol7d%dativar%c(:))
!!$                  if (c_e(this%vol7d%voldatic(i,ii,iii,iiii,iiiii,iiiiii)))then 
!!$                     call idba_set (this%handle,this%vol7d%dativar%c(iiiii)%btable , &
!!$                          this%vol7d%voldatic(i,ii,iii,iiii,iiiii,iiiiii)) 
!!$                     write=.true.
!!$                  end if
!!$               end do
!!$
!!$               if (write) call idba_prendilo (this%handle)
!!$
!!$
!!$               do inddatiattr=1,ndatiattrb
!!$
!!$                  call idba_set (this%handle, vol7dtmp%datiattr%b(inddatiattr)%btable,&
!!$                       vol7dtmp%voldatiattrb((i,ii,iii,iiii,iiiii,iiiiii,inddatiattr)) 
!!$               end do
!!$               if (write) call idba_critica (this%handle)

            end do
         end do
      end do
   end do
end do

END SUBROUTINE vol7d_dballe_export

!!$SUBROUTINE vol7d_dballe_filter(this, var, network, latmin,latmax,ident,timei, timef,level,timerange,attr)
!!$
!!$TYPE(vol7d_dballe),INTENT(in) :: this
!!$CHARACTER(len=*),INTENT(in,optional) :: var(:)
!!$INTEGER,INTENT(in),optional :: network
!!$TYPE(datetime),INTENT(in),optional :: timei, timef
!!$TYPE(vol7d_level),INTENT(in),optional :: level
!!$TYPE(vol7d_timerange),INTENT(in),optional :: timerange
!!$CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:)
!!$logical, allocatable :: lvar(:), lnetwork(:),llevel(:),ltimerange(:),lattr(:)
!!$
!!$
!!$TYPE(vol7d) :: v7d
!!$CHARACTER(len=6) :: btable
!!$CHARACTER(len=7) ::starbtable
!!$
!!$integer :: year,month,day,hour,minute,sec
!!$integer :: rlevel, l1, l2
!!$integer :: rtimerange, p1, p2
!!$integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork
!!$
!!$
!!$integer :: nana,ntime,ntimerange,nlevel,nnetwork
!!$TYPE(vol7d_var) :: var_tmp
!!$
!!$INTEGER :: i,ii, iii,j, k,n,nn, nvar,nvarattr,istat,ana_id,ana_id_staz,nstaz,ist,indattr
!!$integer :: ndativarr,ndatiattrb,ndativarattrb,inddatiattr
!!$
!!$REAL(kind=fp_geo) :: lat,lon ,dato 
!!$INTEGER(kind=int_b)::attrdatib
!!$
!!$
!!$
!!$CALL getval(timei, year=year, month=month, day=day, hour=hour, minute=minute)
!!$!print *,"datemin",year,month,day,hour,minute,0
!!$
!!$CALL getval(timef, year=year, month=month, day=day, hour=hour, minute=minute)
!!$!print *,"datemax",year,month,day,hour,minute,0
!!$
!!$if (present(var))then
!!$   nvar= SIZE(var)
!!$else
!!$   nvar=0
!!$end if
!!$
!!$IF (PRESENT(attr)) THEN
!!$   nvarattr= SIZE(attr)
!!$ELSE
!!$   nvarattr=0
!!$ENDIF
!!$
!!$
!!$
!!$
!!$if (present(timerange))then
!!$end if
!!$
!!$
!!$if (present(level))then
!!$end if
!!$
!!$END SUBROUTINE vol7d_dballe_filter


SUBROUTINE vol7d_dballe_delete(this)
TYPE(vol7d_dballe) :: this

call idba_fatto(this%handle)
call idba_fatto(this%handle_staz)
call idba_arrivederci(this%idbhandle)

!this%dsn=cmiss
!this%user=cmiss
!this%password=cmiss
this%idbhandle=imiss
this%handle=imiss
this%handle_err=imiss
this%handle_staz=imiss

CALL delete(this%vol7d)

END SUBROUTINE vol7d_dballe_delete


END MODULE 
