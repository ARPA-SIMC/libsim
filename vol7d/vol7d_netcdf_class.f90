
MODULE vol7d_netcdf_class

USE char_utilities
USE vol7d_class
USE vol7d_utilities
USE geo_coord_class
USE datetime_class
use netcdf

IMPLICIT NONE
PRIVATE
PUBLIC  import, export



!!$!>\brief importa
!!$INTERFACE import
!!$  MODULE PROCEDURE vol7d_netcdf_import
!!$END INTERFACE

!>\brief esporta
INTERFACE export
  MODULE PROCEDURE vol7d_netcdf_export
END INTERFACE



CONTAINS



subroutine vol7d_netcdf_export (this,ncconventions,ncunit,description,filename)

TYPE(vol7d),INTENT(IN) :: this !< volume vol7d da scrivere 
integer,optional,intent(inout) :: ncunit !< unit� netcdf su cui scrivere; se passata =0 ritorna il valore rielaborato (default =elaborato internamente da netcdf )
character(len=*),intent(in) :: ncconventions !< tipo di convenzione da utilizzare nella scrittura netcdf (per ora supportato "CF-1.1 vol7d")
character(len=*),intent(inout),optional :: filename !< nome del file su cui scrivere; se passato ="" ritorna il valore rielaborato
character(len=*),INTENT(IN),optional :: description !< descrizione del volume

integer :: lunit
character(len=254) :: ldescription,arg,lfilename
integer :: nana, ntime, ntimerange, nlevel, nnetwork, &
 ndativarr, ndativari, ndativarb, ndativard, ndativarc,&
 ndatiattrr, ndatiattri, ndatiattrb, ndatiattrd, ndatiattrc,&
 ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc,&
 nanavarr, nanavari, nanavarb, nanavard, nanavarc,&
 nanaattrr, nanaattri, nanaattrb, nanaattrd, nanaattrc,&
 nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc
!integer :: im,id,iy
integer :: tarray(8)
logical :: opened,exist

integer :: ana_ident_varid,ana_dimid,ana_lon_varid,ana_lat_varid &
 ,ident_len_dimid,var_len_dimid &
 ,level_dimid,level_vdim_dimid,level_vect_varid,network_dimid,network_id_varid,timerange_vdim_dimid &
 ,time_iminuti_varid,time_dimid,timerange_dimid,timerange_vect_varid,var_vdim_dimid &
 ,dativard_dimid,dativarr_dimid,dativari_dimid,dativarb_dimid,dativarc_len_dimid,dativarc_dimid &
 ,voldativarr_varid ,voldativari_varid ,voldativard_varid ,voldativarb_varid ,voldativarc_varid &
 ,anavard_dimid,anavarr_dimid,anavari_dimid,anavarb_dimid,anavarc_len_dimid,anavarc_dimid &
 ,volanavarr_varid ,volanavari_varid ,volanavard_varid ,volanavarb_varid ,volanavarc_varid &
 ,anavarr_varid,anavari_varid,anavard_varid,anavarb_varid,anavarc_varid &
 ,dativarr_varid,dativari_varid,dativard_varid,dativarb_varid,dativarc_varid


integer :: i

type(datetime) :: timeref 

if (ncconventions == "CF-1.1") then
   call vol7d_netcdf_export_CF (this,ncconventions,ncunit,description,filename)
else if (ncconventions /= "CF-1.1 vol7d") then
  print *,"ncconventions not supported: ", ncconventions
  call exit(1)
end if

!call idate(im,id,iy)
call date_and_time(values=tarray)
call getarg(0,arg)

if (present(description))then
  ldescription=description
else
  ldescription="Vol7d generated by: "//trim(arg)
end if


lfilename=trim(arg)//".nc"
if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename == "")then
    filename=lfilename
  else
    lfilename=filename
  end if
end if


if (.not. present(ncunit))then

   inquire(file=lfilename,EXIST=exist)
   if (exist) CALL raise_error('file exist; cannot open new file')
   call check( "0",nf90_create(lfilename, nf90_clobber, lunit) )
   print *, "opened: ",lfilename

else
  if (ncunit==0)then

     inquire(file=lfilename,EXIST=exist)
     if (exist) CALL raise_error('file exist; cannot open new file')
     call check( "0",nf90_create(lfilename, nf90_clobber, lunit) )
     print *, "opened: ",lfilename

    ncunit=lunit
  else
    lunit=ncunit
  end if
end if


call init(timeref,year=1,month=1,day=1,hour=00,minute=00)

nana=size(this%ana)
ntime=size(this%time)
ntimerange=size(this%timerange)
nlevel=size(this%level)
nnetwork=size(this%network)

ndativarr=0
ndativari=0
ndativarb=0
ndativard=0
ndativarc=0

if (associated(this%dativar%r)) ndativarr=size(this%dativar%r)
if (associated(this%dativar%i)) ndativari=size(this%dativar%i)
if (associated(this%dativar%b)) ndativarb=size(this%dativar%b)
if (associated(this%dativar%d)) ndativard=size(this%dativar%d)
if (associated(this%dativar%c)) ndativarc=size(this%dativar%c)

ndatiattrr=size(this%datiattr%r)
ndatiattri=size(this%datiattr%i)
ndatiattrb=size(this%datiattr%b)
ndatiattrd=size(this%datiattr%d)
ndatiattrc=size(this%datiattr%c)

ndativarattrr=size(this%dativarattr%r)
ndativarattri=size(this%dativarattr%i)
ndativarattrb=size(this%dativarattr%b)
ndativarattrd=size(this%dativarattr%d)
ndativarattrc=size(this%dativarattr%c)

nanavarr=0
nanavari=0
nanavarb=0
nanavard=0
nanavarc=0
 
if (associated(this%anavar%r)) nanavarr=size(this%anavar%r)
if (associated(this%anavar%i)) nanavari=size(this%anavar%i)
if (associated(this%anavar%b)) nanavarb=size(this%anavar%b)
if (associated(this%anavar%d)) nanavard=size(this%anavar%d)
if (associated(this%anavar%c)) nanavarc=size(this%anavar%c)

nanaattrr=size(this%anaattr%r)
nanaattri=size(this%anaattr%i)
nanaattrb=size(this%anaattr%b)
nanaattrd=size(this%anaattr%d)
nanaattrc=size(this%anaattr%c)

nanavarattrr=size(this%anavarattr%r)
nanavarattri=size(this%anavarattr%i)
nanavarattrb=size(this%anavarattr%b)
nanavarattrd=size(this%anavarattr%d)
nanavarattrc=size(this%anavarattr%c)


!write(unit=lunit)ldescription
!write(unit=lunit)tarray

call check( "1",nf90_def_dim(lunit,"ana", nana, ana_dimid) )
call check( "2",nf90_def_dim(lunit,"ident_len",vol7d_ana_lenident , ident_len_dimid) )

call check( "3",nf90_def_dim(lunit,"time", ntime, time_dimid) )

call check( "4",nf90_def_dim(lunit,"timerange", ntimerange, timerange_dimid) )
call check( "5",nf90_def_dim(lunit,"timerange_vdim", 3, timerange_vdim_dimid) )

call check( "6",nf90_def_dim(lunit,"level", nlevel, level_dimid) )
call check( "7",nf90_def_dim(lunit,"level_vdim", 4, level_vdim_dimid) )

call check( "8",nf90_def_dim(lunit,"network_id", nnetwork, network_dimid) )

call check( "9",nf90_def_dim(lunit,"var_vdim",3, var_vdim_dimid) )
call check( "9",nf90_def_dim(lunit,"var_len",65, var_len_dimid) )


if (nanavarr > 0) call check( "a1",nf90_def_dim(lunit,"anavarr", nanavarr, anavarr_dimid) )
if (nanavari > 0) call check( "a2",nf90_def_dim(lunit,"anavari", nanavari, anavari_dimid) )
if (nanavarb > 0) call check( "a3",nf90_def_dim(lunit,"anavarb", nanavarb, anavarb_dimid) )
if (nanavard > 0) call check( "a4",nf90_def_dim(lunit,"anavard", nanavard, anavard_dimid) )
if (nanavarc > 0) call check( "a5",nf90_def_dim(lunit,"anavarc", nanavarc, anavarc_dimid) )
call check( "a6",nf90_def_dim(lunit,"anavarc_len",vol7d_cdatalen, anavarc_len_dimid) )


if (ndativarr > 0) call check( "d1",nf90_def_dim(lunit,"dativarr", ndativarr, dativarr_dimid) )
if (ndativari > 0) call check( "d2",nf90_def_dim(lunit,"dativari", ndativari, dativari_dimid) )
if (ndativarb > 0) call check( "d3",nf90_def_dim(lunit,"dativarb", ndativarb, dativarb_dimid) )
if (ndativard > 0) call check( "d4",nf90_def_dim(lunit,"dativard", ndativard, dativard_dimid) )
if (ndativarc > 0) call check( "d5",nf90_def_dim(lunit,"dativarc", ndativarc, dativarc_dimid) )
call check( "d6",nf90_def_dim(lunit,"dativarc_len",vol7d_cdatalen, dativarc_len_dimid) )

! ripetere per datiattr anavar anaattr -- dativarattr anavarattr


call check( "10",nf90_def_var(lunit, "ana_lat", NF90_DOUBLE, ana_dimid, ana_lat_varid) )
call check( "11",nf90_def_var(lunit, "ana_lon", NF90_DOUBLE, ana_dimid, ana_lon_varid) )
call check( "12",nf90_def_var(lunit, "ana_ident", NF90_CHAR, (/ ident_len_dimid,ana_dimid/), ana_ident_varid) )

call check( "13",nf90_def_var(lunit, "time_iminuti", NF90_INT, time_dimid, time_iminuti_varid) )

call check( "14",nf90_def_var(lunit, "timerange_vect", NF90_INT, (/timerange_dimid,timerange_vdim_dimid/), timerange_vect_varid) )
call check( "15",nf90_def_var(lunit, "level_vect", NF90_INT, (/level_dimid, level_vdim_dimid/), level_vect_varid) )
call check( "16",nf90_def_var(lunit, "network_id", NF90_INT, network_dimid, network_id_varid) )


! anagrafica

if (nanavarr > 0)then
   call check( "a81" ,nf90_def_var(lunit,"anavarr",NF90_CHAR,&
        (/var_len_dimid ,var_vdim_dimid,anavarr_dimid/),anavarr_varid ))
   call check( "a82" ,nf90_def_var(lunit,"volanavarr",NF90_REAL,&
        (/ana_dimid,anavarr_dimid,network_dimid/),volanavarr_varid ))
end if

if (nanavari > 0) then
   call check( "a83" ,nf90_def_var(lunit,"anavari",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,anavari_dimid/),anavari_varid ))
   call check( "a84" ,nf90_def_var(lunit,"volanavari",NF90_INT,&
        (/ana_dimid,anavari_dimid,network_dimid/),volanavari_varid ))
end if
if (nanavard > 0) then
   call check( "a85" ,nf90_def_var(lunit,"anavard",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,anavard_dimid/),anavard_varid ))
   call check( "a86" ,nf90_def_var(lunit,"volanavard",NF90_DOUBLE,&
        (/ana_dimid,anavard_dimid,network_dimid/),volanavard_varid ))
end if
if (nanavarb > 0) then
   call check( "a87" ,nf90_def_var(lunit,"anavarb",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,anavarb_dimid/),anavarb_varid ))
   call check( "a88" ,nf90_def_var(lunit,"volanavarb",NF90_BYTE,&
        (/ana_dimid,anavarb_dimid,network_dimid/),volanavarb_varid ))
end if
if (nanavarc > 0) then 
   call check( "a89" ,nf90_def_var(lunit,"anavarc",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,anavarc_dimid/),anavarc_varid ))
   call check( "a90" ,nf90_def_var(lunit,"volanavarc",NF90_CHAR,&
        (/anavarc_len_dimid,ana_dimid,anavarc_dimid,network_dimid/),volanavarc_varid ))
end if



! dati

if (ndativarr > 0) then
   call check( "d81" ,nf90_def_var(lunit,"dativarr",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,dativarr_dimid/),dativarr_varid ))
   call check( "d82" ,nf90_def_var(lunit,"voldativarr",NF90_REAL,&
        (/ana_dimid,time_dimid,level_dimid,timerange_dimid,dativarr_dimid,network_dimid/),voldativarr_varid ))
end if
if (ndativari > 0) then
   call check( "d83" ,nf90_def_var(lunit,"dativari",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,dativari_dimid/),dativari_varid ))
   call check( "d84" ,nf90_def_var(lunit,"voldativari",NF90_INT,&
        (/ana_dimid,time_dimid,level_dimid,timerange_dimid,dativari_dimid,network_dimid/),voldativari_varid ))
end if
if (ndativard > 0) then
   call check( "d85" ,nf90_def_var(lunit,"dativard",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,dativard_dimid/),dativard_varid ))
   call check( "d86" ,nf90_def_var(lunit,"voldativard",NF90_DOUBLE,&
        (/ana_dimid,time_dimid,level_dimid,timerange_dimid,dativard_dimid,network_dimid/),voldativard_varid ))
end if
if (ndativarb > 0) then
   call check( "d87" ,nf90_def_var(lunit,"dativarb",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,dativarb_dimid/),dativarb_varid ))
   call check( "d88" ,nf90_def_var(lunit,"voldativarb",NF90_BYTE,&
        (/ana_dimid,time_dimid,level_dimid,timerange_dimid,dativarb_dimid,network_dimid/),voldativarb_varid ))
end if
if (ndativarc > 0) then
   call check( "d89" ,nf90_def_var(lunit,"dativarc",NF90_CHAR,&
        (/var_len_dimid,var_vdim_dimid ,dativarc_dimid/),dativarc_varid ))
   call check( "d90" ,nf90_def_var(lunit,"voldativarc",NF90_CHAR,&
        (/dativarc_len_dimid,ana_dimid,time_dimid,level_dimid,timerange_dimid,dativarc_dimid,network_dimid/),voldativarc_varid ))
end if

! end definition
call check("22", nf90_enddef(lunit) )


if (associated(this%ana))       call check("23", nf90_put_var(lunit, ana_lat_varid, getlat(this%ana(:)%coord)))
if (associated(this%ana))       call check("24", nf90_put_var(lunit, ana_lon_varid, getlon(this%ana(:)%coord)))

if (associated(this%ana))       call check("25", nf90_put_var(lunit, ana_ident_varid, this%ana(:)%ident))

if (associated(this%time))      call check("26", nf90_put_var(lunit, time_iminuti_varid   , &
 int(timedelta_getamsec(this%time-timeref)/60000)))

if (associated(this%level)) then
  do i=1,nlevel

    call check("27", nf90_put_var(lunit, level_vect_varid,&
     (/this%level(i)%level1,&
     this%level(i)%l1,&
     this%level(i)%level2,&
     this%level(i)%l2/),&
     start=(/i,1/),count=(/1,4/)))
  end do
end if

if (associated(this%timerange)) then
  do i=1,ntimerange
    call check( "28",nf90_put_var(lunit, timerange_vect_varid,&
     (/this%timerange(i)%timerange,&
     this%timerange(i)%p1,&
     this%timerange(i)%p2/),&
     start=(/i,1/),count=(/1,3/)))
  end do
end if

if (associated(this%network)) then
    call check( "29",nf90_put_var(lunit, network_id_varid,this%network(:)%id))
end if


! ana

do i=1,nanavarr
   if (associated(this%anavar%r)) then
      call check( "a291",nf90_put_var(lunit, anavarr_varid,&
           (/this%anavar%r(i)%description,this%anavar%r(i)%unit,this%anavar%r(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do

do i=1,nanavari
   if (associated(this%anavar%i)) then
      call check( "a292",nf90_put_var(lunit, anavari_varid,&
           (/this%anavar%i(i)%description,this%anavar%i(i)%unit,this%anavar%i(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do

do i=1,nanavard
   if (associated(this%anavar%d)) then
      call check( "a293",nf90_put_var(lunit, anavard_varid,&
           (/this%anavar%d(i)%description,this%anavar%d(i)%unit,this%anavar%d(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do

do i=1,nanavarb
   if (associated(this%anavar%b)) then
      call check( "a294",nf90_put_var(lunit, anavarb_varid,&
           (/this%anavar%b(i)%description,this%anavar%b(i)%unit,this%anavar%b(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do

do i=1,nanavarc
   if (associated(this%anavar%c)) then
      call check( "a295",nf90_put_var(lunit, anavarc_varid,&
           (/this%anavar%c(i)%description,this%anavar%c(i)%unit,this%anavar%c(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do



! dati

do i=1,ndativarr
   if (associated(this%dativar%r)) then
      call check( "a291",nf90_put_var(lunit, dativarr_varid,&
           (/this%dativar%r(i)%description,this%dativar%r(i)%unit,this%dativar%r(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do

do i=1,ndativari
   if (associated(this%dativar%i)) then
      call check( "a292",nf90_put_var(lunit, dativari_varid,&
           (/this%dativar%i(i)%description,this%dativar%i(i)%unit,this%dativar%i(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do

do i=1,ndativard
   if (associated(this%dativar%d)) then
      call check( "a293",nf90_put_var(lunit, dativard_varid,&
           (/this%dativar%d(i)%description,this%dativar%d(i)%unit,this%dativar%d(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do

do i=1,ndativarb
   if (associated(this%dativar%b)) then
      call check( "a294",nf90_put_var(lunit, dativarb_varid,&
           (/this%dativar%b(i)%description,this%dativar%b(i)%unit,this%dativar%b(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do

do i=1,ndativarc
   if (associated(this%dativar%c)) then
      call check( "a295",nf90_put_var(lunit, dativarc_varid,&
           (/this%dativar%c(i)%description,this%dativar%c(i)%unit,this%dativar%c(i)%btable/)&
           ,start=(/1,1,i/),count=(/65,3,1/)))
   end if
end do


!!$if (associated(this%volanar))      write(unit=lunit)this%volanar
if (associated(this%volanar))call check("a231", nf90_put_var(lunit,volanavarr_varid,this%volanar))
if (associated(this%volanai))call check("a232", nf90_put_var(lunit,volanavari_varid,this%volanai))
if (associated(this%volanad))call check("a233", nf90_put_var(lunit,volanavard_varid,this%volanad))
if (associated(this%volanab))call check("a234", nf90_put_var(lunit,volanavarb_varid,this%volanab))
if (associated(this%volanac))call check("a235", nf90_put_var(lunit,volanavarc_varid,this%volanac))


if (associated(this%voldatir))call check("d231", nf90_put_var(lunit,voldativarr_varid,this%voldatir))
if (associated(this%voldatii))call check("d232", nf90_put_var(lunit,voldativari_varid,this%voldatii))
if (associated(this%voldatid))call check("d233", nf90_put_var(lunit,voldativard_varid,this%voldatid))
if (associated(this%voldatib))call check("d234", nf90_put_var(lunit,voldativarb_varid,this%voldatib))
if (associated(this%voldatic))call check("d235", nf90_put_var(lunit,voldativarc_varid,this%voldatic))

if (.not. present(ncunit)) call check("90", nf90_close(lunit) )

end subroutine vol7d_netcdf_export



subroutine vol7d_netcdf_export_CF (this,ncconventions,ncunit,description,filename)

TYPE(vol7d),INTENT(IN) :: this !< volume vol7d da scrivere 
integer,optional,intent(inout) :: ncunit !< unit� netcdf su cui scrivere; se passata =0 ritorna il valore rielaborato (default =elaborato internamente da netcdf )
character(len=*),intent(in) :: ncconventions !< tipo di convenzione da utilizzare nella scrittura netcdf (per ora supportato "CF-1.1 vol7d")
character(len=*),intent(inout),optional :: filename !< nome del file su cui scrivere; se passato ="" ritorna il valore rielaborato
character(len=*),INTENT(IN),optional :: description !< descrizione del volume

integer :: lunit
character(len=254) :: ldescription,arg,lfilename
integer :: nana, ntime, ntimerange, nlevel, nnetwork, &
 ndativarr, ndativari, ndativarb, ndativard, ndativarc,&
 ndatiattrr, ndatiattri, ndatiattrb, ndatiattrd, ndatiattrc,&
 ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc,&
 nanavarr, nanavari, nanavarb, nanavard, nanavarc,&
 nanaattrr, nanaattri, nanaattrb, nanaattrd, nanaattrc,&
 nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc
!integer :: im,id,iy
integer :: tarray(8)
logical :: opened,exist

integer :: ana_ident_varid,ana_dimid,ana_lon_varid,ana_lat_varid &
 ,ident_len_dimid,var_len_dimid &
 ,level_dimid,level_vdim_dimid,level_vect_varid,network_dimid,network_id_varid,timerange_vdim_dimid &
 ,time_iminuti_varid,time_dimid,timerange_dimid,timerange_vect_varid &
 ,dativard_dimid,dativarr_dimid,dativari_dimid,dativarb_dimid,dativarc_len_dimid,dativarc_dimid &
 ,dativarr_varid ,dativari_varid ,dativard_varid ,dativarb_varid ,dativarc_varid &
 ,anavard_dimid,anavarr_dimid,anavari_dimid,anavarb_dimid,anavarc_len_dimid,anavarc_dimid &
 ,anavarr_varid ,anavari_varid ,anavard_varid ,anavarb_varid ,anavarc_varid &
 ,anavarr_btable_varid,anavari_btable_varid,anavard_btable_varid,anavarb_btable_varid,anavarc_btable_varid &
 ,dativarr_btable_varid,dativari_btable_varid,dativard_btable_varid,dativarb_btable_varid,dativarc_btable_varid


integer :: i

type(datetime) :: timeref 

if (ncconventions /= "CF-1.1") then
  print *,"ncconventions not supported: ", ncconventions
  call exit(1)
end if


end subroutine vol7d_netcdf_export_CF



subroutine check(stringa,status)
integer, intent ( in) :: status
character (len=*) :: stringa

if(status /= nf90_noerr) then
  print *, stringa
  print *, trim(nf90_strerror(status))
  stop "Stopped"
end if
end subroutine check


end MODULE vol7d_netcdf_class
