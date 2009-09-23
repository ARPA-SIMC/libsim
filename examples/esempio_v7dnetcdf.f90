program demo2

!legge i dati da file formattato

USE vol7d_netcdf_class
USE vol7d_dballe_class
USE vol7d_class
use log4fortran

IMPLICIT NONE

integer :: category,ier
character(len=512):: a_name

integer :: nana, ntime ,nlevel, ntimerange, ndativarr, nnetwork ,nanavarc
integer :: iana, itime, ilevel, itimerange, idativarr, inetwork
TYPE(vol7d) :: v7d
TYPE(datetime) :: time
TYPE(vol7d_level) :: level(10)
TYPE(vol7d_timerange) :: timerange(3)
TYPE(vol7d_network) :: network
TYPE(vol7d_var) ::  dativar,anavar
CHARACTER(len=vol7d_ana_lenident) :: ident
REAL(kind=fp_geo) :: lat,lon
integer :: year,month,day,hour,minute,ist
real :: prec,temp

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo")

!imposta a_name
category=l4f_category_get(a_name)


!init di log4fortran
ier=l4f_init()

CALL init(v7d)

!open (unit=10,file="in.dat")
read(10,*)nana,ntime

ntimerange=2
nlevel=2
nnetwork=1
ndativarr=2
nanavarc=1

call vol7d_alloc (v7d, &
 nana=nana, ntime=ntime, ntimerange=ntimerange, &
 nlevel=nlevel, nnetwork=nnetwork, &
 ndativarr=ndativarr,nanavarc=nanavarc)

call vol7d_alloc_vol (v7d)

inetwork=1
call init(v7d%network(inetwork), 'synop')
call init(v7d%anavar%c(1), btable="B01019")     ! LONG STATION OR SITE NAME
call init(v7d%dativar%r(1), btable="B13011")    ! precipitazione
call init(v7d%dativar%r(2), btable="B12001")    ! temperatura
call init(v7d%level(1),1,0)                     ! al suolo
call init(v7d%level(2),103,2)                   ! a 2 m dal suolo
call init(v7d%timerange(1),1,0, 3600)           ! cumulate n 1 ora
call init(v7d%timerange(2),254,    0, 0)        ! "istantanee"

v7d%volanac(1,1,1)="stazione 1 "
v7d%volanac(2,1,1)="stazione 2 "

call l4f_category_log(category,L4F_INFO,"demo scrittura netcdf da vol7d")


do ist=1,nana
  do itime=1,ntime
    
    read (10,*) iana,lat,lon,year,month,day,hour,minute,prec,temp
    if (iana /= ist) call l4f_category_log(category,L4F_ERROR,&
         "abbiamo un serio problema "//to_char(iana)//to_char(ist))

    call init(v7d%ana(iana),lat=lat,lon=lon)
    call init(v7d%time(itime), year=year, month=month, day=day, hour=hour, minute=minute)
    
    itimerange=1     
    ilevel=1
    idativarr= 1
    v7d%voldatir(iana,itime,ilevel,itimerange,idativarr,inetwork) = prec
    
    itimerange=2     
    ilevel=2
    idativarr= 2
    v7d%voldatir(iana,itime,ilevel,itimerange,idativarr,inetwork) = temp
    
  end do
end do

call vol7d_dballe_set_var_du(v7d)


call export(v7d,ncconventions="CF-1.1 vol7d")

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo2
