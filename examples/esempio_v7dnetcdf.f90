program demo2

!legge i dati da file formattato

USE vol7d_netcdf_class
USE vol7d_class

IMPLICIT NONE

integer :: nana, ntime ,nlevel, ntimerange, ndativarr, nnetwork !,nanavari
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


CALL init(v7d)

read(10,*)nana,ntime

ntimerange=2
nlevel=2
nnetwork=1
ndativarr=2

call vol7d_alloc (v7d, &
 nana=nana, ntime=ntime, ntimerange=ntimerange, &
 nlevel=nlevel, nnetwork=nnetwork, &
 ndativarr=ndativarr)

call vol7d_alloc_vol (v7d)

inetwork=1
call init(v7d%network(inetwork), 11)
call init(v7d%dativar%r(1), btable="B13011")    ! precipitazione
call init(v7d%dativar%r(2), btable="B12001")    ! temperatura
call init(v7d%level(1),1,0,0)                   ! al suolo
call init(v7d%level(2),105,2,0)                 ! a 2 m dal suolo
call init(v7d%timerange(1),1,0, 3600)          ! cumulate n 1 ora
call init(v7d%timerange(2),254,    0, 0)          ! "istantanee"


do ist=1,nana
  do itime=1,ntime
    
    read (10,*) iana,lat,lon,year,month,day,hour,minute,prec,temp
    if (iana /= ist) print *,"abbiamo un serio problema ",iana,ist

    call init(v7d%ana(iana),lat=lat,lon=lon)
    call init(v7d%time(itime), year=year, month=month, day=day, hour=hour, minute=minute)
    
    itimerange=1     
    ilevel=1
    idativarr= 1
    v7d%voldatir(iana,itime,ilevel,itimerange,idativarr,inetwork) = prec
    
    itimerange=1     
    ilevel=2
    idativarr= 2
    v7d%voldatir(iana,itime,ilevel,itimerange,idativarr,inetwork) = temp
    
  end do
end do


call export(v7d,ncconventions="CF-1.1 vol7d")


end program demo2
