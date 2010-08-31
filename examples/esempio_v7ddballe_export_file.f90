! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
PROGRAM v7ddballe_export_file
! Programma di esempio di lettura da file e scrittura su file formato BUFR
!legge i dati da file formattato

USE vol7d_class
USE vol7d_netcdf_class
USE vol7d_dballe_class
use log4fortran

IMPLICIT NONE

integer :: category,ier
character(len=512):: a_name

integer :: nana, ntime ,nlevel, ntimerange, ndativarr, nnetwork ,nanavarc
integer :: iana, itime, ilevel, itimerange, idativarr, inetwork
TYPE(vol7d) :: v7d
TYPE(vol7d_dballe) :: v7d_exp
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
call l4f_launcher(a_name,a_name_force="demo",a_name_append="main")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")


call l4f_category_log(category,L4F_INFO,"demo scrittura BUFR da vol7d")

call l4f_category_log(category,L4F_INFO,"inizializzazioni")


CALL init(v7d)

!open (unit=10,file="in.dat")
read(10,*)nana,ntime

ntimerange=2
nlevel=3
nnetwork=1
ndativarr=4
nanavarc=1

call l4f_category_log(category,L4F_INFO,"numero stazioni: "//to_char(nana))
call l4f_category_log(category,L4F_INFO,"numero time: "//to_char(ntime))

call vol7d_alloc (v7d, &
 nana=nana, ntime=ntime, ntimerange=ntimerange, &
 nlevel=nlevel, nnetwork=nnetwork, &
 ndativarr=ndativarr,nanavarc=nanavarc)

call vol7d_alloc_vol (v7d)

inetwork=1
call init(v7d%network(inetwork), 'generic')

call init(v7d%anavar%c(1), btable="B01019")     ! LONG STATION OR SITE NAME
call init(v7d%dativar%r(1), btable="B13011")    ! precipitazione
call init(v7d%dativar%r(2), btable="B12101")    ! temperatura
call init(v7d%dativar%r(3), btable="B11001")    ! WIND DIRECTION 
call init(v7d%dativar%r(4), btable="B11002")    ! WIND SPEED

call init(v7d%level(1),1,0)                     ! al suolo
call init(v7d%level(2),103,2000)                ! a 2 m dal suolo
call init(v7d%level(3),103,10000)               ! 10m

call init(v7d%timerange(1),1,0, 3600)           ! cumulate n 1 ore
call init(v7d%timerange(2),254,    0, 0)        ! "istantanee"

v7d%volanac(1,1,1)="stazione 1 "
v7d%volanac(2,1,1)="stazione 2 "

call l4f_category_log(category,L4F_INFO,"leggo i dati")

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

    itimerange=2
    ilevel=3
    idativarr= 3
    v7d%voldatir(iana,itime,ilevel,itimerange,idativarr,inetwork) = 180.

    itimerange=2
    ilevel=3
    idativarr= 4
    v7d%voldatir(iana,itime,ilevel,itimerange,idativarr,inetwork) = 5.5

   
  end do
end do


! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_exp,file=.true.,write=.true.,wipe=.true.,categoryappend="exportBUFR",format="BUFR")

v7d_exp%vol7d = v7d

call l4f_category_log(category,L4F_INFO,"Scrivo i dati")

!CALL export(v7d_exp,template="synop")
CALL export(v7d_exp)

CALL delete (v7d_exp) 

call l4f_category_log(category,L4F_INFO,"finito")


!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program v7ddballe_export_file
