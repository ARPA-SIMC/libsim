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
PROGRAM v7ddballe_import_export
! Programma di esempio di estrazione e scrittura dall'archivio DB-all.e
USE datetime_class
USE vol7d_dballe_class
USE vol7d_class

IMPLICIT NONE

TYPE(vol7d_dballe) :: v7d,v7d_exp
TYPE(datetime) :: ti, tf
integer :: sh(5)

TYPE(vol7d_ana) :: ana
TYPE(datetime) :: time
TYPE(vol7d_level) :: level
TYPE(vol7d_timerange) :: timerange
TYPE(vol7d_network) :: network
TYPE(vol7d_var) ::  dativar,attrvar
CHARACTER(len=vol7d_ana_lenident) :: ident
REAL(kind=fp_geo) :: lat,lon

integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork
integer :: inddatiattr,inddativarattr


! Definisco le date iniziale e finale
CALL init(ti, year=2007, month=3, day=18, hour=12)
CALL init(tf, year=2007, month=3, day=21, hour=00)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_exp,dsn="test",user="test",write=.true.,wipe=.false.)

! Importo i dati
!  Esempi:
!CALL import(v7d,(/"B13011","B12001"/), 255, ti, tf, timerange=vol7d_timerange(4,-1800,0), attr=(/"*B33192","*B33007"/))
!CALL import(v7d,(/"B13011","B12001"/), 255, ti, tf,  attr=(/"*B33192","*B33007"/))
!CALL import(v7d)
!CALL import(v7d,var=(/"B13003","B13011","B12001"/))
!CALL import(v7d,var=(/"B13003","B13011","B12001"/),varkind=(/"d","r","r"/), network=255, timei=ti, timef=tf&
! ,attr=(/"*B33192","*B33007"/),attrkind=(/"i","b"/))
! ,attr=(/"*B33192","*B33007"/))

CALL import(v7d,var=(/"B12001"/),varkind=(/"r"/),attr=(/"*B33195","*B33192"/),attrkind=(/"i","b"/))

Print *,"Fine estrazione dati"

!call vol7d_copy(v7d%vol7d,v7d_exp%vol7d)
!call vol7d_diff_only(v7d%vol7d,v7d_exp%vol7d,data_only=.true.)
!CALL delete (v7d) 

sh=shape(v7d%data_id)
print *,sh
allocate (v7d_exp%data_id(sh(1),sh(2),sh(3),sh(4),sh(5)))
v7d_exp%data_id=imiss

v7d_exp%vol7d   =  v7d%vol7d
!v7d_exp%data_id => v7d%data_id


lat=45.8875D0
lon=8.51111D0
ident=cmiss

call init(ana,lat=lat,lon=lon,ident=ident)
call init(time, year=2007, month=3, day=18, hour=00,minute=30)
call init(level, 105,2,0)
call init(timerange, 0, 0, 0)
call init(network, 'rete50')
call init(dativar,"B12001" )
call init(attrvar,"*B33192" )


indana          = firsttrue(ana       == v7d%vol7d%ana)
indtime         = firsttrue(time      == v7d%vol7d%time)
indtimerange    = firsttrue(timerange == v7d%vol7d%timerange)
indlevel        = firsttrue(level     == v7d%vol7d%level)
indnetwork      = firsttrue(network   == v7d%vol7d%network)
inddativar      = firsttrue(dativar   == v7d%vol7d%dativar%r)
inddativarattr  = firsttrue(dativar   == v7d%vol7d%dativarattr%b)
inddatiattr     = firsttrue(attrvar   == v7d%vol7d%datiattr%b)

print *,"modifico questo dato ", indana,indtime,indlevel,indtimerange,indnetwork

v7d_exp%vol7d%voldatiattrb(indana,indtime,indlevel,indtimerange,&
 inddativarattr,indnetwork,inddatiattr) = 96


v7d_exp%data_id(indana,indtime,indlevel,indtimerange,indnetwork)=&
 v7d%data_id(indana,indtime,indlevel,indtimerange,indnetwork)

Print *,"Scrivo i dati"

!CALL export(v7d_exp)
CALL export(v7d_exp,attr_only=.true.)

CALL delete (v7d_exp) 

END PROGRAM v7ddballe_import_export
