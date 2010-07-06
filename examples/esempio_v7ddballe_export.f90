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
PROGRAM v7ddballe_export
! Programma di esempio di lettura da file e scrittura nell'archivio DB-all.e

USE vol7d_dballe_class
USE vol7d_class

IMPLICIT NONE

TYPE(vol7d) :: v7d
TYPE(vol7d_dballe) :: v7d_exp

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


! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_exp,dsn="test3",user="test",write=.true.,wipe=.true.)

CALL import(v7d,filename="lt-esempio_v7ddballe_multi.v7d")

Print *,"Fine lettura dati"

v7d_exp%vol7d = v7d

Print *,"Scrivo i dati"

CALL export(v7d_exp)

CALL delete (v7d_exp) 
!CALL delete (v7d) 

END PROGRAM v7ddballe_export
