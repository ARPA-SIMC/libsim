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
!> \brief Costanti fisiche (SINGOLA PRECISIONE).
!!
!! Questo modulo definisce delle costanti fisiche di uso comune
!! in meteorologia e discipline affini. Attualmente esse sono definite
!! solo in singola precisione, sarebbe opportuno trovare un metodo
!! elegante per definirle semiautomaticamente anche in doppia precisione
!! affinché possano essere usate, ad esempio, dalle routine di
!! conversione geo-UTM della classe geo_coord_class.
!! \ingroup base
MODULE phys_const
IMPLICIT NONE

! pi = 4.*ATAN(1.)
REAL, PARAMETER :: pi =3.141593 !< pi greco (\f$\pi\f$)
REAL, PARAMETER :: rearth = 6370997. !< raggio medio della Terra (\f$m\f$)
REAL, PARAMETER :: gearth = 9.80665 !< accelerazione gravitazionale media alla superficie terrestre (\f$m/s^2\f$)
REAL, PARAMETER :: omearth = 2.*pi/86164. !< velocità angolare terrestre (\f$s^{-1}\f$)
REAL, PARAMETER :: degrad = pi/180. !< fattore di conversione da gradi a radianti
REAL, PARAMETER :: raddeg = 180./pi !< fattore di conversione da radianti a gradi
REAL, parameter :: t0c = 273.15  !< 0 gradi celsius in kelvin
REAL, PARAMETER :: mwater = 18.0153 !< molar mass of water (mol)
real,parameter  :: rgas = 8314.472 !< universal gas constant R 
real,parameter  :: mdry = 28.9644 !< molar mass of dry air 
! rd= 287.05831986852826 ma si trova rd=287.05
REAL, PARAMETER :: rd =  rgas/mdry  !< costante dei gas per l'aria secca
! rv= 461.522816717 ma in vecchi testi   8314.3/18.016=461.495337478
REAL, PARAMETER :: rv = rgas/mwater !< costante dei gas per il vapore acqueo (\f$JK^{-1}Kg^{-1}\f$)
REAL, PARAMETER :: eps0 = rd/rv !< \f$rd/rv\f$
REAL, PARAMETER :: epsy = rv/rd-1. !< \f$rv/rd -1\f$
REAL, PARAMETER :: rcp = 2./7. !< rapporto tra calore specifico...?
REAL, PARAMETER :: cpd = rd/rcp !< calore specifico dell'aria secca a pressione costante
REAL, PARAMETER :: cvd = cpd-rd !< calore specifico dell'aria secca a volume costante
REAL, PARAMETER :: lvw = 2.5E+6 !< calore latente...?
real,parameter  :: convff = 1.94  !< fattore di conversione da m/s a KT
! c1 era parameter(c1=0.378)
real,parameter  :: c1 = 1.- mwater/mdry  !< 1. - ( mwater/mdry ) 


END MODULE phys_const

!> \brief Costanti fisiche (DOUBLEPRECISION).
!!
!! Questo modulo definisce delle costanti fisiche di uso comune
!! in meteorologia e discipline affini. Attualmente esse sono definite
!! solo in singola precisione, sarebbe opportuno trovare un metodo
!! elegante per definirle semiautomaticamente anche in doppia precisione
!! affinché possano essere usate, ad esempio, dalle routine di
!! conversione geo-UTM della classe geo_coord_class.
!! \ingroup base
MODULE doubleprecision_phys_const
IMPLICIT NONE

! pi = 4.*ATAN(1.)
DOUBLEPRECISION, PARAMETER :: pi = 3.1415926535897932D0 !< pi greco (\f$\pi\f$)

DOUBLEPRECISION, PARAMETER :: rearth = 6370997.0D0 !< raggio medio della Terra (\f$m\f$)
DOUBLEPRECISION, PARAMETER :: gearth = 9.80665D0 !< accelerazione gravitazionale media alla superficie terrestre (\f$m/s^2\f$)
DOUBLEPRECISION, PARAMETER :: omearth = 2.0D0*pi/86164.0D0 !< velocità angolare terrestre (\f$s^{-1}\f$)
DOUBLEPRECISION, PARAMETER :: degrad = pi/180.0D0 !< fattore di conversione da gradi a radianti
DOUBLEPRECISION, PARAMETER :: raddeg = 180.0D0/pi !< fattore di conversione da radianti a gradi
DOUBLEPRECISION, parameter :: t0c = 273.15D0  !< 0 gradi celsius in kelvin
DOUBLEPRECISION, PARAMETER :: mwater =  18.0153D0 !< molar mass of water (mol)
DOUBLEPRECISION,parameter  :: rgas = 8314.472D0 !< universal gas constant R 
DOUBLEPRECISION,parameter  :: mdry = 28.9644D0 !< molar mass of dry air 
! rd= 287.05831986852826 ma si trova rd=287.05
DOUBLEPRECISION, PARAMETER :: rd =  rgas/mdry  !< costante dei gas per l'aria secca
! rv= 461.522816717 ma in vecchi testi   8314.3/18.016=461.495337478
DOUBLEPRECISION, PARAMETER :: rv = rgas/mwater !< costante dei gas per il vapore acqueo (\f$JK^{-1}Kg^{-1}\f$)
DOUBLEPRECISION, PARAMETER :: eps0 = rd/rv !< \f$rd/rv\f$
DOUBLEPRECISION, PARAMETER :: epsy = rv/rd-1.0D0 !< \f$rv/rd -1\f$
DOUBLEPRECISION, PARAMETER :: rcp = 2.0D0/7.0D0 !< rapporto tra calore specifico...?
DOUBLEPRECISION, PARAMETER :: cpd = rd/rcp !< calore specifico dell'aria secca a pressione costante
DOUBLEPRECISION, PARAMETER :: cvd = cpd-rd !< calore specifico dell'aria secca a volume costante
DOUBLEPRECISION, PARAMETER :: lvw = 2.5D+6 !< calore latente...?
DOUBLEPRECISION,parameter  :: convff = 1.94D0  !< fattore di conversione da KT a m/s
! c1 era parameter(c1=0.378)
DOUBLEPRECISION,parameter  :: c1 = 1.0D0- mwater/mdry  !< 1. - ( mwater/mdry ) 


END MODULE doubleprecision_phys_const

