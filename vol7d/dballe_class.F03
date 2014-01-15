! Copyright (C) 2013  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Paolo Patruno <ppatruno@arpa.emr.it>
! Davide Cesari <dcesari@arpa.emr.it>

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

#include "config.h"

!> \brief  classe per import ed export di volumi da e in DB-All.e 
!!
!!Questo modulo definisce gli oggetti e i metodi per gestire
!!l'importazione e l'esportazione di volumi dal database per dati sparsi
!!DB-All.e
!!
!!Programma esempio
!!\include example_dballe.F03
!!
!!\ingroup vol7d

MODULE dballe_class

use optional_values
use log4fortran
use err_handling
use missing_values
use vol7d_level_class
use vol7d_timerange_class
use geo_coord_class
use vol7d_ana_class
use vol7d_network_class
use datetime_class

!USE char_utilities

IMPLICIT NONE

include "dballeff.h"
private

character (len=255),parameter:: subcategory="dballe_class"

type,public :: dbaconnection
  integer :: dbhandle=imiss
  integer :: handle_err=imiss
  integer :: category !< log4fortran
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbaconnection_delete
# else
    procedure :: delete => dbaconnection_delete
# endif
end type dbaconnection

interface dbaconnection
  procedure  dbaconnection_init      ! add constructor to shape generic interface
end interface

type,public ::  dbasession
  integer :: sehandle=imiss
  logical :: file=.false.
  integer :: category !< log4fortran
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbasession_delete
# else
    procedure :: delete => dbasession_delete
# endif
    procedure :: unsetall => dbasession_unsetall
    procedure :: set => dbasession_set
    procedure :: setcontextana => dbasession_setcontextana
    procedure :: ingest => dbasession_ingest
    procedure :: prendilo => dbasession_prendilo
    procedure :: var_related => dbasession_var_related
    procedure :: critica => dbasession_critica
!  procedure :: dammelo => dbasession_dammelo
end type dbasession

interface dbasession
  procedure  dbasession_init      ! add constructor to shape generic interface
end interface


type,public,extends(vol7d_level) :: dbalevel
  contains

# ifdef F2003_FULL_FEATURES
!    final :: dbalevel_delete
# else
!    procedure :: delete => dbalevel_delete
# endif
  procedure :: display => dbalevel_display
  procedure :: dbaset => dbalevel_set
  procedure,nopass :: dbacontextana => dbalevel_contextana
!!$procedure :: spiega => dbalevel_spiega
end type dbalevel

interface dbalevel
  procedure   dbalevel_init     ! add constructor to shape generic interface
end interface

type,public,extends(vol7d_timerange) :: dbatimerange
  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbatimerange_delete
# else
!    procedure :: delete => dbatimerange_delete
# endif
  procedure :: display => dbatimerange_display
  procedure :: dbaset => dbatimerange_set
  procedure,nopass :: dbacontextana => dbatimerange_contextana
!!$procedure :: spiega => dbatimerange_spiega
end type dbatimerange

interface dbatimerange
  procedure   dbatimerange_init     ! add constructor to shape generic interface
end interface



type,public,extends(geo_coord) :: dbacoord

!!$  REAL(kind=fp_geo) :: lon !< longitudine
!!$  REAL(kind=fp_geo) :: lat !< latitudine
!!$  INTEGER(kind=int_l) :: ilon !< integer longitude (nint(lon*1.d5)
!!$  INTEGER(kind=int_l) :: ilat !< integer latitude (nint(lat*1.d5)

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbacoord_delete
# else
!    procedure :: delete => dbacoord_delete
# endif
!  procedure :: display => dbacoord_display

end type dbacoord

interface dbacoord
  procedure   dbacoord_init     ! add constructor to shape generic interface
end interface


type,public,extends(vol7d_ana) :: dbaana

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbaana_delete
# else
!    procedure :: delete => dbaana_delete
# endif
  procedure :: display => dbaana_display
  procedure :: dbaset => dbaana_set

end type dbaana

interface dbaana
  procedure   dbaana_init     ! add constructor to shape generic interface
end interface

type,public,extends(vol7d_network) :: dbanetwork

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbanetwork_delete
# else
!    procedure :: delete => dbanetwork_delete
# endif
  procedure :: display => dbanetwork_display
  procedure :: dbaset => dbanetwork_set

end type dbanetwork

interface dbanetwork
  procedure   dbanetwork_init     ! add constructor to shape generic interface
end interface


type,public,extends(datetime) :: dbadatetime

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbanetwork_delete
# else
!    procedure :: delete => dbanetwork_delete
# endif
  procedure :: display => dbadatetime_display
  procedure :: dbaset => dbadatetime_set
  procedure,nopass :: dbacontextana => dbadatetime_contextana
end type dbadatetime

interface dbadatetime
  procedure   dbadatetime_init     ! add constructor to shape generic interface
end interface


type,public,abstract :: dbadata
  character(len=9) :: btable
contains
  procedure(dbadata_set),deferred :: dbaset
  procedure(dbadata_display),deferred :: display
end type dbadata

interface
subroutine dbadata_set(data,session)
import
class(dbadata), intent(in) :: data
type(dbasession), intent(in) :: session
end subroutine dbadata_set

subroutine dbadata_display(data)
import
class(dbadata), intent(in) :: data
end subroutine dbadata_display

end interface

type,public, extends(dbadata) :: dbadatai
  integer :: value
contains
  procedure :: dbaset => dbadatai_set
  procedure :: display => dbadatai_display
end type dbadatai

interface dbadatai
  procedure ::  dbadatai_init     ! add constructor to shape generic interface
end interface dbadatai

type,public, extends(dbadata) :: dbadatar
  real :: value
contains
  procedure :: dbaset => dbadatar_set
  procedure :: display => dbadatar_display
end type dbadatar

interface dbadatar
  procedure ::  dbadatar_init     ! add constructor to shape generic interface
end interface dbadatar


type,public, extends(dbadata) :: dbadatad
  doubleprecision :: value
contains
  procedure :: dbaset => dbadatad_set
  procedure :: display => dbadatad_display
end type dbadatad

interface dbadatad
  procedure ::  dbadatad_init     ! add constructor to shape generic interface
end interface dbadatad


type,public, extends(dbadata) :: dbadatab
  integer(kind=int_b) :: value
contains
  procedure :: dbaset => dbadatab_set
  procedure :: display => dbadatab_display
end type dbadatab

interface dbadatab
  procedure ::  dbadatab_init     ! add constructor to shape generic interface
end interface dbadatab


type,public, extends(dbadata) :: dbadatac
!  character(:) :: value
  character(255) :: value
contains
  procedure :: dbaset => dbadatac_set
  procedure :: display => dbadatac_display
end type dbadatac

interface dbadatac
  procedure ::  dbadatac_init     ! add constructor to shape generic interface
end interface dbadatac


type,public :: dbametadata
  type(dbalevel) :: level
  type(dbatimerange) :: timerange
  type(dbaana) :: ana
  type(dbanetwork) :: network
  type(dbadatetime) :: datetime
  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbametadata_delete
# else
!    procedure :: delete => dbametadata_delete
# endif
  procedure :: dbaset => dbametadata_set
  procedure :: dbacontextana => dbametadata_contextana
  procedure :: display => dbametadata_display
end type dbametadata

interface dbametadata
  procedure   dbametadata_init     ! add constructor to shape generic interface
end interface

type, public :: dbadat
class(dbadata),allocatable :: data
contains
procedure :: display => dbadat_display
procedure :: dbaset => dbadat_set
end type dbadat

type, public :: dbadatav
type(dbadat),allocatable :: dat(:)
contains
procedure :: display => dbadatav_display
procedure :: dbaset => dbadatav_set
end type dbadatav

type, public ,extends(dbadat):: dbadataattr
type(dbadatav) :: attrv
contains
procedure :: display => dbadataattr_display
procedure :: dbaingest => dbadataattr_ingest
end type dbadataattr

type, public :: dbadataattrv
class(dbadataattr),allocatable :: dataattr(:)
contains
procedure :: display => dbadataattrv_display
procedure :: dbaingest => dbadataattrv_ingest
end type dbadataattrv

type, public :: dbametaanddata
type(dbametadata) :: metadata
type(dbadataattrv) ::dataattrv
contains
procedure :: display => dbametaanddata_display
procedure :: dbaingest => dbametaanddata_ingest
end type dbametaanddata

contains

subroutine dbalevel_display(level)
class(dbalevel), intent(in) :: level
call display (level%vol7d_level)
end subroutine dbalevel_display

type(dbalevel) function dbalevel_init(level1, l1, level2, l2)

INTEGER,INTENT(IN),OPTIONAL :: level1 !< tipo di livello 1
INTEGER,INTENT(IN),OPTIONAL :: l1 !< valore per il primo livello
INTEGER,INTENT(IN),OPTIONAL :: level2 !< tipo di livello 2
INTEGER,INTENT(IN),OPTIONAL :: l2 !< valore per il secondo livello

call init (dbalevel_init%vol7d_level,level1, l1, level2, l2)
end function dbalevel_init

subroutine dbalevel_set(level,session)
class(dbalevel), intent(in) :: level
type(dbasession), intent(in) :: session
integer :: ier

!if (c_e(session%sehandle)) then
ier = idba_setlevel(session%sehandle,&
 level%level1, level%l1, level%level2, level%l2)

end subroutine dbalevel_set

type(dbalevel) function dbalevel_contextana()

dbalevel_contextana=dbalevel()

end function dbalevel_contextana


subroutine dbaana_display(ana)
class(dbaana), intent(in) :: ana
call display (ana%vol7d_ana)
end subroutine dbaana_display


type(dbacoord) function dbacoord_init(lon, lat, ilon, ilat)
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lon !< longitudine
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lat !< latitudine
INTEGER(kind=int_l),INTENT(in),OPTIONAL :: ilon !< integer longitude (nint(lon*1.d5)
INTEGER(kind=int_l),INTENT(in),OPTIONAL :: ilat !< integer latitude (nint(lat*1.d5)

CALL init(dbacoord_init%geo_coord, lon=lon, lat=lat , ilon=ilon, ilat=ilat)

end function dbacoord_init

type(dbaana) function dbaana_init(coord,ident,lon, lat, ilon, ilat)
CHARACTER(len=*),INTENT(in),OPTIONAL :: ident !< identificativo del volo
TYPE(dbacoord),INTENT(IN),optional :: coord
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lon !< longitudine
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lat !< latitudine
INTEGER(kind=int_l),INTENT(in),OPTIONAL :: ilon !< integer longitude (nint(lon*1.d5)
INTEGER(kind=int_l),INTENT(in),OPTIONAL :: ilat !< integer latitude (nint(lat*1.d5)

if (present(coord))then
  CALL init(dbaana_init%vol7d_ana, ilon=getilon(coord%geo_coord), ilat=getilat(coord%geo_coord), ident=ident)
else 
  CALL init(dbaana_init%vol7d_ana, lon=lon, lat=lat, ilon=ilon, ilat=ilat, ident=ident)
end if

end function dbaana_init

subroutine dbaana_set(ana,session)
class(dbaana), intent(in) :: ana
type(dbasession), intent(in) :: session
integer :: ier

print*,"set lat and lon"

!if (c_e(session%sehandle)) then
ier = idba_set(session%sehandle,"lat",getilat(ana%vol7d_ana%coord))
ier = idba_set(session%sehandle,"lon",getilon(ana%vol7d_ana%coord))
if (c_e(ana%vol7d_ana%ident)) then
  ier = idba_set(session%sehandle,"ident",ana%vol7d_ana%ident)
  ier = idba_set(session%sehandle,"mobile",1)
else
  ier = idba_set(session%sehandle,"ident",cmiss)
  ier = idba_set(session%sehandle,"mobile",0)  
end if

end subroutine dbaana_set


subroutine dbadat_set(data,session)
class(dbadat), intent(in) :: data
type(dbasession), intent(in) :: session

call data%data%dbaset(session)

end subroutine dbadat_set

subroutine dbadat_display(data)
class(dbadat), intent(in) :: data

call data%data%display()

end subroutine dbadat_display

subroutine dbadatav_set(data,session)
class(dbadatav), intent(in) :: data
type(dbasession), intent(in) :: session
integer :: i

do i=1, size(data%dat)
  call data%dat(i)%dbaset(session)
enddo

end subroutine dbadatav_set

subroutine dbadatav_display(data)
class(dbadatav), intent(in) :: data
integer :: i

do i=1, size(data%dat)
  call data%dat(i)%display()
end do

end subroutine dbadatav_display


subroutine dbadataattr_ingest(data,session)
class(dbadataattr), intent(in) :: data
type(dbasession), intent(in) :: session

!write data in dsn
call data%dbadat%dbaset(session)
call session%prendilo()
!write attributes in dsn
call data%attrv%dbaset(session)
call session%var_related(data%dbadat%data%btable)
call session%critica()

end subroutine dbadataattr_ingest

subroutine dbadataattr_display(data)
class(dbadataattr), intent(in) :: data

call data%dbadat%display()
print*,"attributes"
call data%attrv%display()

end subroutine dbadataattr_display


subroutine dbadataattrv_ingest(dataattr,session)
class(dbadataattrv), intent(in) :: dataattr
type(dbasession), intent(in) :: session
integer :: i

do i=1, size(dataattr%dataattr)
  call dataattr%dataattr(i)%dbaingest(session)
enddo

end subroutine dbadataattrv_ingest

subroutine dbadataattrv_display(dataattr)
class(dbadataattrv), intent(in) :: dataattr
integer :: i

do i=1, size(dataattr%dataattr)
  call dataattr%dataattr(i)%display()
end do

end subroutine dbadataattrv_display



type(dbadatai) elemental function dbadatai_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< descrittore variabile
INTEGER,INTENT(IN),OPTIONAL :: value  !< valore

if (present(btable)) then
  dbadatai_init%btable=btable
else
  dbadatai_init%btable=cmiss
end if

if (present(value)) then
  dbadatai_init%value=value
else
  dbadatai_init%value=imiss
end if

end function dbadatai_init

type(dbadatar) elemental function dbadatar_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< descrittore variabile
real,INTENT(IN),OPTIONAL :: value  !< valore

if (present(btable)) then
  dbadatar_init%btable=btable
else
  dbadatar_init%btable=cmiss
end if

if (present(value)) then
  dbadatar_init%value=value
else
  dbadatar_init%value=rmiss
end if

end function dbadatar_init

type(dbadatad) elemental function dbadatad_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< descrittore variabile
double precision,INTENT(IN),OPTIONAL :: value  !< valore

if (present(btable)) then
  dbadatad_init%btable=btable
else
  dbadatad_init%btable=cmiss
end if

if (present(value)) then
  dbadatad_init%value=value
else
  dbadatad_init%value=dmiss
end if

end function dbadatad_init


type(dbadatab) elemental function dbadatab_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< descrittore variabile
INTEGER(kind=int_b) ,INTENT(IN),OPTIONAL :: value  !< valore

if (present(btable)) then
  dbadatab_init%btable=btable
else
  dbadatab_init%btable=cmiss
end if

if (present(value)) then
  dbadatab_init%value=value
else
  dbadatab_init%value=bmiss
end if

end function dbadatab_init

type(dbadatac) elemental function dbadatac_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< descrittore variabile
character(len=*),INTENT(IN),OPTIONAL :: value  !< valore

if (present(btable)) then
  dbadatac_init%btable=btable
else
  dbadatac_init%btable=cmiss
end if

if (present(value)) then
  dbadatac_init%value=value
else
  dbadatac_init%value=cmiss
end if

end function dbadatac_init


subroutine dbadatai_set(data,session)
class(dbadatai), intent(in) :: data
type(dbasession), intent(in) :: session
integer :: ier
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatai_set

subroutine dbadatai_display(data)
class(dbadatai), intent(in) :: data
print *,data%btable,data%value
end subroutine dbadatai_display

subroutine dbadatar_set(data,session)
class(dbadatar), intent(in) :: data
type(dbasession), intent(in) :: session
integer :: ier
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatar_set

subroutine dbadatar_display(data)
class(dbadatar), intent(in) :: data
print *,data%btable,data%value
end subroutine dbadatar_display


subroutine dbadatad_set(data,session)
class(dbadatad), intent(in) :: data
type(dbasession), intent(in) :: session
integer :: ier
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatad_set

subroutine dbadatad_display(data)
class(dbadatad), intent(in) :: data
print *,data%btable,data%value
end subroutine dbadatad_display

subroutine dbadatab_set(data,session)
class(dbadatab), intent(in) :: data
type(dbasession), intent(in) :: session
integer :: ier
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatab_set

subroutine dbadatab_display(data)
class(dbadatab), intent(in) :: data
print *,data%btable,data%value
end subroutine dbadatab_display

subroutine dbadatac_set(data,session)
class(dbadatac), intent(in) :: data
type(dbasession), intent(in) :: session
integer :: ier
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatac_set

subroutine dbadatac_display(data)
class(dbadatac), intent(in) :: data
print *,data%btable,data%value
end subroutine dbadatac_display


!!$function dbalevel_spiega(level,handle)
!!$class(dbalevel),    intent(in) :: level
!!$integer,           intent(in)  :: handle
!!$character (len=255)   :: dbalevel_spiega
!!$integer :: ier
!!$
!!$ier = idba_spiegal(handle,level%level1,level%l1,level%level2,level%l2,dbalevel_spiega)
!!$if (ier /= 0) dbalevel_spiega = cmiss
!!$
!!$end function dbalevel_spiega


subroutine dbatimerange_display(timerange)
class(dbatimerange), intent(in) :: timerange
call display (timerange%vol7d_timerange)
end subroutine dbatimerange_display

subroutine dbatimerange_set(timerange,session)
class(dbatimerange), intent(in) :: timerange
type(dbasession), intent(in) :: session
integer :: ier

ier = idba_settimerange(session%sehandle,&
 timerange%timerange, timerange%p1, timerange%p2)

end subroutine dbatimerange_set

type(dbatimerange) function dbatimerange_init(timerange, p1, p2)
INTEGER,INTENT(IN),OPTIONAL :: timerange !< tipo di intervallo temporale
INTEGER,INTENT(IN),OPTIONAL :: p1 !< valore per il primo istante temporale
INTEGER,INTENT(IN),OPTIONAL :: p2 !< valore per il secondo istante temporale

call init (dbatimerange_init%vol7d_timerange,timerange, p1, p2)
end function dbatimerange_init

type(dbatimerange) function dbatimerange_contextana()

dbatimerange_contextana=dbatimerange()

end function dbatimerange_contextana


subroutine dbanetwork_display(network)
class(dbanetwork), intent(in) :: network
call display (network%vol7d_network)
end subroutine dbanetwork_display

subroutine dbanetwork_set(network,session)
class(dbanetwork), intent(in) :: network
type(dbasession), intent(in) :: session
integer :: ier

ier = idba_set(session%sehandle,"rep_memo", network%name)

end subroutine dbanetwork_set

type(dbanetwork) function dbanetwork_init(name)
CHARACTER(len=*),INTENT(in),OPTIONAL :: name !< Mnemonic alias for type of report

call init (dbanetwork_init%vol7d_network,name)
end function dbanetwork_init


subroutine dbadatetime_display(datetime)
class(dbadatetime), intent(in) :: datetime
call display (datetime%datetime)
end subroutine dbadatetime_display

subroutine dbadatetime_set(datetime,session)
class(dbadatetime), intent(in) :: datetime
type(dbasession), intent(in) :: session
integer :: ier,year,month,day,hour,minute,sec,msec

CALL getval(datetime%datetime, year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)
sec=nint(float(msec)/1000.)
ier = idba_setdate(session%sehandle,year,month,day,hour,minute,sec)

end subroutine dbadatetime_set

type(dbadatetime) function dbadatetime_init(dt)
type(datetime),INTENT(in),OPTIONAL :: dt !< date and time

if (present(dt)) then
  dbadatetime_init%datetime=dt
else
  dbadatetime_init%datetime=datetime_new()
end if

end function dbadatetime_init

type(dbadatetime) function dbadatetime_contextana()

dbadatetime_contextana%datetime=datetime_new()

end function dbadatetime_contextana


type(dbametadata) function dbametadata_init(level,timerange,ana,network,datetime)

type(dbalevel), intent(in), optional :: level
type(dbatimerange), intent(in), optional :: timerange
type(dbaana), intent(in), optional :: ana
type(dbanetwork), intent(in), optional :: network
type(dbadatetime), intent(in), optional :: datetime

if (present(level)) then
  dbametadata_init%level=level
else
  dbametadata_init%level=dbalevel()
end if

if (present(timerange)) then
  dbametadata_init%timerange=timerange
else
  dbametadata_init%timerange=dbatimerange()
end if

if (present(ana)) then
  dbametadata_init%ana=ana
else
  dbametadata_init%ana=dbaana()
end if

if (present(network)) then
  dbametadata_init%network=network
else
  dbametadata_init%network=dbanetwork()
end if

if (present(datetime)) then
  dbametadata_init%datetime=datetime
else
  dbametadata_init%datetime=dbadatetime()
end if

end function dbametadata_init

subroutine dbametadata_display(metadata)
class(dbametadata), intent(in) :: metadata
call metadata%level%display()
call metadata%timerange%display()
call metadata%ana%display()
call metadata%network%display()
call metadata%datetime%display()

end subroutine dbametadata_display

subroutine dbametadata_set(metadata,session)
class(dbametadata), intent(in) :: metadata
type(dbasession), intent(in) :: session

call metadata%ana%dbaset(session)
call metadata%network%dbaset(session)

if (c_e(metadata%datetime%datetime) .or. &
    c_e(metadata%level%vol7d_level) .or. &
    c_e(metadata%timerange%vol7d_timerange)) then

  call metadata%datetime%dbaset(session)
  call metadata%level%dbaset(session)
  call metadata%timerange%dbaset(session)

else
  call session%setcontextana()
end if

end subroutine dbametadata_set



type(dbametadata) function dbametadata_contextana(metadata)
class(dbametadata), intent(in) :: metadata

type (dbadatetime)     :: datetime
type (dbalevel)        :: level
type (dbatimerange)    :: timerange

select type(metadata)
type is(dbametadata)
  dbametadata_contextana=metadata
end select

dbametadata_contextana%datetime=datetime%dbacontextana()
dbametadata_contextana%level=level%dbacontextana()
dbametadata_contextana%timerange=timerange%dbacontextana()

end function dbametadata_contextana


subroutine dbametaanddata_display(metaanddata)
class(dbametaanddata), intent(in) :: metaanddata

call metaanddata%metadata%display()
call metaanddata%dataattrv%display()

end subroutine dbametaanddata_display

subroutine dbametaanddata_ingest(metaanddata,session)
class(dbametaanddata), intent(in) :: metaanddata
type(dbasession), intent(in) :: session

!write metadata
call session%set(metadata=metaanddata%metadata)
!write ana data and attribute
call session%ingest(dataattrv=metaanddata%dataattrv)

end subroutine dbametaanddata_ingest


type(dbaconnection) function dbaconnection_init(dsn, user, password,categoryappend,idbhandle)
character (len=*), intent(in), optional :: dsn
character (len=*), intent(in), optional :: user
character (len=*), intent(in), optional :: password
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
integer,INTENT(in),OPTIONAL :: idbhandle !< dsn connection; if present it will be used
character(len=50) :: quidsn,quiuser,quipassword
integer :: ier
character(len=512) :: a_name

print *,"dbhandle init"

if (present(categoryappend))then
  call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
  call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
dbaconnection_init%category=l4f_category_get(a_name)

! impostiamo la gestione dell'errore
ier=idba_error_set_callback(0,dballe_error_handler, &
 dbaconnection_init%category,dbaconnection_init%handle_err)

if (.not. c_e(optio_i(idbhandle))) then

  quidsn = "test"
  quiuser = "test"
  quipassword = ""
  IF (PRESENT(dsn)) THEN
    IF (c_e(dsn)) quidsn = dsn
  ENDIF
  IF (PRESENT(user)) THEN
    IF (c_e(user)) quiuser = user
  ENDIF
  IF (PRESENT(password)) THEN
    IF (c_e(password)) quipassword = password
  ENDIF

  ier=idba_presentati(dbaconnection_init%dbhandle,quidsn,quiuser,quipassword)
else
  dbaconnection_init%dbhandle=optio_i(idbhandle)
end if

end function dbaconnection_init

subroutine dbaconnection_delete(handle)
class (dbaconnection), intent(inout) :: handle
integer :: ier

print *,"dbhandle delete"

if (c_e(handle%dbhandle)) then 
  ier = idba_arrivederci(handle%dbhandle)
end if

end subroutine dbaconnection_delete

type(dbasession) function dbasession_init(connection,anaflag, dataflag, attrflag,&
 filename,mode,type,write,wipe,repinfo,categoryappend)
type(dbaconnection),intent(in) :: connection
character (len=*), intent(in), optional :: anaflag
character (len=*), intent(in), optional :: dataflag
character (len=*), intent(in), optional :: attrflag
character (len=*), intent(in), optional :: filename
character (len=*), intent(in), optional :: mode
logical,INTENT(in),OPTIONAL :: write !< abilita la scrittura sul DSN/file ( default=.false. )
logical,INTENT(in),OPTIONAL :: wipe !<  svuota il DSN/file e/o lo prepara per una scrittura ( default=.false. )
character(len=*), INTENT(in),OPTIONAL :: repinfo !< eventuale file repinfo.csv usato con wipe ( default="" )
character(len=*),intent(in),optional :: type !< the file format. It can be "BUFR" or "CREX". (default="BUFR")
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

integer :: ier
character (len=5) :: lanaflag,ldataflag,lattrflag
character (len=1) :: lmode,ltype
logical :: quiwrite,quiwipe
character(len=255) :: quirepinfo
logical :: exist
character(len=512) :: a_name

print *,"dbasession init"


if (present(categoryappend))then
  call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
  call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
dbasession_init%category=l4f_category_get(a_name)


quiwrite=.false.
if (present(write))then
  quiwrite=write
endif

quiwipe=.false.
quirepinfo=""
if (present(wipe))then
  quiwipe=wipe
  if (present(repinfo))then
    quirepinfo=repinfo
  endif
endif

if (present(filename)) then

  if (present(anaflag).or.present(dataflag).or.present( attrflag)) then
    call l4f_category_log(dbasession_init%category,L4F_ERROR,"option anaflag, dataflag, attrflag defined with filename")
    CALL raise_error()
  end if

  inquire(file=filename,EXIST=exist)

  lmode="r"
  if (quiwrite)then
    if (quiwipe.or..not.exist) then
      lmode="w"
    else
      lmode="a"
      call l4f_category_log(dbasession_init%category,L4F_INFO,"file exists; appending data to file")
    end if
  else
    if (.not.exist) then
      call l4f_category_log(dbasession_init%category,L4F_ERROR,"file does not exist; cannot open file for read")
      CALL raise_fatal_error()
    end if
  end if

  if (present(mode)) lmode = mode

  dbasession_init%file=.true.

  ier =  idba_messaggi(dbasession_init%sehandle,filename, lmode, ltype)
else

  if (present(mode).or.present(type)) then
    call l4f_category_log(dbasession_init%category,L4F_ERROR,"option mode, type defined without filename")
    CALL raise_error()
  end if

  call optio(anaflag,lanaflag)
  if (.not. c_e(lanaflag)) lanaflag = "read"

  call optio(dataflag,ldataflag)
  if (.not. c_e(ldataflag)) ldataflag = "read"

  call optio(attrflag,lattrflag)
  if (.not. c_e(lattrflag)) lattrflag = "read"

  ier = idba_preparati(connection%dbhandle,dbasession_init%sehandle, lanaflag, ldataflag, lattrflag)

  dbasession_init%file=.true.

  if (quiwipe)ier=idba_scopa (dbasession_init%sehandle,quirepinfo)

end if


end function dbasession_init


subroutine dbasession_unsetall(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_unsetall(session%sehandle)
end if

end subroutine dbasession_unsetall

subroutine dbasession_prendilo(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_prendilo(session%sehandle)
end if

end subroutine dbasession_prendilo

subroutine dbasession_var_related(session,btable)
class (dbasession), intent(in) :: session
character(len=*),INTENT(IN) :: btable !< descrittore variabile
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_set(session%sehandle,"*var_related",btable)
end if

end subroutine dbasession_var_related

subroutine dbasession_setcontextana(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_setcontextana(session%sehandle)
end if

end subroutine dbasession_setcontextana

subroutine dbasession_critica(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_critica(session%sehandle)
end if

end subroutine dbasession_critica


subroutine dbasession_set(session,metadata,datav,data,level,timerange)
class (dbasession), intent(in) :: session
type (dbametadata),optional ::metadata
class(dbadatav),optional ::datav
class(dbadata),optional ::data
type (dbalevel),optional ::level
type (dbatimerange),optional ::timerange

if (present(metadata)) then
  call metadata%dbaset(session)
endif

if (present(level)) then
  call level%dbaset(session)
endif

if (present(timerange)) then
  call timerange%dbaset(session)
endif

if (present(datav)) then
  call datav%dbaset(session)
end if

if (present(data)) then
  call data%dbaset(session)
end if

end subroutine dbasession_set


subroutine dbasession_ingest(session,dataattr,dataattrv,metaanddata)
class(dbasession), intent(in)   :: session
class(dbadataattr),optional     :: dataattr
class(dbadataattrv),optional    :: dataattrv
class(dbametaanddata),optional  :: metaanddata

if (present(dataattr)) then
  call dataattr%dbaingest(session)
end if

if (present(dataattrv)) then
  call dataattrv%dbaingest(session)
end if

if (present(metaanddata)) then
  call metaanddata%dbaingest(session)
end if

end subroutine dbasession_ingest


subroutine dbasession_delete(session)
class (dbasession), intent(inout) :: session
integer :: ier

print *,"dbasession delete"

if (c_e(session%sehandle)) then 
  ier = idba_fatto(session%sehandle)
end if

end subroutine dbasession_delete


FUNCTION dballe_error_handler(category)
INTEGER :: category, code, l4f_level
INTEGER :: dballe_error_handler

CHARACTER(len=1000) :: message, buf

code = idba_error_code()

! check if "Value outside acceptable domain"
if (code == 13 ) then
  l4f_level=L4F_WARN
else
  l4f_level=L4F_ERROR
end if

call idba_error_message(message)
call l4f_category_log(category,l4f_level,trim(message))

call idba_error_context(buf)

call l4f_category_log(category,l4f_level,trim(buf))

call idba_error_details(buf)
call l4f_category_log(category,L4F_INFO,trim(buf))


! if "Value outside acceptable domain" do not raise error
if (l4f_level == L4F_ERROR ) CALL raise_fatal_error("dballe: "//message)

dballe_error_handler = 0
return

END FUNCTION dballe_error_handler

end MODULE dballe_class
