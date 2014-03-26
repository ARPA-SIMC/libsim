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
use list_abstract

IMPLICIT NONE

include "dballeff.h"
private

character (len=255),parameter:: subcategory="dballe_class"

!> manage connection handle to a DSN
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

!> manage session handle
type,public ::  dbasession
  integer :: sehandle=imiss
  logical :: file=.false.
  character(len=40) :: template=cmiss
  character(len=255) :: filename=cmiss
  character(len=40) :: mode=cmiss
  character(len=40) :: format=cmiss
  logical           :: simplified=.true.
  logical           :: memdb=.false.
  type(dbaconnection) :: memconnection
  integer :: category=imiss !< log4fortran
  integer :: count=imiss !< number of results left to query
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbasession_delete
# else
    procedure :: delete => dbasession_delete
# endif
    procedure :: unsetall => dbasession_unsetall
    procedure :: remove_all => dbasession_remove_all
    procedure :: set => dbasession_set
    procedure :: setcontextana => dbasession_setcontextana
    procedure :: dimenticami => dbasession_dimenticami
    procedure :: ingest => dbasession_ingest
    procedure :: prendilo => dbasession_prendilo
    procedure :: var_related => dbasession_var_related
    procedure :: critica => dbasession_critica
    procedure :: scusa => dbasession_scusa
    procedure :: messages_open_input => dbasession_messages_open_input
    procedure :: messages_open_output => dbasession_messages_open_output
    procedure :: messages_read_next => dbasession_messages_read_next
    procedure :: messages_write_next => dbasession_messages_write_next
    procedure :: close_message => dbasession_close_message
    procedure :: unsetb => dbasession_unsetb
    procedure :: filerewind => dbasession_filerewind
    procedure :: extrude_ana => dbasession_extrude_ana
    procedure :: extrude_anav => dbasession_extrude_anav
    procedure :: extrude_anal => dbasession_extrude_anal
    procedure :: extrude_metaanddata => dbasession_extrude_metaanddata
    procedure :: extrude_metaanddatal => dbasession_extrude_metaanddatal
    procedure :: extrude_metaanddatav => dbasession_extrude_metaanddatav
    procedure :: extrude_metaanddatai => dbasession_extrude_metaanddatai
    procedure :: extrude_metaanddataiv => dbasession_extrude_metaanddataiv
    procedure :: extrude_metaanddatail => dbasession_extrude_metaanddatail
    procedure :: extrude_metaanddatab => dbasession_extrude_metaanddatab
    procedure :: extrude_metaanddatabv => dbasession_extrude_metaanddatabv
    procedure :: extrude_metaanddatabl => dbasession_extrude_metaanddatabl
    procedure :: extrude_metaanddatad => dbasession_extrude_metaanddatad
    procedure :: extrude_metaanddatadv => dbasession_extrude_metaanddatadv
    procedure :: extrude_metaanddatadl => dbasession_extrude_metaanddatadl
    procedure :: extrude_metaanddatar => dbasession_extrude_metaanddatar
    procedure :: extrude_metaanddatarv => dbasession_extrude_metaanddatarv
    procedure :: extrude_metaanddatarl => dbasession_extrude_metaanddatarl
    procedure :: extrude_metaanddatac => dbasession_extrude_metaanddatac
    procedure :: extrude_metaanddatacv => dbasession_extrude_metaanddatacv
    procedure :: extrude_metaanddatacl => dbasession_extrude_metaanddatacl
    procedure :: dissolve_metadata => dbasession_dissolve_metadata
    procedure :: dissolveattr => dbasession_dissolveattr_metadata
    generic   :: dissolve => dissolve_metadata ,dimenticami
    generic   :: extrudea => extrude_ana, extrude_anav,extrude_anal
    generic   :: extrude  => extrude_metaanddata,extrude_metaanddatav,extrude_metaanddatal,&
     extrude_metaanddataiv,extrude_metaanddatabv,extrude_metaanddatadv,extrude_metaanddatarv,extrude_metaanddatacv&
     ,extrude_metaanddatail,extrude_metaanddatarl,extrude_metaanddatadl,extrude_metaanddatabl,extrude_metaanddatacl
end type dbasession

interface dbasession
  procedure  dbasession_init      ! add constructor to shape generic interface
end interface

!> level metadata
type,public,extends(vol7d_level) :: dbalevel
  contains

# ifdef F2003_FULL_FEATURES
!    final :: dbalevel_delete
# else
!    procedure :: delete => dbalevel_delete
# endif
  procedure :: display => dbalevel_display
  procedure :: dbaset => dbalevel_set
  procedure :: dbaenq => dbalevel_enq
  procedure,nopass :: dbacontextana => dbalevel_contextana
!!$procedure :: spiega => dbalevel_spiega
end type dbalevel

interface dbalevel
  procedure   dbalevel_init     ! add constructor to shape generic interface
end interface

!> timerange metadata
type,public,extends(vol7d_timerange) :: dbatimerange
  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbatimerange_delete
# else
!    procedure :: delete => dbatimerange_delete
# endif
  procedure :: display => dbatimerange_display
  procedure :: dbaset => dbatimerange_set
  procedure :: dbaenq => dbatimerange_enq
  procedure,nopass :: dbacontextana => dbatimerange_contextana
!!$procedure :: spiega => dbatimerange_spiega
end type dbatimerange

interface dbatimerange
  procedure   dbatimerange_init     ! add constructor to shape generic interface
end interface

!> fortran 2003 interface to geo_coord
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
  procedure :: display => dbacoord_display

end type dbacoord

interface dbacoord
  procedure   dbacoord_init     ! add constructor to shape generic interface
end interface

!> ana metadata
type,public,extends(vol7d_ana) :: dbaana

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbaana_delete
# else
!    procedure :: delete => dbaana_delete
# endif
  procedure :: display => dbaana_display
  procedure :: dbaset => dbaana_set
  procedure :: dbaenq => dbaana_enq
  procedure :: dbaingest => dbaana_ingest
end type dbaana

interface dbaana
  procedure   dbaana_init     ! add constructor to shape generic interface
end interface

!> double linked list of ana
type, public, extends(list) :: dbaanaList
  contains
  procedure :: current => currentdbaana ! get integer pointed by iterator
  procedure :: display => displaydbaana ! print the integer list
end type dbaanaList

!> network metadata
type,public,extends(vol7d_network) :: dbanetwork

  !Every type of report has an associated priority that controls which
  !data are first returned when there is more than one in the same
  !physical space. It can be changed by editing
  !/etc/dballe/repinfo.csv
  integer :: priority

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbanetwork_delete
# else
!    procedure :: delete => dbanetwork_delete
# endif
  procedure :: display => dbanetwork_display
  procedure :: dbaset => dbanetwork_set
  procedure :: dbaenq => dbanetwork_enq

end type dbanetwork

interface dbanetwork
  procedure   dbanetwork_init     ! add constructor to shape generic interface
end interface


!> datetime metadata
type,public,extends(datetime) :: dbadatetime

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbanetwork_delete
# else
!    procedure :: delete => dbanetwork_delete
# endif
  procedure :: display => dbadatetime_display
  procedure :: dbaset => dbadatetime_set
  procedure :: dbaenq => dbadatetime_enq
  procedure,nopass :: dbacontextana => dbadatetime_contextana
end type dbadatetime

interface dbadatetime
  procedure   dbadatetime_init     ! add constructor to shape generic interface
end interface


!> base (abstract)  type for data
type,public,abstract :: dbadata
  character(len=9) :: btable
contains
  procedure(dbadata_set),deferred :: dbaset
  procedure :: dbadata_geti
  procedure :: dbadata_getr
  procedure :: dbadata_getd
  procedure :: dbadata_getb
  procedure :: dbadata_getc
  generic   :: get => dbadata_geti,dbadata_getr,dbadata_getd,dbadata_getb,dbadata_getc
  procedure(dbadata_display),deferred :: display
end type dbadata

abstract interface
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

!> integer version for dbadata
type,public, extends(dbadata) :: dbadatai
  integer :: value
contains
  procedure :: dbadata_geti => dbadatai_geti
  procedure :: dbaset => dbadatai_set
  procedure :: display => dbadatai_display
end type dbadatai

interface dbadatai
  procedure ::  dbadatai_init     ! add constructor to shape generic interface
end interface dbadatai

!> real version for dbadata
type,public, extends(dbadata) :: dbadatar
  real :: value
contains
  procedure :: dbadata_getr => dbadatar_getr
  procedure :: dbaset => dbadatar_set
  procedure :: display => dbadatar_display
end type dbadatar

interface dbadatar
  procedure ::  dbadatar_init     ! add constructor to shape generic interface
end interface dbadatar


!> doubleprecision version for dbadata
type,public, extends(dbadata) :: dbadatad
  doubleprecision :: value
contains
  procedure :: dbadata_getd => dbadatad_getd
  procedure :: dbaset => dbadatad_set
  procedure :: display => dbadatad_display
end type dbadatad

interface dbadatad
  procedure ::  dbadatad_init     ! add constructor to shape generic interface
end interface dbadatad


!> byte version for dbadata
type,public, extends(dbadata) :: dbadatab
  integer(kind=int_b) :: value
contains
  procedure :: dbadata_getb => dbadatab_getb
  procedure :: dbaset => dbadatab_set
  procedure :: display => dbadatab_display
end type dbadatab

interface dbadatab
  procedure ::  dbadatab_init     ! add constructor to shape generic interface
end interface dbadatab


!> character version for dbadata
type,public, extends(dbadata) :: dbadatac
!  character(:) :: value
  character(255) :: value
contains
  procedure :: dbadata_getc => dbadatac_getc
  procedure :: dbaset => dbadatac_set
  procedure :: display => dbadatac_display
end type dbadatac

interface dbadatac
  procedure ::  dbadatac_init     ! add constructor to shape generic interface
end interface dbadatac

!> summ of all metadata pieces
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
  procedure :: dbaenq => dbametadata_enq
  procedure :: dbacontextana => dbametadata_contextana
  procedure :: display => dbametadata_display
  procedure :: equal => dbametadata_equal
  generic   :: operator (==) => equal
end type dbametadata

interface dbametadata
  procedure   dbametadata_init     ! add constructor to shape generic interface
end interface

!> container for dbadata (used for promiscuous vector of data)
type, public :: dbadat
  class(dbadata),allocatable :: dat
  contains
  procedure :: display => dbadat_display
  procedure :: dbaset => dbadat_set
!  procedure :: dbaingest => dbadat_ingest
end type dbadat

!> vector of container of dbadata
type, public :: dbadatav
  type(dbadat),allocatable :: dat(:)
  contains
  procedure :: display => dbadatav_display
  procedure :: dbaset => dbadatav_set
!  procedure :: dbaingest => dbadatav_ingest
end type dbadatav

!> extend one data container with a vector of data container (one data plus attributes)
type, public ,extends(dbadat):: dbadataattr
  type(dbadatav) :: attrv
  contains
  procedure :: display => dbadataattr_display
  procedure :: dbaingest => dbadataattr_ingest
end type dbadataattr

!> vector of dbadataattr (more data plus attributes)
type, public :: dbadataattrv
  class(dbadataattr),allocatable :: dataattr(:)
  contains
  procedure :: display => dbadataattrv_display
  procedure :: dbaingest => dbadataattrv_ingest
end type dbadataattrv

!> one metadata with more data plus attributes
type, public :: dbametaanddata
  type(dbametadata) :: metadata
  type(dbadataattrv) ::dataattrv
  contains
  procedure :: display => dbametaanddata_display
  procedure :: dbaingest => dbametaanddata_ingest
end type dbametaanddata

!> one metadata plus vector of container of dbadata
type, public :: dbametaanddatav
  type(dbametadata) :: metadata
  type(dbadatav) ::datav
  contains
  procedure :: display => dbametaanddatav_display
  procedure :: dbaingest => dbametaanddatav_ingest
end type dbametaanddatav

!> double linked list of dbametaanddata
type, public, extends(list) :: dbametaanddataList
  contains
  procedure :: current => currentdbametaanddata ! get integer pointed by iterator
  procedure :: display => displaydbametaanddata ! print the integer list
end type dbametaanddataList

!> metadata and integer data
type, public,extends(dbadatai) :: dbametaanddatai
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatai_display
  procedure :: dbaingest => dbametaanddatai_ingest
end type dbametaanddatai

type, public, extends(list) :: dbametaanddataiList
  contains
  procedure :: current => currentdbametaanddatai ! get integer pointed by iterator
  procedure :: display => displaydbametaanddatai ! print the integer list
  procedure :: toarray => toarray_dbametaanddatai ! return ana allocated array
end type dbametaanddataiList

!> metadata and byte data
type, public,extends(dbadatab) :: dbametaanddatab
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatab_display
  procedure :: dbaingest => dbametaanddatab_ingest
end type dbametaanddatab

type, public, extends(list) :: dbametaanddatabList
  contains
  procedure :: current => currentdbametaanddatab ! get integer pointed by iterator
  procedure :: display => displaydbametaanddatab ! print the integer list
  procedure :: toarray => toarray_dbametaanddatab ! return ana allocated array
end type dbametaanddatabList

!> metadata and doubleprecision data
type, public,extends(dbadatad) :: dbametaanddatad
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatad_display
  procedure :: dbaingest => dbametaanddatad_ingest
end type dbametaanddatad

type, public, extends(list) :: dbametaanddatadList
  contains
  procedure :: current => currentdbametaanddatad ! get integer pointed by iterator
  procedure :: display => displaydbametaanddatad ! print the integer list
  procedure :: toarray => toarray_dbametaanddatad ! return ana allocated array
end type dbametaanddatadList

!> metadata and real data
type, public,extends(dbadatar) :: dbametaanddatar
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatar_display
  procedure :: dbaingest => dbametaanddatar_ingest
end type dbametaanddatar

type, public, extends(list) :: dbametaanddatarList
  contains
  procedure :: current => currentdbametaanddatar ! get integer pointed by iterator
  procedure :: display => displaydbametaanddatar ! print the integer list
  procedure :: toarray => toarray_dbametaanddatar ! return ana allocated array
end type dbametaanddatarList

!> metadata and character data
type, public,extends(dbadatac) :: dbametaanddatac
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatac_display
  procedure :: dbaingest => dbametaanddatac_ingest
end type dbametaanddatac

type, public, extends(list) :: dbametaanddatacList
  contains
  procedure :: current => currentdbametaanddatac ! get integer pointed by iterator
  procedure :: display => displaydbametaanddatac ! print the integer list
  procedure :: toarray => toarray_dbametaanddatac ! return ana allocated array
end type dbametaanddatacList

!> filter to apply before extrude data
type, public ::  dbafilter
  type(dbaana)       :: ana
  character(len=6)   :: var
  type(dbadatetime)  :: datetime
  type(dbalevel)     :: level
  type(dbatimerange) :: timerange
  type(dbanetwork)   :: network

  type(dbacoord)     :: coordmin,coordmax
  type(dbadatetime)  :: datetimemin,datetimemax
  integer            :: limit
  character(len=255) :: ana_filter, data_filter, attr_filter, varlist, starvarlist
  integer            :: priomin,priomax
  logical            :: contextana
  type(dbadatav)     :: vars,starvars
  contains
  procedure          :: display => dbafilter_display
  procedure          :: dbaset => dbafilter_set
end type dbafilter

interface dbafilter
  procedure   dbafilter_init     ! add constructor to shape generic interface
end interface

contains

subroutine displaydbametaanddata(this)
class(dbametaanddataList) :: this
type(dbametaanddata) :: element

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:"
  element=this%current()
  call element%display()
  call this%next()
end do
end subroutine displaydbametaanddata

type(dbametaanddata) function currentdbametaanddata(this)
class(dbametaanddataList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddata)
  currentdbametaanddata = v
end select
end function currentdbametaanddata

subroutine dbadata_geti(data,value)
class(dbadata), intent(in) :: data
integer, intent(out) :: value
value=imiss
end subroutine dbadata_geti

subroutine dbadata_getr(data,value)
class(dbadata), intent(in) :: data
real, intent(out) :: value
value=rmiss
end subroutine dbadata_getr

subroutine dbadata_getd(data,value)
class(dbadata), intent(in) :: data
doubleprecision, intent(out) :: value
value=dmiss
end subroutine dbadata_getd

subroutine dbadata_getb(data,value)
class(dbadata), intent(in) :: data
INTEGER(kind=int_b), intent(out) :: value
value=bmiss
end subroutine dbadata_getb

subroutine dbadata_getc(data,value)
class(dbadata), intent(in) :: data
character(len=*), intent(out) :: value
value=cmiss
end subroutine dbadata_getc

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

subroutine dbalevel_enq(level,session)
class(dbalevel), intent(out) :: level
type(dbasession), intent(in) :: session
integer :: ier

ier = idba_enqlevel(session%sehandle,&
 level%level1, level%l1, level%level2, level%l2)

end subroutine dbalevel_enq

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

subroutine dbacoord_display(coord)
class(dbacoord), intent(in) :: coord
call display (coord%geo_coord)
end subroutine dbacoord_display

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

subroutine dbaana_enq(ana,session)
class(dbaana), intent(out) :: ana
type(dbasession), intent(in) :: session
integer :: ier,ilat,ilon

!if (c_e(session%sehandle)) then
ier = idba_enq(session%sehandle,"lat",ilat)
ier = idba_enq(session%sehandle,"lon",ilon)

call init(ana%vol7d_ana%coord,ilon=ilon,ilat=ilat)
ier = idba_enq(session%sehandle,"ident",ana%vol7d_ana%ident)

end subroutine dbaana_enq


subroutine dbaana_ingest(ana,session)
class(dbaana), intent(in) :: ana
type(dbasession), intent(in) :: session

call session%unsetall()
!write ana
call session%set(ana=ana)
call session%prendilo()

!to close message on file
call session%close_message()

end subroutine dbaana_ingest


subroutine displaydbaana(this)
class(dbaanaList) :: this
type(dbaana) :: element

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:"
  element=this%current()
  call element%display()
  call this%next()
end do
end subroutine displaydbaana

type(dbaana) function currentdbaana(this)
class(dbaanaList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbaana)
  currentdbaana = v
end select
end function currentdbaana


subroutine dbadat_set(data,session)
class(dbadat), intent(in) :: data
type(dbasession), intent(in) :: session

call data%dat%dbaset(session)

end subroutine dbadat_set

subroutine dbadat_display(data)
class(dbadat), intent(in) :: data

call data%dat%display()

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

!!$subroutine dbadat_ingest(dat,session)
!!$class(dbadat), intent(in) :: dat
!!$type(dbasession), intent(in) :: session
!!$
!!$!write data in dsn
!!$call dat%dbaset(session)
!!$call session%prendilo()
!!$
!!$end subroutine dbadat_ingest
!!$
!!$subroutine dbadatav_ingest(datav,session)
!!$class(dbadatav), intent(in) :: datav
!!$type(dbasession), intent(in) :: session
!!$integer :: i
!!$!write data in dsn
!!$do i =1,size(datav%dat)
!!$  call datav%dat(i)%dbaset(session)
!!$end do
!!$call session%prendilo()
!!$
!!$end subroutine dbadatav_ingest


subroutine dbasession_unsetb(session)
class(dbasession), intent(in) :: session
integer :: ier

if (session%file)then
  ier=idba_unsetb(session%sehandle)
end if
end subroutine dbasession_unsetb

subroutine dbasession_close_message(session)
class(dbasession), intent(in) :: session
integer :: ier

if (session%file)then
  if (c_e(session%template)) then
    ier=idba_set (session%sehandle,"query","message "//trim(session%template))
  else
    ier=idba_set (session%sehandle,"query","message")
  end if

  call session%unsetb()
  call session%prendilo()

end if
end subroutine dbasession_close_message


subroutine dbasession_messages_open_input(session,filename,mode,format,simplified)
class(dbasession), intent(in) :: session
character (len=*), intent(in) :: filename
character (len=*), intent(in) :: mode !> "r"/"w"/"a" the open mode ("r" for read, "w" for write or create, "a" for append) 
character (len=*), intent(in) :: format !< the file format. It can be "BUFR" or "CREX". (default="BUFR")
logical, intent(in)           :: simplified !> rappresentation when interpret message (simplified/precise)
!character (len=*), intent(in) :: template !> template to use for write on file

integer :: ier

ier = idba_messages_open_input(session%sehandle, filename, mode, format, simplified)

end subroutine dbasession_messages_open_input


subroutine dbasession_messages_open_output(session,filename,mode,format)
class(dbasession), intent(in) :: session
character (len=*), intent(in) :: filename
character (len=*), intent(in),optional :: mode !> "r"/"w"/"a" the open mode ("r" for read, "w" for write or create, "a" for append) 
character (len=*), intent(in),optional :: format !< the file format. It can be "BUFR" or "CREX". (default="BUFR")

integer :: ier
character (len=40) :: lmode, lformat

lmode="w"
if (present(mode)) lmode=mode

lformat="BUFR"
if (present(format)) lformat=format

ier = idba_messages_open_output(session%sehandle, filename, lmode, lformat)

end subroutine dbasession_messages_open_output


logical function dbasession_messages_read_next(session)
class(dbasession), intent(in) :: session

integer :: ier

ier = idba_messages_read_next(session%sehandle, dbasession_messages_read_next)

end function dbasession_messages_read_next

subroutine dbasession_messages_write_next(session,template_name)
class(dbasession), intent(in) :: session
character(len=*), optional :: template_name
character(len=40) :: ltemplate_name

integer :: ier

ltemplate_name="generic"
if (present(template_name)) ltemplate_name=template_name

ier = idba_messages_write_next(session%sehandle,ltemplate_name)

end subroutine dbasession_messages_write_next


subroutine dbasession_dissolve_metadata(session,metadata)
class(dbasession), intent(in) :: session
type(dbametadata), intent(in) :: metadata(:)

integer :: i

do i =1, size (metadata)

  call metadata(i)%dbaset(session)
  call session%dissolve()

end do

end subroutine dbasession_dissolve_metadata



subroutine dbasession_dissolveattr_metadata(session,metadata)
class(dbasession), intent(in) :: session
type(dbametadata), intent(in),optional :: metadata(:)

character(len=9) :: btable
integer :: i,ii,count,ier

if (present (metadata)) then
  do i =1, size (metadata)

    ! here if metadata have some field missig they will be set to missing so it will be unset in dballe (I hope)
    call metadata(i)%dbaset(session)
    ier = idba_voglioquesto(session%sehandle, count)

    if (.not. c_e(count)) cycle
    do ii =1,count
      ier = idba_dammelo(session%sehandle, btable)
      !call session%var_related(btable)         !not needed after dammelo
      call session%scusa()
    end do

  end do
else

  ier = idba_voglioquesto(session%sehandle, count)

  if (c_e(count)) then
    do i =1,count
      ier = idba_dammelo(session%sehandle, btable)
      !call session%var_related(btable)         !not needed after dammelo
      call session%scusa()
    end do
  end if
end if
end subroutine dbasession_dissolveattr_metadata


subroutine dbadataattr_ingest(data,session)
class(dbadataattr), intent(in) :: data
type(dbasession), intent(in) :: session

!write data in dsn
call data%dbadat%dbaset(session)
call session%prendilo()
!write attributes in dsn
call data%attrv%dbaset(session)
call session%var_related(data%dbadat%dat%btable)
call session%critica()

!to close message on file
!call session%close_message()

end subroutine dbadataattr_ingest

subroutine dbadataattr_display(data)
class(dbadataattr), intent(in) :: data

print*,"Data:"
call data%dbadat%display()
print*,"Attributes:"
call data%attrv%display()

end subroutine dbadataattr_display


subroutine dbadataattrv_ingest(dataattr,session)
class(dbadataattrv), intent(in) :: dataattr
type(dbasession), intent(in) :: session
integer :: i

do i=1, size(dataattr%dataattr)
  call dataattr%dataattr(i)%dbaingest(session)
enddo

!to close message on file
!call session%prendilo()
!call session%close_message()

end subroutine dbadataattrv_ingest

subroutine dbadataattrv_display(dataattr)
class(dbadataattrv), intent(in) :: dataattr
integer :: i

do i=1, size(dataattr%dataattr)
  call dataattr%dataattr(i)%display()
end do

end subroutine dbadataattrv_display


subroutine dbadatai_geti(data,value)
class(dbadatai), intent(in) :: data
integer, intent(out) :: value
value=data%value
end subroutine dbadatai_geti

subroutine dbadatar_getr(data,value)
class(dbadatar), intent(in) :: data
real, intent(out) :: value
value=data%value
end subroutine dbadatar_getr

subroutine dbadatad_getd(data,value)
class(dbadatad), intent(in) :: data
doubleprecision, intent(out) :: value
value=data%value
end subroutine dbadatad_getd

subroutine dbadatab_getb(data,value)
class(dbadatab), intent(in) :: data
integer(kind=int_b), intent(out) :: value
value=data%value
end subroutine dbadatab_getb

subroutine dbadatac_getc(data,value)
class(dbadatac), intent(in) :: data
character(len=*), intent(out) :: value
value=data%value
end subroutine dbadatac_getc


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
print *,"Btable: ",data%btable,"Value: ",trim(data%value)
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

subroutine dbatimerange_enq(timerange,session)
class(dbatimerange), intent(out) :: timerange
type(dbasession), intent(in) :: session
integer :: ier

ier = idba_enqtimerange(session%sehandle,&
 timerange%timerange, timerange%p1, timerange%p2)

end subroutine dbatimerange_enq

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
print *,"Priority=",network%priority
end subroutine dbanetwork_display

subroutine dbanetwork_set(network,session)
class(dbanetwork), intent(in) :: network
type(dbasession), intent(in) :: session
integer :: ier

ier = idba_set(session%sehandle,"rep_memo", network%name)

end subroutine dbanetwork_set

subroutine dbanetwork_enq(network,session)
class(dbanetwork), intent(out) :: network
type(dbasession), intent(in) :: session
integer :: ier

ier = idba_enq(session%sehandle,"rep_memo", network%name)
ier = idba_enq(session%sehandle,"priority", network%priority)

end subroutine dbanetwork_enq

type(dbanetwork) function dbanetwork_init(name)
CHARACTER(len=*),INTENT(in),OPTIONAL :: name !< Mnemonic alias for type of report

call init (dbanetwork_init%vol7d_network,name)
dbanetwork_init%priority=imiss
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

if (c_e(msec)) then
  sec=nint(float(msec)/1000.)
else
  sec=imiss
end if

ier = idba_setdate(session%sehandle,year,month,day,hour,minute,sec)

end subroutine dbadatetime_set

subroutine dbadatetime_enq(datetime,session)
class(dbadatetime), intent(out) :: datetime
type(dbasession), intent(in) :: session
integer :: ier,year,month,day,hour,minute,sec,msec

ier = idba_enqdate(session%sehandle,year,month,day,hour,minute,sec)

if (c_e(sec)) then
  msec=sec*1000
else
  msec=imiss
end if

CALL init(datetime%datetime, year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)

end subroutine dbadatetime_enq

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

subroutine dbametadata_enq(metadata,session)
class(dbametadata), intent(out) :: metadata
type(dbasession), intent(in) :: session

call metadata%ana%dbaenq(session)
call metadata%network%dbaenq(session)
call metadata%datetime%dbaenq(session)
call metadata%level%dbaenq(session)
call metadata%timerange%dbaenq(session)

end subroutine dbametadata_enq



elemental logical function dbametadata_equal(this,that)

class(dbametadata), intent(in) :: this
class(dbametadata), intent(in) :: that

if ( &
 this%level%vol7d_level         == that%level%vol7d_level .and. &
 this%timerange%vol7d_timerange == that%timerange%vol7d_timerange .and. &
 this%datetime%datetime         == that%datetime%datetime .and. &
 this%network%vol7d_network     == that%network%vol7d_network .and. &
 this%ana%vol7d_ana             == that%ana%vol7d_ana &
 ) then
  dbametadata_equal = .true.
else
  dbametadata_equal = .false.
end if

end function dbametadata_equal


type(dbafilter) function dbafilter_init(ana,var,datetime,level,timerange,network,&
 datetimemin,datetimemax,coordmin,coordmax,limit,&
 ana_filter, data_filter, attr_filter, varlist, starvarlist, priomin, priomax, contextana,&
 vars, starvars)

type(dbaana),intent(in),optional       :: ana
character(len=*),intent(in),optional   :: var
type(dbadatetime),intent(in),optional  :: datetime
type(dbalevel),intent(in),optional     :: level
type(dbatimerange),intent(in),optional :: timerange
type(dbanetwork),intent(in),optional   :: network

type(dbacoord),intent(in),optional :: coordmin,coordmax
type(dbadatetime),intent(in),optional :: datetimemin,datetimemax
integer,intent(in),optional :: limit
character(len=*),intent(in),optional :: ana_filter, data_filter, attr_filter, varlist, starvarlist
integer,intent(in),optional :: priomin,priomax
logical,intent(in),optional :: contextana
class(dbadatav),intent(in),optional :: vars,starvars

integer :: i

if (present(ana)) then
  dbafilter_init%ana=ana
else
  dbafilter_init%ana=dbaana()
end if

if (present(var)) then
  dbafilter_init%var=var
else
  dbafilter_init%var=cmiss
end if

if (present(datetime)) then
  dbafilter_init%datetime=datetime
else
  dbafilter_init%datetime=dbadatetime()
end if

if (present(level)) then
  dbafilter_init%level=level
else
  dbafilter_init%level=dbalevel()
end if

if (present(timerange)) then
  dbafilter_init%timerange=timerange
else
  dbafilter_init%timerange=dbatimerange()
end if

if (present(network)) then
  dbafilter_init%network=network
else
  dbafilter_init%network=dbanetwork()
end if

if (present(datetimemin)) then
  dbafilter_init%datetimemin=datetimemin
else
  dbafilter_init%datetimemin=dbadatetime()
end if

if (present(datetimemax)) then
  dbafilter_init%datetimemax=datetimemax
else
  dbafilter_init%datetimemax=dbadatetime()
end if

if (present(coordmin)) then
  dbafilter_init%coordmin=coordmin
else
  dbafilter_init%coordmin=dbacoord()
end if

if (present(coordmax)) then
  dbafilter_init%coordmax=coordmax
else
  dbafilter_init%coordmax=dbacoord()
end if

if (present(limit)) then
  dbafilter_init%limit=limit
else
  dbafilter_init%limit=imiss
end if

if (present(ana_filter)) then
  dbafilter_init%ana_filter=ana_filter
else
  dbafilter_init%ana_filter=cmiss
end if

if (present(data_filter)) then
  dbafilter_init%data_filter=data_filter
else
  dbafilter_init%data_filter=cmiss
end if

if (present(attr_filter)) then
  dbafilter_init%attr_filter=attr_filter
else
  dbafilter_init%attr_filter=cmiss
end if

if (present(varlist)) then
  dbafilter_init%varlist=varlist
else
  dbafilter_init%varlist=cmiss
end if

if (present(starvarlist)) then
  dbafilter_init%starvarlist=starvarlist
else
  dbafilter_init%starvarlist=cmiss
end if

if (present(vars)) then

  allocate(dbafilter_init%vars%dat(size(vars%dat)))
  do i =1,size(vars%dat)
    allocate(dbafilter_init%vars%dat(i)%dat,source=vars%dat(i)%dat)
  end do

  dbafilter_init%varlist=""
  do i=1,size(vars%dat) 
    dbafilter_init%varlist=trim(dbafilter_init%varlist)//vars%dat(i)%dat%btable
    if (i /=  size(vars%dat)) dbafilter_init%varlist=trim(dbafilter_init%varlist)//","
  end do
end if

if (present(starvars)) then
  allocate(dbafilter_init%starvars%dat(size(starvars%dat)))
  do i =1,size(starvars%dat)
    allocate(dbafilter_init%starvars%dat(i)%dat,source=starvars%dat(i)%dat)
  end do

  dbafilter_init%starvarlist=""
  do i=1,size(starvars%dat)
    dbafilter_init%starvarlist=trim(dbafilter_init%starvarlist)//starvars%dat(i)%dat%btable
    if (i /=  size(starvars%dat)) dbafilter_init%starvarlist=trim(dbafilter_init%starvarlist)//","
  end do
end if

if (present(priomin)) then
  dbafilter_init%priomin=priomax
else
  dbafilter_init%priomin=imiss
end if

if (present(priomax)) then
  dbafilter_init%priomax=priomax
else
  dbafilter_init%priomax=imiss
end if

if (present(contextana)) then
  dbafilter_init%contextana=contextana
else
  dbafilter_init%contextana=.false.
end if

end function dbafilter_init

subroutine dbafilter_display(filter)
class(dbafilter), intent(in) :: filter

call filter%ana%display()
call filter%datetime%display()
call filter%level%display()
call filter%timerange%display()
call filter%network%display()
call filter%datetimemin%display()
call filter%datetimemax%display()
call filter%coordmin%display()
call filter%coordmax%display()
print *,"var=",filter%var
print *,"limit=",filter%limit
print *,"ana_filter=",trim(filter%ana_filter)
print *,"data_filter=",trim(filter%data_filter)
print *,"attr_filter=",trim(filter%attr_filter)
print *,"varlist=",trim(filter%varlist)
print *,"*varlist=",trim(filter%starvarlist)
print *,"priomin=",filter%priomin
print *,"priomax=",filter%priomax
print *,"contextana=",filter%contextana

end subroutine dbafilter_display

subroutine dbafilter_set(filter,session)
class(dbafilter), intent(in) :: filter
type(dbasession), intent(in) :: session

integer :: ier,year,month,day,hour,minute,sec,msec

call session%unsetall()

call filter%ana%dbaset(session)
call filter%datetime%dbaset(session)
call filter%level%dbaset(session)
call filter%timerange%dbaset(session)
call filter%network%dbaset(session)
ier = idba_set(session%sehandle,"var",filter%var)

ier = idba_set(session%sehandle,"limit",filter%limit)
ier = idba_set(session%sehandle,"priomin",filter%priomin)
ier = idba_set(session%sehandle,"priomax",filter%priomax)

CALL getval(filter%datetimemin%datetime, year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)
if (c_e(msec)) then
  sec=nint(float(msec)/1000.)
else
  sec=imiss
end if

ier = idba_set(session%sehandle,"yearmin",year)
ier = idba_set(session%sehandle,"monthmin",month)
ier = idba_set(session%sehandle,"daymin",day)
ier = idba_set(session%sehandle,"hourmin",hour)
ier = idba_set(session%sehandle,"minumin",minute)
ier = idba_set(session%sehandle,"secmin",sec)

CALL getval(filter%datetimemax%datetime, year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)

if (c_e(msec)) then
  sec=nint(float(msec)/1000.)
else
  sec=imiss
end if

ier = idba_set(session%sehandle,"yearmax",year)
ier = idba_set(session%sehandle,"monthmax",month)
ier = idba_set(session%sehandle,"daymax",day)
ier = idba_set(session%sehandle,"hourmax",hour)
ier = idba_set(session%sehandle,"minumax",minute)
ier = idba_set(session%sehandle,"secmax",sec)

ier = idba_set(session%sehandle,"latmin",getilat(filter%coordmin%geo_coord))
ier = idba_set(session%sehandle,"lonmin",getilon(filter%coordmin%geo_coord))
ier = idba_set(session%sehandle,"latmax",getilat(filter%coordmax%geo_coord))
ier = idba_set(session%sehandle,"lonmax",getilon(filter%coordmax%geo_coord))

ier = idba_set(session%sehandle,"ana_filter",filter%ana_filter)
ier = idba_set(session%sehandle,"data_filter",filter%data_filter)
ier = idba_set(session%sehandle,"attr_filter",filter%attr_filter)
ier = idba_set(session%sehandle,"varlist",filter%varlist)
ier = idba_set(session%sehandle,"*varlist",filter%starvarlist)

if (filter%contextana) then
 call session%setcontextana()
end if

end subroutine dbafilter_set


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

call session%unsetall()
!write metadata
call session%set(metadata=metaanddata%metadata)
!write data and attribute
call session%ingest(dataattrv=metaanddata%dataattrv)
!to close message on file
call session%close_message()

end subroutine dbametaanddata_ingest


subroutine dbametaanddatav_display(metaanddatav)
class(dbametaanddatav), intent(in) :: metaanddatav

call metaanddatav%metadata%display()
call metaanddatav%datav%display()

end subroutine dbametaanddatav_display

subroutine dbametaanddatav_ingest(metaanddatav,session)
class(dbametaanddatav), intent(in) :: metaanddatav
type(dbasession), intent(in) :: session

!write metadata
call session%set(metadata=metaanddatav%metadata)
!write ana data and attribute
call session%set(datav=metaanddatav%datav)
!to close message on file
call session%close_message()

end subroutine dbametaanddatav_ingest


subroutine displaydbametaanddatai(this)
class(dbametaanddataiList) :: this
type(dbametaanddatai) :: element

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:"
  element=this%current()
  call element%display()
  call this%next()
end do
end subroutine displaydbametaanddatai

type(dbametaanddatai) function currentdbametaanddatai(this)
class(dbametaanddataiList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatai)
  currentdbametaanddatai = v
end select
end function currentdbametaanddatai


subroutine dbasession_extrude_metaanddatail(session,metaanddatal)
class(dbasession), intent(inout) :: session
type(dbametaanddatailist), intent(inout) :: metaanddatal
type(dbametaanddatai) :: element

call session%extrude_metaanddatai()

do while (session%count >0)
  call session%extrude_metaanddatai(element)
  call metaanddatal%append(element)
  call session%extrude_metaanddatai()
end do

end subroutine dbasession_extrude_metaanddatail


function toarray_dbametaanddatai(this)
type(dbametaanddatai),allocatable :: toarray_dbametaanddatai(:)
class(dbametaanddataiList) :: this

integer :: i

allocate (toarray_dbametaanddatai(this%countelements()))

call this%rewind()
i=0
do while(this%element())
  i=i+1
  toarray_dbametaanddatai(i) =this%current()
  call this%next()
end do
end function toarray_dbametaanddatai


subroutine displaydbametaanddatar(this)
class(dbametaanddatarList) :: this
type(dbametaanddatar) :: element

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:"
  element=this%current()
  call element%display()
  call this%next()
end do
end subroutine displaydbametaanddatar

type(dbametaanddatar) function currentdbametaanddatar(this)
class(dbametaanddatarList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatar)
  currentdbametaanddatar = v
end select
end function currentdbametaanddatar


subroutine dbasession_extrude_metaanddatarl(session,metaanddatal)
class(dbasession), intent(inout) :: session
type(dbametaanddatarlist), intent(inout) :: metaanddatal
type(dbametaanddatar) :: element

call session%extrude_metaanddatar()

do while (session%count >0)
  call session%extrude_metaanddatar(element)
  call metaanddatal%append(element)
  call session%extrude_metaanddatar()
end do

end subroutine dbasession_extrude_metaanddatarl


function toarray_dbametaanddatar(this)
type(dbametaanddatar),allocatable :: toarray_dbametaanddatar(:)
class(dbametaanddatarList) :: this

integer :: i
i=this%countelements()
!print *, "allocate:",i
allocate (toarray_dbametaanddatar(this%countelements()))

call this%rewind()
i=0
do while(this%element())
  i=i+1
  toarray_dbametaanddatar(i) =this%current()
  call this%next()
end do
end function toarray_dbametaanddatar



subroutine displaydbametaanddatad(this)
class(dbametaanddatadList) :: this
type(dbametaanddatad) :: element

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:"
  element=this%current()
  call element%display()
  call this%next()
end do
end subroutine displaydbametaanddatad

type(dbametaanddatad) function currentdbametaanddatad(this)
class(dbametaanddatadList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatad)
  currentdbametaanddatad = v
end select
end function currentdbametaanddatad


subroutine dbasession_extrude_metaanddatadl(session,metaanddatal)
class(dbasession), intent(inout) :: session
type(dbametaanddatadlist), intent(inout) :: metaanddatal
type(dbametaanddatad) :: element

call session%extrude_metaanddatad()

do while (session%count >0)
  call session%extrude_metaanddatad(element)
  call metaanddatal%append(element)
  call session%extrude_metaanddatad()
end do

end subroutine dbasession_extrude_metaanddatadl


function toarray_dbametaanddatad(this)
type(dbametaanddatad),allocatable :: toarray_dbametaanddatad(:)
class(dbametaanddatadList) :: this

integer :: i

allocate (toarray_dbametaanddatad(this%countelements()))

call this%rewind()
i=0
do while(this%element())
  i=i+1
  toarray_dbametaanddatad(i) =this%current()
  call this%next()
end do
end function toarray_dbametaanddatad


subroutine displaydbametaanddatab(this)
class(dbametaanddatabList) :: this
type(dbametaanddatab) :: element

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:"
  element=this%current()
  call element%display()
  call this%next()
end do
end subroutine displaydbametaanddatab

type(dbametaanddatab) function currentdbametaanddatab(this)
class(dbametaanddatabList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatab)
  currentdbametaanddatab = v
end select
end function currentdbametaanddatab


subroutine dbasession_extrude_metaanddatabl(session,metaanddatal)
class(dbasession), intent(inout) :: session
type(dbametaanddatablist), intent(inout) :: metaanddatal
type(dbametaanddatab) :: element

call session%extrude_metaanddatab()

do while (session%count >0)
  call session%extrude_metaanddatab(element)
  call metaanddatal%append(element)
  call session%extrude_metaanddatab()
end do

end subroutine dbasession_extrude_metaanddatabl


function toarray_dbametaanddatab(this)
type(dbametaanddatab),allocatable :: toarray_dbametaanddatab(:)
class(dbametaanddatabList) :: this

integer :: i

allocate (toarray_dbametaanddatab(this%countelements()))

call this%rewind()
i=0
do while(this%element())
  i=i+1
  toarray_dbametaanddatab(i) =this%current()
  call this%next()
end do
end function toarray_dbametaanddatab


subroutine displaydbametaanddatac(this)
class(dbametaanddatacList) :: this
type(dbametaanddatac) :: element

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:"
  element=this%current()
  call element%display()
  call this%next()
end do
end subroutine displaydbametaanddatac

type(dbametaanddatac) function currentdbametaanddatac(this)
class(dbametaanddatacList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatac)
  currentdbametaanddatac = v
end select
end function currentdbametaanddatac


subroutine dbasession_extrude_metaanddatacl(session,metaanddatal)
class(dbasession), intent(inout) :: session
type(dbametaanddataclist), intent(inout) :: metaanddatal
type(dbametaanddatac) :: element

call session%extrude_metaanddatac()

do while (session%count >0)
  call session%extrude_metaanddatac(element)
  call metaanddatal%append(element)
  call session%extrude_metaanddatac()
end do

end subroutine dbasession_extrude_metaanddatacl


function toarray_dbametaanddatac(this)
type(dbametaanddatac),allocatable :: toarray_dbametaanddatac(:)
class(dbametaanddatacList) :: this

integer :: i

allocate (toarray_dbametaanddatac(this%countelements()))

call this%rewind()
i=0
do while(this%element())
  i=i+1
  toarray_dbametaanddatac(i) =this%current()
  call this%next()
end do
end function toarray_dbametaanddatac




subroutine dbametaanddatai_display(data)
class(dbametaanddatai), intent(in) :: data

call data%metadata%display()
call data%dbadatai%display()

end subroutine dbametaanddatai_display

subroutine dbametaanddatab_display(data)
class(dbametaanddatab), intent(in) :: data

call data%metadata%display()
call data%dbadatab%display()

end subroutine dbametaanddatab_display

subroutine dbametaanddatad_display(data)
class(dbametaanddatad), intent(in) :: data

call data%metadata%display()
call data%dbadatad%display()

end subroutine dbametaanddatad_display

subroutine dbametaanddatar_display(data)
class(dbametaanddatar), intent(in) :: data

call data%metadata%display()
call data%dbadatar%display()

end subroutine dbametaanddatar_display


subroutine dbametaanddatac_display(data)
class(dbametaanddatac), intent(in) :: data

call data%metadata%display()
call data%dbadatac%display()

end subroutine dbametaanddatac_display


subroutine dbametaanddatai_ingest(metaanddatai,session)
class(dbametaanddatai), intent(in) :: metaanddatai
type(dbasession), intent(in) :: session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatai%metadata)
!write ana data and attribute
call session%set(data=metaanddatai%dbadatai)
call session%prendilo()

end subroutine dbametaanddatai_ingest

subroutine dbametaanddatab_ingest(metaanddatab,session)
class(dbametaanddatab), intent(in) :: metaanddatab
type(dbasession), intent(in) :: session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatab%metadata)
!write ana data and attribute
call session%set(data=metaanddatab%dbadatab)
call session%prendilo()

end subroutine dbametaanddatab_ingest

subroutine dbametaanddatad_ingest(metaanddatad,session)
class(dbametaanddatad), intent(in) :: metaanddatad
type(dbasession), intent(in) :: session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatad%metadata)
!write ana data and attribute
call session%set(data=metaanddatad%dbadatad)
call session%prendilo()

end subroutine dbametaanddatad_ingest

subroutine dbametaanddatar_ingest(metaanddatar,session)
class(dbametaanddatar), intent(in) :: metaanddatar
type(dbasession), intent(in) :: session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatar%metadata)
!write ana data and attribute
call session%set(data=metaanddatar%dbadatar)
call session%prendilo()

end subroutine dbametaanddatar_ingest

subroutine dbametaanddatac_ingest(metaanddatac,session)
class(dbametaanddatac), intent(in) :: metaanddatac
type(dbasession), intent(in) :: session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatac%metadata)
!write ana data and attribute
call session%set(data=metaanddatac%dbadatac)
call session%prendilo()

end subroutine dbametaanddatac_ingest


subroutine dbasession_extrude_ana(session,ana)
class(dbasession), intent(inout) :: session
type(dbaana), intent(out),optional :: ana

integer :: ier

if (.not. present(ana)) then
  ier = idba_quantesono(session%sehandle, session%count)
  !print *,"numero ana",session%count
else
  ier = idba_elencamele(session%sehandle)
  call ana%dbaenq(session)
  session%count=session%count-1
end if

end subroutine dbasession_extrude_ana


subroutine dbasession_extrude_anav(session,anav)
class(dbasession), intent(inout) :: session
type(dbaana), intent(out),allocatable :: anav(:)
integer :: i

call session%extrude_ana()

if (c_e(session%count)) then
  allocate(anav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%extrude_ana(anav(i))
  end do
else
  allocate(anav(0))
end if

end subroutine dbasession_extrude_anav


subroutine dbasession_extrude_anal(session,anal)
class(dbasession), intent(inout) :: session
type(dbaanalist), intent(out) :: anal
type(dbaana) :: element

call session%extrude_ana()
if (c_e(session%count))then
  do while (session%count >0)
    call session%extrude_ana(element)
    call anal%append(element)
    call session%extrude_ana()
  end do
end if
end subroutine dbasession_extrude_anal


subroutine dbasession_extrude_metaanddata(session,metaanddata,dataonly,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddata), intent(inout),optional :: metaanddata
logical,intent(in),optional :: dataonly
type(dbafilter),intent(in),optional :: filter

type(dbametadata) :: metadata
integer :: ier,acount,i,j,k
character(len=9) :: btable
character(255) :: value
logical :: lvars,lstarvars

                                ! if you do not pass metaanddata we presume to have to initialize the query
if (.not. present(metaanddata)) then
  ier = idba_voglioquesto(session%sehandle, session%count)
!  print *,"extrude voglioquesto number data",session%count

                                ! preroll one read because after I have to read one more to check metadata
  if (c_e(session%count) .and. session%count > 0) then
    ier = idba_dammelo(session%sehandle, btable)
  end if

else

!  print *,"extrude dammelo"
                                ! you pass metaanddata so we continue with the query

  if (allocated(metaanddata%dataattrv%dataattr)) then
    deallocate (metaanddata%dataattrv%dataattr)
  end if

  lvars=.false.
  if (present(filter)) then
    if (allocated(filter%vars%dat)) then
      lvars=.true.
    end if
  end if
  lstarvars=.false.
  if (present(filter)) then
    if (allocated(filter%starvars%dat)) then
      lstarvars=.true.
    end if
  end if

  if (lvars) then

    ! create an empty vector for data 
    allocate (metaanddata%dataattrv%dataattr(size(filter%vars%dat)))
    do i = 1, size(filter%vars%dat)
      allocate (metaanddata%dataattrv%dataattr(i)%dat,source=filter%vars%dat(i)%dat)
      ! initialize to (0) the attribute vector
      allocate (metaanddata%dataattrv%dataattr(i)%attrv%dat(0))
    end do

    ! load metadata
    call metaanddata%metadata%dbaenq(session)
    ! load curret metadata
    call metadata%dbaenq(session)

    ! if current metadata is equal to metadata
    do while ( metaanddata%metadata == metadata )
      ier = idba_enq(session%sehandle,"var",btable)
      do i=1,size(metaanddata%dataattrv%dataattr)
        if (metaanddata%dataattrv%dataattr(i)%dat%btable == btable) then

          select type ( dat =>  metaanddata%dataattrv%dataattr(i)%dat )
          type is (dbadatai)
            ier = idba_enq(session%sehandle, btable,dat%value)
          type is (dbadatar)
            ier = idba_enq(session%sehandle, btable,dat%value)
          type is (dbadatad)
            ier = idba_enq(session%sehandle, btable,dat%value)
          type is (dbadatab)
            ier = idba_enq(session%sehandle, btable,dat%value)
          type is (dbadatac)
            ier = idba_enq(session%sehandle, btable,dat%value)
          end select
          
          if (.not. optio_log(dataonly))then

            deallocate (metaanddata%dataattrv%dataattr(i)%attrv%dat)

            if (lstarvars) then

              allocate (metaanddata%dataattrv%dataattr(i)%attrv%dat(size(filter%starvars%dat)))
              do j = 1, size(filter%starvars%dat)
                allocate (metaanddata%dataattrv%dataattr(i)%attrv%dat(j)%dat,source=filter%starvars%dat(j)%dat)
              end do

              if (c_e(session%count) .and. session%count > 0) then
              
                ier = idba_voglioancora(session%sehandle, acount)
                do k =1,acount
                  ier = idba_ancora(session%sehandle, btable)
                  ier = idba_enq(session%sehandle, btable,value)
              
              
                  do j=1,size(metaanddata%dataattrv%dataattr(i)%attrv%dat)
                    if (metaanddata%dataattrv%dataattr(i)%attrv%dat(j)%dat%btable == btable) then
                    
                      select type ( dat =>  metaanddata%dataattrv%dataattr(i)%attrv%dat(j)%dat )
                      type is (dbadatai)
                        ier = idba_enq(session%sehandle, btable,dat%value)
                      type is (dbadatar)
                        ier = idba_enq(session%sehandle, btable,dat%value)
                      type is (dbadatad)
                        ier = idba_enq(session%sehandle, btable,dat%value)
                      type is (dbadatab)
                        ier = idba_enq(session%sehandle, btable,dat%value)
                      type is (dbadatac)
                        ier = idba_enq(session%sehandle, btable,dat%value)
                      end select
                  
                    end if
                  end do
                end do
              end if
            else
              if (c_e(session%count) .and. session%count > 0) then
                ier = idba_voglioancora(session%sehandle, acount)
              
                allocate (metaanddata%dataattrv%dataattr(i)%attrv%dat(acount))
                do j =1,acount
                  ier = idba_ancora(session%sehandle, btable)
                  ier = idba_enq(session%sehandle, btable,value)
                  allocate (metaanddata%dataattrv%dataattr(i)%attrv%dat(j)%dat,source=dbadatac(btable,value))
                end do
              else
                allocate (metaanddata%dataattrv%dataattr(i)%attrv%dat(0))
              end if
            end if
          end if
        end if
      end do
      
      if (c_e(session%count)) session%count=session%count-1
      
      if (c_e(session%count) .and. session%count > 0 ) then
        ier = idba_dammelo(session%sehandle, btable)
        call metadata%dbaenq(session)
      else
        metadata=dbametadata()
      end if
    end do
  else

    allocate (metaanddata%dataattrv%dataattr(1))
    ier = idba_enq(session%sehandle,"var",btable)
    ier = idba_enq(session%sehandle, btable,value)
    allocate (metaanddata%dataattrv%dataattr(1)%dat,source=dbadatac(btable,value))
    call metaanddata%metadata%dbaenq(session)


    if (optio_log(dataonly))then

      allocate (metaanddata%dataattrv%dataattr(1)%attrv%dat(0))

    else

      if (lstarvars) then

        allocate (metaanddata%dataattrv%dataattr(1)%attrv%dat(size(filter%starvars%dat)))
        do j = 1, size(filter%starvars%dat)
          allocate (metaanddata%dataattrv%dataattr(1)%attrv%dat(j)%dat,source=filter%starvars%dat(j)%dat)
        end do
        
        ier = idba_voglioancora(session%sehandle, acount)
        if (acount > 0) then
          ier = idba_ancora(session%sehandle, btable)
          ier = idba_enq(session%sehandle, btable,value)
        
          do j=1,size(metaanddata%dataattrv%dataattr(1)%attrv%dat)
            if (metaanddata%dataattrv%dataattr(1)%attrv%dat(j)%dat%btable == btable) then
              
              select type ( dat =>  metaanddata%dataattrv%dataattr(1)%attrv%dat(j)%dat )
              type is (dbadatai)
                ier = idba_enq(session%sehandle, btable,dat%value)
              type is (dbadatar)
                ier = idba_enq(session%sehandle, btable,dat%value)
              type is (dbadatad)
                ier = idba_enq(session%sehandle, btable,dat%value)
              type is (dbadatab)
                ier = idba_enq(session%sehandle, btable,dat%value)
              type is (dbadatac)
                ier = idba_enq(session%sehandle, btable,dat%value)
              end select
            end if
          end do
        end if
      else

        if (c_e(session%count) .and. session%count > 0 ) then
          ier = idba_voglioancora(session%sehandle, acount)
        
          allocate (metaanddata%dataattrv%dataattr(1)%attrv%dat(acount))
          do j =1,acount
            ier = idba_ancora(session%sehandle, btable)
            ier = idba_enq(session%sehandle, btable,value)
            allocate (metaanddata%dataattrv%dataattr(1)%attrv%dat(j)%dat,source=dbadatac(btable,value))
          end do
        else
          allocate (metaanddata%dataattrv%dataattr(1)%attrv%dat(0))
        end if
      end if
    end if

    if (c_e(session%count)) then
      session%count=session%count-1

      if (session%count > 0 ) then
        ier = idba_dammelo(session%sehandle, btable)
      end if
    end if
  end if
end if

end subroutine dbasession_extrude_metaanddata


subroutine dbasession_extrude_metaanddatav(session,metaanddatav,dataonly,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddata), intent(inout),allocatable :: metaanddatav(:)
logical, intent(in),optional :: dataonly
type(dbafilter),intent(in),optional :: filter

type(dbametaanddata),allocatable :: metaanddatavbuf(:)
integer :: i

call session%extrude()
!print*," count: ",session%count

if (c_e(session%count)) then
  ! allocate to max dimension
  allocate(metaanddatavbuf(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%extrude(metaanddatavbuf(i),dataonly=dataonly,filter=filter)
  end do

  ! compact data to real dimension
  ! allocate (metaanddatav(i))
  metaanddatav=metaanddatavbuf(:i)
  deallocate(metaanddatavbuf)

else
  allocate(metaanddatav(0))
end if


end subroutine dbasession_extrude_metaanddatav

!> extrude metaanddata list
subroutine dbasession_extrude_metaanddatal(session,metaanddatal,dataonly,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddatalist), intent(out) :: metaanddatal
logical, intent(in),optional :: dataonly
type(dbafilter),intent(in),optional :: filter

type(dbametaanddata),allocatable :: metaanddatavbuf(:)
integer :: i

if (session%memdb)then

  do while (session%messages_read_next())
    call session%set(filter=filter)
    call session%extrude()
    call session%extrude(metaanddatavbuf,dataonly=dataonly,filter=filter)
    do i=1,size(metaanddatavbuf)
      call metaanddatal%append(metaanddatavbuf(i))
    end do
    
    call session%remove_all()
    deallocate (metaanddatavbuf)
  end do

else

  call session%extrude()
  
  do while (c_e(session%count) .and. session%count >0)
    call session%extrude(metaanddatavbuf,dataonly=dataonly,filter=filter)
    do i=1,size(metaanddatavbuf)
      if (present(filter)) then 
                                ! exclude contextana data from file 
        if (filter%contextana) then
          if (datetime_new() /= metaanddatavbuf(i)%metadata%datetime%datetime) cycle
        end if
      end if
      call metaanddatal%append(metaanddatavbuf(i))
    end do
    if (session%file) call session%extrude()
    deallocate (metaanddatavbuf)
  end do
end if

end subroutine dbasession_extrude_metaanddatal

subroutine dbasession_extrude_metaanddatai(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatai), intent(inout),optional :: metaanddata

integer :: ier
character(len=9) :: btable
integer :: value

if (.not. present(metaanddata)) then
  ier = idba_voglioquesto(session%sehandle, session%count)
else
  ier = idba_dammelo(session%sehandle, btable)
  ier = idba_enq(session%sehandle, btable,value)
  metaanddata%dbadatai=dbadatai(btable,value)
  call metaanddata%metadata%dbaenq(session)
  session%count=session%count-1
end if
end subroutine dbasession_extrude_metaanddatai


subroutine dbasession_extrude_metaanddataiv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatai), intent(inout),allocatable :: metaanddatav(:)

integer :: i

call session%extrude_metaanddatai()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%extrude_metaanddatai(metaanddatav(i))
  end do
else
  allocate(metaanddatav(0))
end if

end subroutine dbasession_extrude_metaanddataiv


subroutine dbasession_extrude_metaanddatab(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatab), intent(inout),optional :: metaanddata

integer :: ier
character(len=9) :: btable
integer(kind=int_b) :: value

if (.not. present(metaanddata)) then
  ier = idba_voglioquesto(session%sehandle, session%count)
else
  ier = idba_dammelo(session%sehandle, btable)
  ier = idba_enq(session%sehandle, btable,value)
  metaanddata%dbadatab=dbadatab(btable,value)
  call metaanddata%metadata%dbaenq(session)
  session%count=session%count-1
end if
end subroutine dbasession_extrude_metaanddatab


subroutine dbasession_extrude_metaanddatabv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatab), intent(inout),allocatable :: metaanddatav(:)

integer :: i

call session%extrude_metaanddatab()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%extrude_metaanddatab(metaanddatav(i))
  end do
else
  allocate(metaanddatav(0))
end if

end subroutine dbasession_extrude_metaanddatabv


subroutine dbasession_extrude_metaanddatad(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatad), intent(inout),optional :: metaanddata

integer :: ier
character(len=9) :: btable
doubleprecision :: value

if (.not. present(metaanddata)) then
  ier = idba_voglioquesto(session%sehandle, session%count)
else
  ier = idba_dammelo(session%sehandle, btable)
  ier = idba_enq(session%sehandle, btable,value)
  metaanddata%dbadatad=dbadatad(btable,value)
  call metaanddata%metadata%dbaenq(session)
  session%count=session%count-1
end if
end subroutine dbasession_extrude_metaanddatad


subroutine dbasession_extrude_metaanddatadv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatad), intent(inout),allocatable :: metaanddatav(:)

integer :: i

call session%extrude_metaanddatad()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%extrude_metaanddatad(metaanddatav(i))
  end do
else
  allocate(metaanddatav(0))
end if
end subroutine dbasession_extrude_metaanddatadv


subroutine dbasession_extrude_metaanddatar(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatar), intent(inout),optional :: metaanddata

integer :: ier
character(len=9) :: btable
real :: value

if (.not. present(metaanddata)) then
  ier = idba_voglioquesto(session%sehandle, session%count)
else
  ier = idba_dammelo(session%sehandle, btable)
  ier = idba_enq(session%sehandle, btable,value)
  metaanddata%dbadatar=dbadatar(btable,value)
  call metaanddata%metadata%dbaenq(session)
  session%count=session%count-1
end if
end subroutine dbasession_extrude_metaanddatar


subroutine dbasession_extrude_metaanddatarv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatar), intent(inout),allocatable :: metaanddatav(:)

integer :: i

call session%extrude_metaanddatar()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%extrude_metaanddatar(metaanddatav(i))
  end do
else
  allocate(metaanddatav(0))
end if
end subroutine dbasession_extrude_metaanddatarv



subroutine dbasession_extrude_metaanddatac(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatac), intent(inout),optional :: metaanddata

integer :: ier
character(len=9) :: btable
character(len=255) :: value

if (.not. present(metaanddata)) then
  ier = idba_voglioquesto(session%sehandle, session%count)
else
  ier = idba_dammelo(session%sehandle, btable)
  ier = idba_enq(session%sehandle, btable,value)
  metaanddata%dbadatac=dbadatac(btable,value)
  call metaanddata%metadata%dbaenq(session)
  session%count=session%count-1
end if
end subroutine dbasession_extrude_metaanddatac


subroutine dbasession_extrude_metaanddatacv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatac), intent(inout),allocatable :: metaanddatav(:)

integer :: i

call session%extrude_metaanddatac()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%extrude_metaanddatac(metaanddatav(i))
  end do
else
  allocate(metaanddatav(session%count))
end if
end subroutine dbasession_extrude_metaanddatacv


type(dbaconnection) function dbaconnection_init(dsn, user, password,categoryappend,idbhandle)
character (len=*), intent(in), optional :: dsn
character (len=*), intent(in), optional :: user
character (len=*), intent(in), optional :: password
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
integer,INTENT(in),OPTIONAL :: idbhandle !< dsn connection; if present it will be used
character(len=50) :: quidsn,quiuser,quipassword
integer :: ier
character(len=512) :: a_name

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

if (c_e(handle%dbhandle)) then 
  ier = idba_arrivederci(handle%dbhandle)
end if

end subroutine dbaconnection_delete

recursive type(dbasession) function dbasession_init(connection,anaflag, dataflag, attrflag,&
 filename,mode,format,template,write,wipe,repinfo,simplified,memdb,categoryappend)
type(dbaconnection),intent(in),optional :: connection
character (len=*), intent(in), optional :: anaflag
character (len=*), intent(in), optional :: dataflag
character (len=*), intent(in), optional :: attrflag
character (len=*), intent(in), optional :: filename
character (len=*), intent(in), optional :: mode !> "r"/"w"/"a" the open mode ("r" for read, "w" for write or create, "a" for append) 
character (len=*), intent(in), optional :: template !> template to use for write on file
logical,INTENT(in),OPTIONAL :: write !< abilita la scrittura sul DSN/file ( default=.false. )
logical,INTENT(in),OPTIONAL :: wipe !<  svuota il DSN/file e/o lo prepara per una scrittura ( default=.false. )
character(len=*), INTENT(in),OPTIONAL :: repinfo !< eventuale file repinfo.csv usato con wipe ( default="" )
character(len=*),intent(in),optional :: format !< the file format. It can be "BUFR" or "CREX". (default="BUFR")
logical,intent(in),optional :: simplified !< rappresentation for interpreted message (simplified/precise)
logical,intent(in),optional :: memdb     !< if true import from file to memdb (default=true if filename present else false)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

integer :: ier
character (len=5) :: lanaflag,ldataflag,lattrflag
character (len=1) :: lmode
logical :: quiwrite,quiwipe
character(len=255) :: quirepinfo
character(len=40) :: lformat
logical :: exist,lsimplified
character(len=512) :: a_name

! those are assigned by the default constructor?
!!$dbasession_init%sehandle=imiss
!!$dbasession_init%file=.false.
!!$dbasession_init%template=cmiss
!!$dbasession_init%count=imiss

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

  if (present(template))then
    dbasession_init%template=template
  endif
  
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

  lformat="BUFR"
  if (present(format))then
    lformat=format
  end if

  lsimplified=.true.
  if (present(simplified))then
    lsimplified=simplified
  end if

  
  dbasession_init%file=.true.
  dbasession_init%filename=filename
  dbasession_init%mode=lmode
  dbasession_init%format=lformat
  dbasession_init%simplified=lsimplified

  dbasession_init%memdb=.true. ! default with filename
  if (present(memdb))then
    dbasession_init%memdb=memdb
  end if

  if (dbasession_init%memdb)then
                                ! connect to dsn type DBA
    dbasession_init%memconnection=dbaconnection(dsn="mem:")
    dbasession_init=dbasession(dbasession_init%memconnection,anaflag=anaflag, dataflag=dataflag, attrflag=attrflag)
    call dbasession_init%messages_open_input(filename=filename,mode=lmode,format=lformat,simplified=lsimplified)

  else

    if (present(anaflag).or.present(dataflag).or.present( attrflag)) then
      call l4f_category_log(dbasession_init%category,L4F_ERROR,"option anaflag, dataflag, attrflag defined with filename")
      CALL raise_error()
    end if
  
    ier =  idba_messaggi(dbasession_init%sehandle,filename, lmode, lformat)

  end if

else
  
  if (.not. present(connection)) then
    call l4f_category_log(dbasession_init%category,L4F_ERROR,"connection not present accessing DBA")
    CALL raise_error()
  end if

  if (present(mode).or.present(format).or.present(template)) then
    call l4f_category_log(dbasession_init%category,L4F_ERROR,"option mode or format or template defined without filename")
    CALL raise_error()
  end if

  call optio(anaflag,lanaflag)
  if (.not. c_e(lanaflag)) lanaflag = "read"

  call optio(dataflag,ldataflag)
  if (.not. c_e(ldataflag)) ldataflag = "read"

  call optio(attrflag,lattrflag)
  if (.not. c_e(lattrflag)) lattrflag = "read"

  ier = idba_preparati(connection%dbhandle,dbasession_init%sehandle, lanaflag, ldataflag, lattrflag)

  dbasession_init%file=.false.

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



subroutine dbasession_remove_all(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then
  ier = idba_remove_all(session%sehandle)
end if

end subroutine dbasession_remove_all



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


subroutine dbasession_dimenticami(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_dimenticami(session%sehandle)
end if

end subroutine dbasession_dimenticami


subroutine dbasession_critica(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_critica(session%sehandle)
end if

end subroutine dbasession_critica

subroutine dbasession_scusa(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_scusa(session%sehandle)
end if

end subroutine dbasession_scusa

subroutine dbasession_set(session,metadata,datav,data,datetime,ana,network,level,timerange,filter)
class (dbasession), intent(in) :: session
type (dbametadata),optional :: metadata
class(dbadatav),optional :: datav
class(dbadata),optional :: data
type (dbadatetime),optional :: datetime
type (dbaana),optional :: ana
type (dbanetwork),optional :: network
type (dbalevel),optional :: level
type (dbatimerange),optional :: timerange
type (dbafilter),optional :: filter

if (present(metadata)) then
  call metadata%dbaset(session)
endif

if (present(datetime)) then
  call datetime%dbaset(session)
endif

if (present(ana)) then
  call ana%dbaset(session)
endif

if (present(network)) then
  call network%dbaset(session)
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

if (present(filter)) then
  call filter%dbaset(session)
end if

end subroutine dbasession_set


subroutine dbasession_ingest(session,ana,dataattr,dataattrv,metaanddata,&
 metaanddatai,metaanddatab,metaanddatad,metaanddatac,metaanddatar,&
 metaanddatav)
class(dbasession), intent(in)   :: session
class(dbaana),optional     :: ana
class(dbadataattr),optional     :: dataattr
class(dbadataattrv),optional    :: dataattrv
class(dbametaanddata),optional  :: metaanddata
class(dbametaanddatai),optional   :: metaanddatai
class(dbametaanddatab),optional   :: metaanddatab
class(dbametaanddatad),optional   :: metaanddatad
class(dbametaanddatac),optional   :: metaanddatac
class(dbametaanddatar),optional   :: metaanddatar
class(dbametaanddatav),optional :: metaanddatav

if (present(ana)) then
  call ana%dbaingest(session)
end if

if (present(dataattr)) then
  call dataattr%dbaingest(session)
end if

if (present(dataattrv)) then
  call dataattrv%dbaingest(session)
end if

if (present(metaanddata)) then
  call metaanddata%dbaingest(session)
end if

if (present(metaanddatai)) then
  call metaanddatai%dbaingest(session)
end if

if (present(metaanddatab)) then
  call metaanddatab%dbaingest(session)
end if

if (present(metaanddatad)) then
  call metaanddatad%dbaingest(session)
end if

if (present(metaanddatac)) then
  call metaanddatac%dbaingest(session)
end if

if (present(metaanddatar)) then
  call metaanddatar%dbaingest(session)
end if

if (present(metaanddatav)) then
  call metaanddatav%dbaingest(session)
end if

end subroutine dbasession_ingest


subroutine dbasession_delete(session)
class (dbasession), intent(inout) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_fatto(session%sehandle)
end if

call session%memconnection%delete()


session%sehandle=imiss
session%file=.false.
session%template=cmiss
session%filename=cmiss
session%mode=cmiss
session%format=cmiss
session%simplified=.true.
session%memdb=.false.
session%category=imiss
session%count=imiss

end subroutine dbasession_delete


subroutine dbasession_filerewind(session)
class (dbasession), intent(inout) :: session
integer :: ier

if (c_e(session%sehandle).and. session%file) then 
  ier = idba_fatto(session%sehandle)
  ier = idba_messaggi(session%sehandle,session%filename,session%mode,session%format)

!!$! example: here we call constructor after a cast to reassign self (can you pass self attributes to constructor?) 
!!$  select type(session)
!!$  type is (dbasession)
!!$    session=dbasession(filename=session%filename,mode=session%mode,format=session%format)
!!$  end select
  
end if

end subroutine dbasession_filerewind


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
