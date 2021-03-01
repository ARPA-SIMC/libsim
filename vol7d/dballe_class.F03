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

!> \brief  class for import and export data from e to DB-All.e. 
!! This module define objects and methods to manage
!! import and export of data from database for sparse data DB-All.e
!!
!! The main usefull structure is this:
!!\verbatim
!!                     %timerange
!!                     %ana
!!                     %network
!!                     %datetime
!! metaanddata%metadata%level
!!            %dataattrv%dataattr(*)%dat
!!                                  %attrv%dcv(*)%dat
!!
!!\endverbatim
!! You can use a vector of this structure to archive a full dataset in memory.
!!
!! The program example is the better starting point:
!!\include example_dballe.F03
!!
!!\ingroup vol7d
!!
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
use vol7d_class, only: vol7d_cdatalen
use dballef
IMPLICIT NONE

private

character (len=255),parameter:: subcategory="dballe_class"

!> manage connection handle to a DSN
type,public :: dbaconnection
  integer :: dbhandle=imiss !< dballe DB handle
  integer :: handle_err=imiss !< dballe error handler
  integer :: category=0 !< log4fortran
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbaconnection_delete
# else
    procedure :: delete => dbaconnection_delete !< todo
# endif
end type dbaconnection

!> User-defined constructors => dballe_class::dbaconnection_init
interface dbaconnection
  procedure  dbaconnection_init !< add constructor to shape generic interface
end interface

!> manage session handle
type,public ::  dbasession
  integer :: sehandle=imiss !< session handler
  logical :: file=.false. !< is it a file?
  character(len=40) :: template='generic' !< template for BUFR format
  character(len=255) :: filename=cmiss !< filename
  character(len=40) :: mode=cmiss !< mode for open file
  character(len=40) :: format=cmiss !< format (bufr/crex)
  logical           :: simplified=.true. !< how to interpret input data
  logical           :: memdb=.false. !<  use memdb
  logical           :: loadfile=.false. !<  load data in file into memdb
  type(dbaconnection) :: memconnection !< connection for memdb
  integer :: category=0 !< log4fortran category
  integer :: count=imiss !< number of results left to query
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbasession_delete
# else
    procedure :: delete => dbasession_delete !< todo
# endif
    procedure :: unsetall => dbasession_unsetall !< dballe unsetall
    procedure :: remove_all => dbasession_remove_all !< dballe remove_all
    procedure :: set => dbasession_set !< set dballe parameters
    procedure :: setcontextana => dbasession_setcontextana !< set dballe station data context
    procedure :: dimenticami => dbasession_dimenticami !< dballe dimenticami
!!$    procedure :: extrude_dbasession => dbasession_extrude !< put data on DSN
!!$    procedure :: extrude_ana => dbasession_extrude_ana !< put data on DSN
!!$    procedure :: extrude_dataattr => dbasession_extrude_dataattr !< put data on DSN
!!$    procedure :: extrude_dataattrv => dbasession_extrude_dataattrv !< put data on DSN
!!$    procedure :: extrude_metaanddata => dbasession_extrude_metaanddata !< put data on DSN
!!$    procedure :: extrude_metaanddatai => dbasession_extrude_metaanddatai !< put data on DSN
!!$    procedure :: extrude_metaanddatar => dbasession_extrude_metaanddatar !< put data on DSN
!!$    procedure :: extrude_metaanddatad => dbasession_extrude_metaanddatad !< put data on DSN
!!$    procedure :: extrude_metaanddatab => dbasession_extrude_metaanddatab !< put data on DSN
!!$    procedure :: extrude_metaanddatac => dbasession_extrude_metaanddatac !< put data on DSN
!!$    procedure :: extrude_metaanddatav => dbasession_extrude_metaanddatav !< put data on DSN
!!$    procedure :: extrude_metaanddatal => dbasession_extrude_metaanddatal !< put data on DSN
    procedure :: prendilo => dbasession_prendilo !< dballe prendilo
    procedure :: var_related => dbasession_var_related !< dballe var_related
    procedure :: critica => dbasession_critica !< dballe critica
    procedure :: scusa => dbasession_scusa !< dballe scusa
    procedure :: messages_open_input => dbasession_messages_open_input !< dballe messages_open_input
    procedure :: messages_open_output => dbasession_messages_open_output !< dballe messages_open_output
    procedure :: messages_read_next => dbasession_messages_read_next !< dballe messages_read_next
    procedure :: messages_write_next => dbasession_messages_write_next !< dballe messages_write_next
    procedure :: close_message => dbasession_close_message !< dballe close_message
    procedure :: unsetb => dbasession_unsetb !< dballe unsetb
    procedure :: filerewind => dbasession_filerewind !< rewind file associates with this session
    procedure :: ingest_ana => dbasession_ingest_ana !< get station metadata from DSN
    procedure :: ingest_anav => dbasession_ingest_anav !< get station metadata from DSN
    procedure :: ingest_anal => dbasession_ingest_anal !< get station metadata from DSN
    procedure :: ingest_metaanddata => dbasession_ingest_metaanddata !< get data from DSN
    procedure :: ingest_metaanddatal => dbasession_ingest_metaanddatal !< get data from DSN
    procedure :: ingest_metaanddatav => dbasession_ingest_metaanddatav !< get data from DSN
    procedure :: ingest_metaanddatai => dbasession_ingest_metaanddatai !< get data from DSN
    procedure :: ingest_metaanddataiv => dbasession_ingest_metaanddataiv !< get data from DSN
    procedure :: ingest_metaanddatail => dbasession_ingest_metaanddatail !< get data from DSN
    procedure :: ingest_metaanddatab => dbasession_ingest_metaanddatab !< get data from DSN
    procedure :: ingest_metaanddatabv => dbasession_ingest_metaanddatabv !< get data from DSN
    procedure :: ingest_metaanddatabl => dbasession_ingest_metaanddatabl !< get data from DSN
    procedure :: ingest_metaanddatad => dbasession_ingest_metaanddatad !< get data from DSN
    procedure :: ingest_metaanddatadv => dbasession_ingest_metaanddatadv !< get data from DSN
    procedure :: ingest_metaanddatadl => dbasession_ingest_metaanddatadl !< get data from DSN
    procedure :: ingest_metaanddatar => dbasession_ingest_metaanddatar !< get data from DSN
    procedure :: ingest_metaanddatarv => dbasession_ingest_metaanddatarv !< get data from DSN
    procedure :: ingest_metaanddatarl => dbasession_ingest_metaanddatarl !< get data from DSN
    procedure :: ingest_metaanddatac => dbasession_ingest_metaanddatac !< get data from DSN
    procedure :: ingest_metaanddatacv => dbasession_ingest_metaanddatacv !< get data from DSN
    procedure :: ingest_metaanddatacl => dbasession_ingest_metaanddatacl !< get data from DSN
    procedure :: dissolve_metadata => dbasession_dissolve_metadata !< remove data from DSN
    procedure :: dissolveattr => dbasession_dissolveattr_metadata !< remove attributes from DSN
    generic   :: dissolve => dissolve_metadata ,dimenticami !< remove from DSN
    generic   :: ingesta => ingest_ana, ingest_anav,ingest_anal !< get station metadata from DSN
    generic   :: ingest  => ingest_metaanddata,ingest_metaanddatav,ingest_metaanddatal,&
     !ingest_metaanddatai,ingest_metaanddatab,ingest_metaanddatad,ingest_metaanddatar,ingest_metaanddatac,&      !ambiguos
     ingest_metaanddataiv,ingest_metaanddatabv,ingest_metaanddatadv,ingest_metaanddatarv,ingest_metaanddatacv,&
     ingest_metaanddatail,ingest_metaanddatarl,ingest_metaanddatadl,ingest_metaanddatabl,ingest_metaanddatacl !< get data from DSN
!!$    generic   :: extrude => extrude_ana,extrude_dataattr,extrude_dataattrv,extrude_metaanddata,&
!!$     extrude_metaanddatai,extrude_metaanddatar,extrude_metaanddatad,extrude_metaanddatab,extrude_metaanddatac,&
!!$     extrude_metaanddatav,extrude_metaanddatal
end type dbasession

!> User-defined constructors => dballe_class::dbasession_init
interface dbasession
  procedure  dbasession_init     !< add constructor to shape generic interface
end interface

!> level metadata
type,public,extends(vol7d_level) :: dbalevel
  contains

# ifdef F2003_FULL_FEATURES
!    final :: dbalevel_delete
# else
!    procedure :: delete => dbalevel_delete !< todo
# endif
  procedure :: display => dbalevel_display !< print a summary of object contents
  procedure :: dbaset => dbalevel_set  !< set dballe parameters
  procedure :: dbaenq => dbalevel_enq  !< query dballe parameters
  procedure,nopass :: dbacontextana => dbalevel_contextana !< set dballe station data context for level  (in object, not dballe session)
!!$procedure :: spiega => dbalevel_spiega
end type dbalevel

!> User-defined constructors => dballe_class::dbalevel_init
interface dbalevel
  procedure   dbalevel_init     !< add constructor to shape generic interface
end interface

!> timerange metadata
type,public,extends(vol7d_timerange) :: dbatimerange
  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbatimerange_delete
# else
!    procedure :: delete => dbatimerange_delete
# endif
  procedure :: display => dbatimerange_display !< print a summary of object contents
  procedure :: dbaset => dbatimerange_set  !< set dballe parameters
  procedure :: dbaenq => dbatimerange_enq  !< query dballe parameters
  procedure,nopass :: dbacontextana => dbatimerange_contextana !< set dballe station data context for timerange (in object, not dballe session)
!!$procedure :: spiega => dbatimerange_spiega
end type dbatimerange

!> User-defined constructors => dballe_class::dbatimerange_init
interface dbatimerange
  procedure   dbatimerange_init     !< add constructor to shape generic interface
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
  procedure :: display => dbacoord_display !< print a summary of object contents

end type dbacoord

!> User-defined constructors => dballe_class::dbacoord_init
interface dbacoord
  procedure   dbacoord_init     !< add constructor to shape generic interface
end interface

!> ana metadata
type,public,extends(vol7d_ana) :: dbaana

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbaana_delete
# else
!    procedure :: delete => dbaana_delete
# endif
  procedure :: display => dbaana_display !< print a summary of object contents
  procedure :: dbaset => dbaana_set  !< set dballe parameters
  procedure :: dbaenq => dbaana_enq  !< query dballe parameters
  procedure :: extrude => dbaana_extrude !< put data on DSN
end type dbaana

!> User-defined constructors => dballe_class::dbaana_init
interface dbaana
  procedure   dbaana_init     !< add constructor to shape generic interface
end interface

!> double linked list of ana
type, public, extends(list) :: dbaanaList
  contains
  procedure :: current => currentdbaana !< get dbaana pointed by iterator
  procedure :: display => displaydbaana !< print a summary of object contents
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
  procedure :: display => dbanetwork_display  !< print a summary of object contents
  procedure :: dbaset => dbanetwork_set  !< set dballe parameters
  procedure :: dbaenq => dbanetwork_enq  !< query dballe parameters

end type dbanetwork

!> User-defined constructors => dballe_class::dbanetwork_init
interface dbanetwork
  procedure   dbanetwork_init     !< add constructor to shape generic interface
end interface


!> datetime metadata
type,public,extends(datetime) :: dbadatetime

  contains
# ifdef F2003_FULL_FEATURES
!    final :: dbanetwork_delete
# else
!    procedure :: delete => dbanetwork_delete
# endif
  procedure :: display => dbadatetime_display  !< print a summary of object contents
  procedure :: dbaset => dbadatetime_set  !< set dballe parameters
  procedure :: dbaenq => dbadatetime_enq  !< query dballe parameters
  procedure,nopass :: dbacontextana => dbadatetime_contextana  !< set dballe station data context for date and time (in object, not dballe session)
end type dbadatetime

!> User-defined constructors => dballe_class::dbadatetime_init
interface dbadatetime
  procedure   dbadatetime_init     !< add constructor to shape generic interface
end interface


!> base (abstract)  type for data
type,public,abstract :: dbadata
  character(len=9) :: btable
contains
  procedure(dbadata_set),deferred :: dbaset !< set dballe parameters
  procedure :: dbadata_geti !< return integer value
  procedure :: dbadata_getr !< return real value
  procedure :: dbadata_getd !< return double precision value
  procedure :: dbadata_getb !< return byte value
  procedure :: dbadata_getc !< return character value
  generic   :: get => dbadata_geti,dbadata_getr,dbadata_getd,dbadata_getb,dbadata_getc !< return value
  procedure :: dbadata_c_e_i !< test integer missing value
  procedure :: dbadata_c_e_r !< test real missing value
  procedure :: dbadata_c_e_d !< test double precision missing value
  procedure :: dbadata_c_e_b !< test byte missing value
  procedure :: dbadata_c_e_c !< test character missing value
  procedure :: c_e => dbadata_c_e !< test missing value
  procedure(dbadata_display),deferred :: display  !< print a summary of object contents
  procedure :: equal => dbadata_equal !> compare two dbametadata
  generic   :: operator (==) => equal !> == operator
end type dbadata

!> set parameters in dballe API
abstract interface
subroutine dbadata_set(data,session)
import
class(dbadata), intent(in) :: data
type(dbasession), intent(in) :: session
end subroutine dbadata_set

!> print a summary of object contents
subroutine dbadata_display(data)
import
class(dbadata), intent(in) :: data
end subroutine dbadata_display

end interface

!> integer version for dbadata
type,public, extends(dbadata) :: dbadatai
  integer :: value
contains
  procedure :: dbadata_geti => dbadatai_geti !< return integer value
  procedure :: dbaset => dbadatai_set !< set dballe parameters
  procedure :: display => dbadatai_display  !< print a summary of object contents
end type dbadatai

!> User-defined constructors => dballe_class::dbadatai_init
interface dbadatai
  procedure ::  dbadatai_init     !< add constructor to shape generic interface
end interface dbadatai

!> real version for dbadata
type,public, extends(dbadata) :: dbadatar
  real :: value
contains
  procedure :: dbadata_getr => dbadatar_getr !< return real value
  procedure :: dbaset => dbadatar_set !< set dballe parameters
  procedure :: display => dbadatar_display !< print a summary of object contents
end type dbadatar

!> User-defined constructors => dballe_class::dbadatar_init
interface dbadatar
  procedure ::  dbadatar_init     !< add constructor to shape generic interface
end interface dbadatar


!> doubleprecision version for dbadata
type,public, extends(dbadata) :: dbadatad
  doubleprecision :: value
contains
  procedure :: dbadata_getd => dbadatad_getd !< return double precision value
  procedure :: dbaset => dbadatad_set !< set dballe parameters
  procedure :: display => dbadatad_display !< print a summary of object contents
end type dbadatad

!> User-defined constructors => dballe_class::dbadatad_init
interface dbadatad
  procedure ::  dbadatad_init     !< add constructor to shape generic interface
end interface dbadatad


!> byte version for dbadata
type,public, extends(dbadata) :: dbadatab
  integer(kind=int_b) :: value
contains
  procedure :: dbadata_getb => dbadatab_getb !< return byte value
  procedure :: dbaset => dbadatab_set !< set dballe parameters
  procedure :: display => dbadatab_display !< print a summary of object contents
end type dbadatab

!> User-defined constructors => dballe_class::dbadatab_init
interface dbadatab
  procedure ::  dbadatab_init     !< add constructor to shape generic interface
end interface dbadatab


!> character version for dbadata
type,public, extends(dbadata) :: dbadatac
!  character(:) :: value
!  character(255) :: value
character(vol7d_cdatalen) :: value

contains
  procedure :: dbadata_getc => dbadatac_getc !< return character value
  procedure :: dbaset => dbadatac_set !< set dballe parameters
  procedure :: display => dbadatac_display !< print a summary of object contents
end type dbadatac

!> User-defined constructors => dballe_class::dbadatac_init
interface dbadatac
  procedure ::  dbadatac_init     !< add constructor to shape generic interface
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
  procedure :: dbaset => dbametadata_set  !< set dballe parameters
  procedure :: dbaenq => dbametadata_enq  !< query dballe parameters
  procedure :: dbacontextana => dbametadata_contextana  !< set dballe station data context for all metadata (in object, not dballe session)
  procedure :: display => dbametadata_display !< print a summary of object contents
  procedure :: equal => dbametadata_equal !> compare two dbametadata
  generic   :: operator (==) => equal !> == operator
end type dbametadata

!> User-defined constructors => dballe_class::dbametadata_init
interface dbametadata
  procedure   dbametadata_init     !< add constructor to shape generic interface
end interface

!> container for dbadata (used for promiscuous vector of data)
type, public :: dbadc
  class(dbadata),allocatable :: dat
  contains
  procedure :: display => dbadc_display !< print a summary of object contents
  procedure :: dbaset => dbadc_set !< set dballe parameters
  procedure :: extrude => dbadc_extrude
end type dbadc


!> vector of container of dbadata
type, public :: dbadcv
  type(dbadc),allocatable :: dcv(:)
  contains
  procedure :: display => dbadcv_display !< print a summary of object contents
  procedure :: dbaset => dbadcv_set !< set dballe parameters
  procedure :: extrude => dbadcv_extrude
  procedure :: equal => dbadcv_equal_dbadata !> compare dbadcv and  dbadata
  generic   :: operator (==) => equal !> == operator
end type dbadcv

!> extend one data container with a vector of data container (one data plus attributes)
type, public ,extends(dbadc):: dbadataattr
  type(dbadcv) :: attrv
  contains
  procedure :: display => dbadataattr_display !< print a summary of object contents
  procedure :: extrude => dbadataattr_extrude !< put data on DSN
end type dbadataattr

!> vector of dbadataattr (more data plus attributes)
type, public :: dbadataattrv
  class(dbadataattr),allocatable :: dataattr(:)
  contains
  procedure :: display => dbadataattrv_display !< print a summary of object contents
  procedure :: extrude => dbadataattrv_extrude!< put data on DSN
end type dbadataattrv

!> one metadata with more data plus attributes
type, public :: dbametaanddata
  type(dbametadata) :: metadata
  type(dbadataattrv) ::dataattrv
  contains
  procedure :: display => dbametaanddata_display !< print a summary of object contents
  procedure :: extrude => dbametaanddata_extrude!< put data on DSN
end type dbametaanddata

!> one metadata plus vector of container of dbadata
type, public :: dbametaanddatav
  type(dbametadata) :: metadata
  type(dbadcv) ::datav
  contains
  procedure :: display => dbametaanddatav_display !< print a summary of object contents
  procedure :: extrude => dbametaanddatav_extrude!< put data on DSN
end type dbametaanddatav

!> double linked list of dbametaanddata
type, public, extends(list) :: dbametaanddataList
  contains
  procedure :: current => currentdbametaanddata  !< get dbametaanddata pointed by iterator
  procedure :: display => displaydbametaanddata  !< print a summary of object contents
  procedure :: extrude => dbametaanddatal_extrude !< put data on DSN
end type dbametaanddataList

!> metadata and integer data
type, public,extends(dbadatai) :: dbametaanddatai
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatai_display !< print a summary of object contents
  procedure :: extrude => dbametaanddatai_extrude!< put data on DSN
end type dbametaanddatai

!> metadata and integer data double linked list
type, public, extends(list) :: dbametaanddataiList
  contains
  procedure :: current => currentdbametaanddatai  !< get dbametaanddatai pointed by iterator
  procedure :: display => displaydbametaanddatai  !< print a summary of object contents
  procedure :: toarray => toarray_dbametaanddatai !< return dbametaanddatai allocated array
end type dbametaanddataiList

!> metadata and byte data
type, public,extends(dbadatab) :: dbametaanddatab
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatab_display !< print a summary of object contents
  procedure :: extrude => dbametaanddatab_extrude!< put data on DSN
end type dbametaanddatab

!> metadata and byte data double linked list
type, public, extends(list) :: dbametaanddatabList
  contains
  procedure :: current => currentdbametaanddatab  !< get dbametaanddatab pointed by iterator
  procedure :: display => displaydbametaanddatab  !< print a summary of object contents
  procedure :: toarray => toarray_dbametaanddatab !< return dbametaanddatab allocated array
end type dbametaanddatabList

!> metadata and doubleprecision data
type, public,extends(dbadatad) :: dbametaanddatad
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatad_display !< print a summary of object contents
  procedure :: extrude => dbametaanddatad_extrude!< put data on DSN
end type dbametaanddatad

!> metadata and diubleprecision data double linked list
type, public, extends(list) :: dbametaanddatadList
  contains
  procedure :: current => currentdbametaanddatad  !< get dbametaanddatad pointed by iterator
  procedure :: display => displaydbametaanddatad  !< print a summary of object contents
  procedure :: toarray => toarray_dbametaanddatad !< return dbametaanddatad allocated array
end type dbametaanddatadList

!> metadata and real data
type, public,extends(dbadatar) :: dbametaanddatar
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatar_display !< print a summary of object contents
  procedure :: extrude => dbametaanddatar_extrude!< put data on DSN
end type dbametaanddatar

!> metadata and real data double linked list
type, public, extends(list) :: dbametaanddatarList
  contains
  procedure :: current => currentdbametaanddatar  !< get dbametaanddatar pointed by iterator
  procedure :: display => displaydbametaanddatar  !< print a summary of object contents
  procedure :: toarray => toarray_dbametaanddatar !< return dbametaanddatar allocated array
end type dbametaanddatarList

!> metadata and character data
type, public,extends(dbadatac) :: dbametaanddatac
  type(dbametadata) :: metadata
  contains
  procedure :: display => dbametaanddatac_display !< print a summary of object contents
  procedure :: extrude => dbametaanddatac_extrude!< put data on DSN
end type dbametaanddatac

!> metadata and character data double linked list
type, public, extends(list) :: dbametaanddatacList
  contains
  procedure :: current => currentdbametaanddatac  !< get dbametaanddatac pointed by iterator
  procedure :: display => displaydbametaanddatac  !< print a summary of object contents
  procedure :: toarray => toarray_dbametaanddatac !< return dbametaanddatac allocated array
end type dbametaanddatacList

!> filter to apply before ingest data
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
  character(len=255) :: ana_filter, data_filter, attr_filter, varlist, starvarlist, anavarlist, anastarvarlist
  character(len=40)  :: query 
  integer            :: priority,priomin,priomax
  logical            :: contextana
  logical            :: anaonly
  logical            :: dataonly
  type(dbadcv)       :: vars,starvars
  type(dbadcv)       :: anavars,anastarvars
  contains
  procedure          :: display => dbafilter_display  !< print a summary of object contents
  procedure          :: dbaset => dbafilter_set !< set dballe parameters
  procedure :: equalmetadata => dbafilter_equal_dbametadata !> compare filter with dbametadata
!!$  procedure :: equaldata => dbafilter_equal_dbadata !> compare filter with dbadata
!!$  generic   :: operator (==) => equalmetadata,equaldata !> == operator
  generic   :: operator (==) => equalmetadata !> == operator
end type dbafilter

!> User-defined constructors => dballe_class::dbafilter_init
interface dbafilter
  procedure   dbafilter_init     !< add constructor to shape generic interface
end interface

contains

!> print a summary of object contents
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

!> Get dbametaanddata pointed by iterator
type(dbametaanddata) function currentdbametaanddata(this)
class(dbametaanddataList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddata)
  currentdbametaanddata = v
end select
end function currentdbametaanddata


!> equal operator for dbadata
elemental logical function dbadata_equal(this,that)

class(dbadata), intent(in) :: this !< first element
class(dbadata), intent(in) :: that !< second element

if ( this%btable         == that%btable ) then
  dbadata_equal = .true.
else
  dbadata_equal = .false.
end if

end function dbadata_equal


!> return integer value
subroutine dbadata_geti(data,value)
class(dbadata), intent(in) :: data !< data object to be decoded
integer, intent(out) :: value !< returned value
value=imiss

select type(data)
type is (dbadatai)
  value = data%value
end select

end subroutine dbadata_geti


!> test missing value
logical function dbadata_c_e_i(data)
class(dbadata), intent(in) :: data !< data object to be tested

dbadata_c_e_i=.false.

select type(data)
type is (dbadatai)
  dbadata_c_e_i = c_e(data%value)
end select

end function dbadata_c_e_i

!> return real value
subroutine dbadata_getr(data,value)
class(dbadata), intent(in) :: data !< data object to be decoded
real, intent(out) :: value !< returned value
value=rmiss

select type(data)
type is (dbadatar)
  value = data%value
end select

end subroutine dbadata_getr

!> test missing value
logical function dbadata_c_e_r(data)
class(dbadata), intent(in) :: data !< data object to be tested

dbadata_c_e_r=.false.

select type(data)
type is (dbadatar)
  dbadata_c_e_r = c_e(data%value)
end select

end function dbadata_c_e_r

!> return double precision value
subroutine dbadata_getd(data,value)
class(dbadata), intent(in) :: data !< data object to be decoded
doubleprecision, intent(out) :: value !< returned value
value=dmiss

select type(data)
type is (dbadatad)
  value = data%value
end select

end subroutine dbadata_getd

!> test missing value
logical function dbadata_c_e_d(data)
class(dbadata), intent(in) :: data !< data object to be tested

dbadata_c_e_d=.false.

select type(data)
type is (dbadatad)
  dbadata_c_e_d = c_e(data%value)
end select

end function dbadata_c_e_d


!> return byte value
subroutine dbadata_getb(data,value)
class(dbadata), intent(in) :: data !< data object to be decoded
INTEGER(kind=int_b), intent(out) :: value !< returned value
value=bmiss

select type(data)
type is (dbadatab)
  value = data%value
end select

end subroutine dbadata_getb

!> test missing value
logical function dbadata_c_e_b(data)
class(dbadata), intent(in) :: data !< data object to be tested

dbadata_c_e_b=.false.

select type(data)
type is (dbadatab)
  dbadata_c_e_b = c_e(data%value)
end select

end function dbadata_c_e_b

!> return character value
subroutine dbadata_getc(data,value)
class(dbadata), intent(in) :: data !< data object to be decoded
character(len=*), intent(out) :: value !< returned value
value=cmiss

select type(data)
type is (dbadatac)
  value = data%value
end select

end subroutine dbadata_getc


!> test missing value
logical function dbadata_c_e_c(data)
class(dbadata), intent(in) :: data !< data object to be tested

dbadata_c_e_c=.false.

select type(data)
type is (dbadatac)
  dbadata_c_e_c = c_e(data%value)
end select

end function dbadata_c_e_c


!> test missing value
logical function dbadata_c_e(data)
class(dbadata), intent(in) :: data !< data object to be tested

dbadata_c_e=data%dbadata_c_e_i() .or. data%dbadata_c_e_r() .or. data%dbadata_c_e_d() &
 .or. data%dbadata_c_e_b() .or. data%dbadata_c_e_c()

end function dbadata_c_e


!> print a summary of object content
subroutine dbalevel_display(level)
class(dbalevel), intent(in) :: level
call display (level%vol7d_level)
end subroutine dbalevel_display

!> Constructor
!! Without parameter it is initialized to missing
type(dbalevel) function dbalevel_init(level1, l1, level2, l2)

INTEGER,INTENT(IN),OPTIONAL :: level1 !< type for level 1
INTEGER,INTENT(IN),OPTIONAL :: l1 !< value for level 1
INTEGER,INTENT(IN),OPTIONAL :: level2 !< type for level 2
INTEGER,INTENT(IN),OPTIONAL :: l2 !< value for level 2

call init (dbalevel_init%vol7d_level,level1, l1, level2, l2)
end function dbalevel_init

!> set parameters in dballe API
subroutine dbalevel_set(level,session)
class(dbalevel), intent(in) :: level
type(dbasession), intent(in) :: session !< dballe session
integer :: ier

!if (c_e(session%sehandle)) then
ier = idba_setlevel(session%sehandle,&
 level%level1, level%l1, level%level2, level%l2)

!todo this is a work around
if (.not. c_e(level%vol7d_level)) then
  call session%setcontextana
end if

end subroutine dbalevel_set

!> query parameters from dballe API
subroutine dbalevel_enq(level,session)
class(dbalevel), intent(out) :: level
type(dbasession), intent(in) :: session !< dballe session
integer :: ier

ier = idba_enqlevel(session%sehandle,&
 level%level1, level%l1, level%level2, level%l2)

end subroutine dbalevel_enq

!> set dballe station data context for level (in object, not dballe session)
type(dbalevel) function dbalevel_contextana()

dbalevel_contextana=dbalevel()

end function dbalevel_contextana


!> print a summary of object content
subroutine dbaana_display(ana)
class(dbaana), intent(in) :: ana
call display (ana%vol7d_ana)
end subroutine dbaana_display


!> Constructor
!! Without parameter it is initialized to missing
type(dbacoord) function dbacoord_init(lon, lat, ilon, ilat)
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lon !< longitudine
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lat !< latitudine
INTEGER(kind=int_l),INTENT(in),OPTIONAL :: ilon !< integer longitude (nint(lon*1.d5)
INTEGER(kind=int_l),INTENT(in),OPTIONAL :: ilat !< integer latitude (nint(lat*1.d5)

CALL init(dbacoord_init%geo_coord, lon=lon, lat=lat , ilon=ilon, ilat=ilat)

end function dbacoord_init

!> print a summary of object content
subroutine dbacoord_display(coord)
class(dbacoord), intent(in) :: coord
call display (coord%geo_coord)
end subroutine dbacoord_display

!> Constructor
!! Without parameter it is initialized to missing
type(dbaana) function dbaana_init(coord,ident,lon, lat, ilon, ilat)
CHARACTER(len=*),INTENT(in),OPTIONAL :: ident !< identificativo del volo
TYPE(dbacoord),INTENT(IN),optional :: coord !< coordinate
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

!> set parameters in dballe API
subroutine dbaana_set(ana,session)
class(dbaana), intent(in) :: ana
type(dbasession), intent(in) :: session !< dballe session
integer :: ier

!if (c_e(session%sehandle)) then
ier = idba_set(session%sehandle,"lat",getilat(ana%vol7d_ana%coord))
ier = idba_set(session%sehandle,"lon",getilon(ana%vol7d_ana%coord))
if (c_e(ana%vol7d_ana%ident)) then
  ier = idba_set(session%sehandle,"ident",ana%vol7d_ana%ident)
  ier = idba_set(session%sehandle,"mobile",1)
else
  ier = idba_set(session%sehandle,"ident",cmiss)
  ier = idba_set(session%sehandle,"mobile",imiss)  
end if

end subroutine dbaana_set

!> query parameters from dballe API
subroutine dbaana_enq(ana,session)
class(dbaana), intent(out) :: ana
type(dbasession), intent(in) :: session !< dballe session
integer :: ier,ilat,ilon

!if (c_e(session%sehandle)) then
ier = idba_enq(session%sehandle,"lat",ilat)
ier = idba_enq(session%sehandle,"lon",ilon)

call init(ana%vol7d_ana%coord,ilon=ilon,ilat=ilat)
ier = idba_enq(session%sehandle,"ident",ana%vol7d_ana%ident)

end subroutine dbaana_enq


!> put data on DSN
subroutine dbaana_extrude(ana,session)
class(dbaana), intent(in) :: ana
type(dbasession), intent(in) :: session !< dballe session

call session%unsetall()
!write ana
call session%set(ana=ana)
call session%prendilo()

!to close message on file
call session%close_message()

end subroutine dbaana_extrude


!> print a summary of object content
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

!> get dbaana pointed by iterator
type(dbaana) function currentdbaana(this)
class(dbaanaList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbaana)
  currentdbaana = v
end select
end function currentdbaana


!> set parameters in dballe API
subroutine dbadc_set(dc,session)
class(dbadc), intent(in) :: dc
type(dbasession), intent(in) :: session !< dballe session

call dc%dat%dbaset(session)

end subroutine dbadc_set

!> print a summary of object content
subroutine dbadc_display(dc)
class(dbadc), intent(in) :: dc

call dc%dat%display()

end subroutine dbadc_display

!> set parameters in dballe API
subroutine dbadcv_set(dcv,session)
class(dbadcv), intent(in) :: dcv
type(dbasession), intent(in) :: session !< dballe session
integer :: i

do i=1, size(dcv%dcv)
  call dcv%dcv(i)%dbaset(session)
enddo

end subroutine dbadcv_set



!> put data on DSN
subroutine dbadcv_extrude(dcv,session,noattr,filter,template)
class(dbadcv), intent(in) :: dcv
type(dbasession), intent(in) :: session !< dballe session
logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
character(len=*),intent(in),optional :: template
integer :: i

do i=1, size(dcv%dcv)
  call dcv%dcv(i)%extrude(session,noattr,filter,template=template)
enddo

end subroutine dbadcv_extrude

!> put data on DSN
subroutine dbadc_extrude(data,session,noattr,filter,attronly,template)
class(dbadc), intent(in) :: data
type(dbasession), intent(in) :: session !< dballe session
logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
logical, intent(in),optional :: attronly          !< set to .true. to export attr only (no data) 
character(len=*),intent(in),optional :: template

call data%extrude(session,noattr,filter,attronly,template)

end subroutine dbadc_extrude


!> print a summary of object content
subroutine dbadcv_display(dcv)
class(dbadcv), intent(in) :: dcv
integer :: i

if (allocated(dcv%dcv)) then
  do i=1, size(dcv%dcv)
    call dcv%dcv(i)%display()
  end do
end if
end subroutine dbadcv_display

!!$subroutine dbadat_extrude(dat,session)
!!$class(dbadat), intent(in) :: dat
!!$type(dbasession), intent(in) :: session
!!$
!!$!write data in dsn
!!$call dat%dbaset(session)
!!$call session%prendilo()
!!$
!!$end subroutine dbadat_extrude
!!$
!!$subroutine dbadatav_extrude(datav,session)
!!$class(dbadatav), intent(in) :: datav
!!$type(dbasession), intent(in) :: session
!!$integer :: i
!!$!write data in dsn
!!$do i =1,size(datav%dat)
!!$  call datav%dat(i)%dbaset(session)
!!$end do
!!$call session%prendilo()
!!$
!!$end subroutine dbadatav_extrude


!> dballe unsetb
subroutine dbasession_unsetb(session)
class(dbasession), intent(in) :: session
integer :: ier

!if (session%file)then
ier=idba_unsetb(session%sehandle)
!end if
end subroutine dbasession_unsetb

!> dballe close_message
subroutine dbasession_close_message(session,template)
class(dbasession), intent(in) :: session
character(len=*),intent(in),optional :: template
integer :: ier
character(len=40) :: ltemplate


ltemplate=session%template
if (present(template)) ltemplate=template

!!$print*,"--------------- dbasession ---------------------------------"
!!$print *,'file',session%file
!!$print *,'filename',trim(session%filename)
!!$print *,'mode',session%mode
!!$print *,'format',session%format
!!$print *,'simplified',session%simplified
!!$print *,'memdb',session%memdb
!!$print *,'loadfile',session%loadfile
!!$print *,'template',ltemplate
!!$print*,"------------------------------------------------"

if (session%file)then

  if (session%memdb) then

    return
    !call session%messages_write_next(template=ltemplate)

  else

    if (c_e(ltemplate)) then
      ier=idba_set (session%sehandle,"query","message "//trim(ltemplate))
    else
      ier=idba_set (session%sehandle,"query","message")
    end if
    
    call session%unsetb()
    call session%prendilo()
  
  end if
end if
end subroutine dbasession_close_message


!> dballe messages_open_input
subroutine dbasession_messages_open_input(session,filename,mode,format,simplified)
class(dbasession), intent(in) :: session
character (len=*), intent(in) :: filename !< file name to open
character (len=*), intent(in),optional :: mode !< "r"/"w"/"a" the open mode ("r" for read, "w" for write or create, "a" for append) 
character (len=*), intent(in),optional :: format !< the file format. It can be "BUFR", "CREX" or "JSON" (default="BUFR")
logical, intent(in),optional           :: simplified !< rappresentation when interpret message (simplified/precise)

integer :: ier
character (len=40) :: lmode, lformat
logical :: lsimplified

lmode="r"
if (present(mode)) lmode=mode

lformat="BUFR"
if (present(format)) lformat=format

lsimplified=.true.
if (present(simplified)) lsimplified=simplified

ier = idba_messages_open_input(session%sehandle, filename, lmode, lformat, lsimplified)

end subroutine dbasession_messages_open_input


!> dballe messages_open_output
subroutine dbasession_messages_open_output(session,filename,mode,format)
class(dbasession), intent(in) :: session
character (len=*), intent(in) :: filename !< file name to open
character (len=*), intent(in),optional :: mode !< "r"/"w"/"a" the open mode ("r" for read, "w" for write or create, "a" for append) 
character (len=*), intent(in),optional :: format !< the file format. It can be "BUFR", "CREX" o "JSON" (default="BUFR")

integer :: ier
character (len=40) :: lmode, lformat

lmode="w"
if (present(mode)) lmode=mode

lformat="BUFR"
if (present(format)) lformat=format

ier = idba_messages_open_output(session%sehandle, filename, lmode, lformat)

end subroutine dbasession_messages_open_output


!> dballe messages_read_next
logical function dbasession_messages_read_next(session)
class(dbasession), intent(in) :: session

integer :: ier

ier = idba_messages_read_next(session%sehandle, dbasession_messages_read_next)

end function dbasession_messages_read_next

!> dballe messages_write_next
subroutine dbasession_messages_write_next(session,template)
class(dbasession), intent(in) :: session
character(len=*), optional :: template !< template to use writing BUFR/CREX
character(len=40) :: ltemplate

integer :: ier

!TODO how to set autodetect?
!ltemplate="generic"  !! "wmo" =  wmo - WMO style templates (autodetect) ?

ltemplate=session%template
if (present(template)) ltemplate=template

ier = idba_messages_write_next(session%sehandle,ltemplate)

end subroutine dbasession_messages_write_next


!> remove data from DSN
subroutine dbasession_dissolve_metadata(session,metadata)
class(dbasession), intent(in) :: session
type(dbametadata), intent(in) :: metadata(:) !< data are removed where those metadata match

integer :: i

do i =1, size (metadata)

  call metadata(i)%dbaset(session)
  call session%dissolve()

end do

end subroutine dbasession_dissolve_metadata



!> remove attributes from DSN
subroutine dbasession_dissolveattr_metadata(session,metadata)
class(dbasession), intent(in) :: session
type(dbametadata), intent(in),optional :: metadata(:) !< attributes are removed where those metadata match

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


!> put data on DSN
subroutine dbadataattr_extrude(data,session,noattr,filter,attronly,template)
class(dbadataattr), intent(in) :: data
type(dbasession), intent(in) :: session           !< dballe session
logical, intent(in),optional :: noattr            !< set to .true. to put data only (no attribute)
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data
logical, intent(in),optional :: attronly          !< set to .true. to put attr only (no data)
character(len=*),intent(in),optional :: template
integer :: i,ierr,count,code
logical :: critica
character(len=9) :: btable


if (session%file .and. optio_log(attronly))then
  call l4f_category_log(session%category,L4F_ERROR,"attronly writing on file not supported")
  CALL raise_fatal_error()
end if

if (present(filter))then
  if (filter%contextana) then
    if (.not. filter%anavars == data%dbadc%dat) return
  else
    if (.not. filter%vars == data%dbadc%dat) return
  end if
endif

!write data in dsn

!print *,"extrude dati:"
!call data%dbadc%display()

! missing on file do nothing
if (.not. data%dbadc%dat%c_e() .and. session%file) return

call data%dbadc%dbaset(session)

code = idba_error_code()    !! 13 for Value is outside the range

if (optio_log(attronly).or. .not. data%dbadc%dat%c_e() .or. code ==13 ) then

  !! those hare required?
  ierr = idba_set(session%sehandle,"var",data%dbadc%dat%btable)
  !!

  ierr = idba_voglioquesto(session%sehandle, count)

  ! with missing data to extrude and missing data in DB we have nothing to delete
  ! with attronly  and missing data in DB we have nothing to do
  ierr=idba_unsetb(session%sehandle)
  if (count ==0) return

  if (c_e(count)) then
    if (optio_log(attronly))then
      ierr=idba_dammelo(session%sehandle, btable)
                                !ierr=idba_enqi(session%sehandle, "context_id", id)
    else
                                !remove data from db if data is missing
      ierr=idba_dimenticami(session%sehandle)
    endif
  endif
else
  call session%prendilo()
  ierr=idba_unsetb(session%sehandle)
end if

if (optio_log(noattr)) return

!write attributes in dsn
if (allocated(data%attrv%dcv)) then
  if (size(data%attrv%dcv) > 0 )then
    critica = .false.
    do i = 1, size(data%attrv%dcv)
      if (present(filter))then
        if (filter%contextana) then
          if (.not. filter%anastarvars == data%attrv%dcv(i)%dat) cycle
        else
          if (.not. filter%starvars == data%attrv%dcv(i)%dat) cycle
        end if
      endif

      if (data%attrv%dcv(i)%dat%c_e()) then
          !print *,"extrude attributi:"
          !call data%attrv%dcv(i)%dat%display()
        call data%attrv%dcv(i)%dat%dbaset(session)
        critica=.true.
      else if(optio_log(attronly)) then
        !ierr=idba_seti(session%sehandle, "*context_id", id)
        !call session%var_related(data%dbadc%dat%btable) ! If I have made a prendilo I do not need this
        !call data%attrv%dcv(i)%dat%dbaset(session)
        ierr = idba_set(session%sehandle,"*var",data%attrv%dcv(i)%dat%btable)
        !print *,"scusa  attributi:"
        !call data%attrv%dcv(i)%dat%display()
        call session%scusa()
      endif
    end do
    if (critica) then
      !ierr=idba_seti(session%sehandle, "*context_id", id)
      !call session%var_related(data%dbadc%dat%btable) ! If I have made a prendilo I do not need this
      call session%critica()
    end if

  end if
end if


!to close message on file
!call session%close_message()

end subroutine dbadataattr_extrude

!> print a summary of object content
subroutine dbadataattr_display(dc)
class(dbadataattr), intent(in) :: dc

print*,"Data:"
call dc%dbadc%display()
print*,"Attributes:"
call dc%attrv%display()

end subroutine dbadataattr_display


!> put data on DSN
subroutine dbadataattrv_extrude(dataattr,session,noattr,filter,attronly,template)
class(dbadataattrv), intent(in) :: dataattr
type(dbasession), intent(in) :: session !< dballe session
logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
logical, intent(in),optional :: attronly          !< set to .true. to export attr only (no data) 
character(len=*),intent(in),optional :: template

integer :: i

if(.not. allocated(dataattr%dataattr)) return
do i=1, size(dataattr%dataattr)
  call dataattr%dataattr(i)%extrude(session,noattr,filter,attronly,template)
enddo

!to close message on file
!call session%prendilo()
!call session%close_message()

end subroutine dbadataattrv_extrude

!> print a summary of object content
subroutine dbadataattrv_display(dataattr)
class(dbadataattrv), intent(in) :: dataattr
integer :: i

do i=1, size(dataattr%dataattr)
  call dataattr%dataattr(i)%display()
end do

end subroutine dbadataattrv_display

!> return integer value
subroutine dbadatai_geti(data,value)
class(dbadatai), intent(in) :: data
integer, intent(out) :: value !< value returned
value=data%value
end subroutine dbadatai_geti

!> return real value
subroutine dbadatar_getr(data,value)
class(dbadatar), intent(in) :: data
real, intent(out) :: value !< value returned
value=data%value
end subroutine dbadatar_getr

!> return double precision value
subroutine dbadatad_getd(data,value)
class(dbadatad), intent(in) :: data
doubleprecision, intent(out) :: value !< value returned
value=data%value
end subroutine dbadatad_getd

!> return byte value
subroutine dbadatab_getb(data,value)
class(dbadatab), intent(in) :: data
integer(kind=int_b), intent(out) :: value !< value returned
value=data%value
end subroutine dbadatab_getb

!> return character value
subroutine dbadatac_getc(data,value)
class(dbadatac), intent(in) :: data
character(len=*), intent(out) :: value !< value returned
value=data%value
end subroutine dbadatac_getc


!> Constructor
!! Without parameter it is initialized to missing
type(dbadatai) elemental function dbadatai_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< parameter descriptor
INTEGER,INTENT(IN),OPTIONAL :: value  !< value

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

!> Constructor
!! Without parameter it is initialized to missing
type(dbadatar) elemental function dbadatar_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< parameter descriptor
real,INTENT(IN),OPTIONAL :: value  !< value

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

!> Constructor
!! Without parameter it is initialized to missing
type(dbadatad) elemental function dbadatad_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< parameter descriptor
double precision,INTENT(IN),OPTIONAL :: value  !< value

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


!> Constructor
!! Without parameter it is initialized to missing
type(dbadatab) elemental function dbadatab_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< parameter descriptor
INTEGER(kind=int_b) ,INTENT(IN),OPTIONAL :: value  !< value

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

!> Constructor
!! Without parameter it is initialized to missing
type(dbadatac) elemental function dbadatac_init(btable,value)

character(len=*),INTENT(IN),OPTIONAL :: btable !< parameter descriptor
character(len=*),INTENT(IN),OPTIONAL :: value  !< value

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


!> set parameters in dballe API
subroutine dbadatai_set(data,session)
class(dbadatai), intent(in) :: data
type(dbasession), intent(in) :: session !< dballe session
integer :: ier
if (.not. c_e(data%btable)) return
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatai_set

!> print a summary of object content
subroutine dbadatai_display(data)
class(dbadatai), intent(in) :: data
print *,"Btable: ", t2c(data%btable,miss="Missing")," Value: ", t2c(data%value,miss="Missing value")
end subroutine dbadatai_display

!> set parameters in dballe API
subroutine dbadatar_set(data,session)
class(dbadatar), intent(in) :: data
type(dbasession), intent(in) :: session !< dballe session
integer :: ier
if (.not. c_e(data%btable)) return
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatar_set

!> print a summary of object content
subroutine dbadatar_display(data)
class(dbadatar), intent(in) :: data
print *,"Btable: ", t2c(data%btable,miss="Missing")," Value: ", t2c(data%value,miss="Missing value")
end subroutine dbadatar_display


!> set parameters in dballe API
subroutine dbadatad_set(data,session)
class(dbadatad), intent(in) :: data
type(dbasession), intent(in) :: session !< dballe session
integer :: ier
if (.not. c_e(data%btable)) return
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatad_set

!> print a summary of object content
subroutine dbadatad_display(data)
class(dbadatad), intent(in) :: data
print *,"Btable: ", t2c(data%btable,miss="Missing")," Value: ", t2c(data%value,miss="Missing value")
end subroutine dbadatad_display

!> set parameters in dballe API
subroutine dbadatab_set(data,session)
class(dbadatab), intent(in) :: data
type(dbasession), intent(in) :: session !< dballe session
integer :: ier
if (.not. c_e(data%btable)) return
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatab_set

!> print a summary of object content
subroutine dbadatab_display(data)
class(dbadatab), intent(in) :: data
print *,"Btable: ", t2c(data%btable,miss="Missing")," Value: ", t2c(data%value,miss="Missing value")
end subroutine dbadatab_display

!> set parameters in dballe API
subroutine dbadatac_set(data,session)
class(dbadatac), intent(in) :: data
type(dbasession), intent(in) :: session !< dballe session
integer :: ier
if (.not. c_e(data%btable)) return
ier = idba_set(session%sehandle,data%btable,data%value)
end subroutine dbadatac_set

!> print a summary of object content
subroutine dbadatac_display(data)
class(dbadatac), intent(in) :: data
print *,"Btable: ", t2c(data%btable,miss="Missing")," Value: ", t2c(data%value,miss="Missing value")
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


!> print a summary of object content
subroutine dbatimerange_display(timerange)
class(dbatimerange), intent(in) :: timerange
call display (timerange%vol7d_timerange)
end subroutine dbatimerange_display

!> set parameters in dballe API
subroutine dbatimerange_set(timerange,session)
class(dbatimerange), intent(in) :: timerange
type(dbasession), intent(in) :: session !< dballe session
integer :: ier

ier = idba_settimerange(session%sehandle,&
 timerange%timerange, timerange%p1, timerange%p2)

!todo this is a work around
if (.not. c_e(timerange%vol7d_timerange)) then
  call session%setcontextana
end if

end subroutine dbatimerange_set

!> query parameters from dballe API
subroutine dbatimerange_enq(timerange,session)
class(dbatimerange), intent(out) :: timerange
type(dbasession), intent(in) :: session !< dballe session
integer :: ier

ier = idba_enqtimerange(session%sehandle,&
 timerange%timerange, timerange%p1, timerange%p2)

end subroutine dbatimerange_enq

!> Constructor
!! Without parameter it is initialized to missing
type(dbatimerange) function dbatimerange_init(timerange, p1, p2)
INTEGER,INTENT(IN),OPTIONAL :: timerange !< tipo di intervallo temporale
INTEGER,INTENT(IN),OPTIONAL :: p1 !< valore per il primo istante temporale
INTEGER,INTENT(IN),OPTIONAL :: p2 !< valore per il secondo istante temporale

call init (dbatimerange_init%vol7d_timerange,timerange, p1, p2)
end function dbatimerange_init

!> set dballe station data context for timerange  (in object, not dballe session)
type(dbatimerange) function dbatimerange_contextana()

dbatimerange_contextana=dbatimerange()

end function dbatimerange_contextana


!> print a summary of object content
subroutine dbanetwork_display(network)
class(dbanetwork), intent(in) :: network
call display (network%vol7d_network)
print *,"Priority=",network%priority
end subroutine dbanetwork_display

!> set parameters in dballe API
subroutine dbanetwork_set(network,session)
class(dbanetwork), intent(in) :: network
type(dbasession), intent(in) :: session
integer :: ier

ier = idba_set(session%sehandle,"rep_memo", network%name)

end subroutine dbanetwork_set

!> query parameters from dballe API
subroutine dbanetwork_enq(network,session)
class(dbanetwork), intent(out) :: network
type(dbasession), intent(in) :: session !< dballe session
integer :: ier

ier = idba_enq(session%sehandle,"rep_memo", network%name)
ier = idba_enq(session%sehandle,"priority", network%priority)

end subroutine dbanetwork_enq

!> Constructor
!! Without parameter it is initialized to missing
type(dbanetwork) function dbanetwork_init(name)
CHARACTER(len=*),INTENT(in),OPTIONAL :: name !< Mnemonic alias for type of report

call init (dbanetwork_init%vol7d_network,name)
dbanetwork_init%priority=imiss
end function dbanetwork_init


!> print a summary of object content
subroutine dbadatetime_display(datetime)
class(dbadatetime), intent(in) :: datetime
call display (datetime%datetime)
end subroutine dbadatetime_display

!> set parameters in dballe API
subroutine dbadatetime_set(datetime,session)
class(dbadatetime), intent(in) :: datetime
type(dbasession), intent(in) :: session !< dballe session
integer :: ier,year,month,day,hour,minute,sec,msec

CALL getval(datetime%datetime, year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)

if (c_e(msec)) then
  sec=nint(float(msec)/1000.)
else
  sec=imiss
end if

ier = idba_setdate(session%sehandle,year,month,day,hour,minute,sec)

!todo this is a work around
if (.not. c_e(datetime%datetime)) then
  call session%setcontextana
end if

end subroutine dbadatetime_set

!> query parameters from dballe API
subroutine dbadatetime_enq(datetime,session)
class(dbadatetime), intent(out) :: datetime
type(dbasession), intent(in) :: session !< dballe session

integer :: ier,year,month,day,hour,minute,sec,msec

ier = idba_enqdate(session%sehandle,year,month,day,hour,minute,sec)

if (c_e(sec)) then
  msec=sec*1000
else
  msec=imiss
end if

!! TODO
!! this is a workaround ! year == 1000 should never exist
if (year==1000) then
  datetime%datetime=datetime_new()
else
  CALL init(datetime%datetime, year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)
end if

end subroutine dbadatetime_enq

!> Constructor
!! Without parameter it is initialized to missing
type(dbadatetime) function dbadatetime_init(dt)
type(datetime),INTENT(in),OPTIONAL :: dt !< date and time

if (present(dt)) then
  dbadatetime_init%datetime=dt
else
  dbadatetime_init%datetime=datetime_new()
end if

end function dbadatetime_init

!> set dballe station data context for date and time  (in object, not dballe session)
type(dbadatetime) function dbadatetime_contextana()

dbadatetime_contextana%datetime=datetime_new()

end function dbadatetime_contextana


!> Constructor
!! Without parameter it is initialized to missing
type(dbametadata) function dbametadata_init(level,timerange,ana,network,datetime)

type(dbalevel), intent(in), optional :: level !< level
type(dbatimerange), intent(in), optional :: timerange !< timerange
type(dbaana), intent(in), optional :: ana !< ana
type(dbanetwork), intent(in), optional :: network !< network
type(dbadatetime), intent(in), optional :: datetime !< date and time

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

!> print a summary of object content
subroutine dbametadata_display(metadata)
class(dbametadata), intent(in) :: metadata
call metadata%level%display()
call metadata%timerange%display()
call metadata%ana%display()
call metadata%network%display()
call metadata%datetime%display()

end subroutine dbametadata_display

!> set parameters in dballe API
subroutine dbametadata_set(metadata,session)
class(dbametadata), intent(in) :: metadata
type(dbasession), intent(in) :: session !< dballe session

!print *,"extrude metadata:"
!call metadata%display()

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

!> query parameters from dballe API
subroutine dbametadata_enq(metadata,session)
class(dbametadata), intent(out) :: metadata
type(dbasession), intent(in) :: session !< dballe session

call metadata%ana%dbaenq(session)
call metadata%network%dbaenq(session)
call metadata%datetime%dbaenq(session)
call metadata%level%dbaenq(session)
call metadata%timerange%dbaenq(session)

end subroutine dbametadata_enq


!> equal operator for dbafilter and dbametadata
logical function dbafilter_equal_dbametadata(this,that)

class(dbafilter), intent(in) :: this !< first element
class(dbametadata), intent(in) :: that !< second element

dbafilter_equal_dbametadata = .false.

!! TODO utilizzare dataonly ? direi di no

if (this%contextana .and. c_e(that%timerange%vol7d_timerange)) return
if (this%contextana .and. c_e(that%datetime%datetime)) return
if (this%contextana .and. c_e(that%level%vol7d_level)) return

if (c_e(this%level%vol7d_level)         .and. .not. this%level%vol7d_level         == that%level%vol7d_level ) return
if (c_e(this%timerange%vol7d_timerange) .and. .not. this%timerange%vol7d_timerange == that%timerange%vol7d_timerange ) return
if (c_e(this%datetime%datetime)         .and. .not. this%datetime%datetime         == that%datetime%datetime ) return
if (c_e(this%network%vol7d_network)     .and. .not. this%network%vol7d_network     == that%network%vol7d_network ) return
if (c_e(this%ana%vol7d_ana)             .and. .not. this%ana%vol7d_ana             == that%ana%vol7d_ana ) return

if ( c_e(this%datetimemin%datetime) .and. c_e(that%datetime%datetime) .and. &
 this%datetimemin%datetime > that%datetime%datetime ) return
if ( c_e(this%datetimemax%datetime) .and. c_e(that%datetime%datetime) .and. &
 this%datetimemax%datetime < that%datetime%datetime ) return

if (c_e(this%coordmin%geo_coord)) then
  if (geo_coord_ll(that%ana%vol7d_ana%coord, this%coordmin%geo_coord)) return 
end if

if (c_e(this%coordmax%geo_coord)) then
  if (geo_coord_ur(that%ana%vol7d_ana%coord, this%coordmax%geo_coord)) return 
end if

dbafilter_equal_dbametadata = .true.

end function dbafilter_equal_dbametadata


!!$!> equal operator for dbafilter and dbadata
!!$! todo qui vuene utilizzata vars ma potrebbe essere attrs: bisogna distinguere
!!$elemental logical function dbafilter_equal_dbadata(this,that)
!!$
!!$class(dbafilter), intent(in) :: this !< first element
!!$class(dbadata), intent(in) :: that !< second element
!!$
!!$integer :: i
!!$
!!$!non compila:
!!$!dbafilter_equal_dbadata = any(this%vars%dcv(:)%dat == that)
!!$
!!$if (allocated(this%vars%dcv)) then
!!$  do i=1, size(this%vars%dcv(:))
!!$    dbafilter_equal_dbadata = this%vars%dcv(i)%dat == that
!!$    if (dbafilter_equal_dbadata) continue
!!$  end do
!!$else
!!$  dbafilter_equal_dbadata=.false.
!!$end if
!!$
!!$end function dbafilter_equal_dbadata


!> equal operator for dbadcv and dbadata
!! if dbadcv%dcv is not allocated result is .true.
elemental logical function dbadcv_equal_dbadata(this,that)

class(dbadcv), intent(in) :: this !< first element
class(dbadata), intent(in) :: that !< second element

integer :: i

!non compila:
!dbafilter_equal_dbadata = any(this%vars%dcv(:)%dat == that)

if (allocated(this%dcv)) then
  dbadcv_equal_dbadata=.false.
  do i=1, size(this%dcv)
    dbadcv_equal_dbadata = this%dcv(i)%dat == that
    if (dbadcv_equal_dbadata) exit
  end do
else
  dbadcv_equal_dbadata=.true.
end if

end function dbadcv_equal_dbadata


!> equal operator for dbametadata
elemental logical function dbametadata_equal(this,that)

class(dbametadata), intent(in) :: this !< first element
class(dbametadata), intent(in) :: that !< second element

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


!> Constructor
!! This is the filter we can use to limit results fron the ingest operation
!! Without parameter it is initialized to missing
type(dbafilter) function dbafilter_init(filter,ana,var,datetime,level,timerange,network,&
 datetimemin,datetimemax,coordmin,coordmax,limit,&
 ana_filter, data_filter, attr_filter, varlist, starvarlist, anavarlist, anastarvarlist ,&
 priority, priomin, priomax, contextana,&
 vars, starvars, anavars, anastarvars, query,anaonly,dataonly)

type(dbafilter),intent(in),optional    :: filter !< prototype filter to use as default
type(dbaana),intent(in),optional       :: ana !< ana filter
character(len=*),intent(in),optional   :: var !< var filter
type(dbadatetime),intent(in),optional  :: datetime !< date and time filter
type(dbalevel),intent(in),optional     :: level !< level filter
type(dbatimerange),intent(in),optional :: timerange !< timerange filter
type(dbanetwork),intent(in),optional   :: network !< network filter
type(dbacoord),intent(in),optional     :: coordmin !< minimun coordinate filter
type(dbacoord),intent(in),optional     :: coordmax !< maximum coordinate filter
type(dbadatetime),intent(in),optional  :: datetimemin !< minumum date and time filter
type(dbadatetime),intent(in),optional  :: datetimemax !< maximum date and time filter
integer,intent(in),optional            :: limit !< Maximum number of results to return 
character(len=*),intent(in),optional   :: ana_filter !< Restricts the results to only those stations which have a pseudoana value that matches the filter. Examples: 'height>=1000', 'B02001=1', '1000<=height<=2000
character(len=*),intent(in),optional   :: data_filter !< Restricts the results to only the variables of the given type, which have a value that matches the filter. Examples: 't<260', 'B22021>2', '10<=B22021<=20'
character(len=*),intent(in),optional   :: attr_filter !< Restricts the results to only those data which have an attribute that matches the filter. Examples: 'conf>70', 'B33197=0', '25<=conf<=50' 
character(len=*),intent(in),optional   :: varlist !< Comma-separated list of variable B codes wanted on output 
character(len=*),intent(in),optional   :: starvarlist !< Comma-separated list of attribute B codes wanted on output 
character(len=*),intent(in),optional   :: anavarlist !< Comma-separated list of variable B codes wanted on output for ana 
character(len=*),intent(in),optional   :: anastarvarlist !< Comma-separated list of attribute B codes wanted on output for ana
integer,intent(in),optional            :: priority !< priority on network wanted on output
integer,intent(in),optional            :: priomin !< minimum priority on network wanted on output
integer,intent(in),optional            :: priomax !< maximum priority on network wanted on output
logical,intent(in),optional            :: contextana !< set contextana; if true we want to work on station data else on data
class(dbadcv),intent(in),optional      :: vars ! vector of vars wanted on output
class(dbadcv),intent(in),optional      :: starvars ! vector of vars for attribute wanted on output
class(dbadcv),intent(in),optional      :: anavars ! vector of ana vars wanted on output
class(dbadcv),intent(in),optional      :: anastarvars ! vector of vars for attribute of ana wanted on output
character(len=*),intent(in),optional   :: query !< Comma-separated list of query modifiers. Can have one of: 'best' or obsoletes:  'bigana', 'nosort', 'stream'. Examples: 'best'  obsoletes: 'nosort,stream' 
logical,intent(in),optional            :: anaonly !< only station data wanted on input/output
logical,intent(in),optional            :: dataonly !< only data wanted on input/output

integer :: i
logical :: nopreserve

nopreserve=.true.
if (present(filter)) then
  dbafilter_init=filter

!!$  if (allocated(filter%vars%dcv)) then
!!$    if (allocated(dbafilter_init%vars%dcv)) deallocate(dbafilter_init%vars%dcv)
!!$    allocate(dbafilter_init%vars%dcv(size(filter%vars%dcv)))
!!$    do i =1,size(filter%vars%dcv)
!!$      allocate(dbafilter_init%vars%dcv(i)%dat,source=filter%vars%dcv(i)%dat)
!!$    end do
!!$  end if
!!$
!!$  if (allocated(filter%starvars%dcv)) then
!!$    if (allocated(dbafilter_init%starvars%dcv)) deallocate(dbafilter_init%starvars%dcv)
!!$    allocate(dbafilter_init%starvars%dcv(size(filter%starvars%dcv)))
!!$    do i =1,size(filter%starvars%dcv)
!!$      allocate(dbafilter_init%starvars%dcv(i)%dat,source=filter%starvars%dcv(i)%dat)
!!$    end do
!!$  end if
!!$
!!$  if (allocated(filter%anavars%dcv)) then
!!$    if (allocated(dbafilter_init%anavars%dcv)) deallocate(dbafilter_init%anavars%dcv)
!!$    allocate(dbafilter_init%anavars%dcv(size(filter%anavars%dcv)))
!!$    do i =1,size(filter%anavars%dcv)
!!$      call filter%anavars%dcv(i)%dat%display()
!!$      allocate(dbafilter_init%anavars%dcv(i)%dat,source=filter%anavars%dcv(i)%dat)
!!$    end do
!!$  end if
!!$
!!$  if (allocated(filter%anastarvars%dcv)) then
!!$    if (allocated(dbafilter_init%anastarvars%dcv)) deallocate(dbafilter_init%anastarvars%dcv)
!!$    allocate(dbafilter_init%anastarvars%dcv(size(filter%anastarvars%dcv)))
!!$    do i =1,size(filter%anastarvars%dcv)
!!$      allocate(dbafilter_init%anastarvars%dcv(i)%dat,source=filter%anastarvars%dcv(i)%dat)
!!$    end do
!!$  end if

  nopreserve=.false.
end if

if (present(ana)) then
  dbafilter_init%ana=ana
else if (nopreserve) then
  dbafilter_init%ana=dbaana()
end if

if (present(var)) then
  dbafilter_init%var=var
else if (nopreserve) then
  dbafilter_init%var=cmiss
end if

if (present(datetime)) then
  dbafilter_init%datetime=datetime
else if (nopreserve) then
  dbafilter_init%datetime=dbadatetime()
end if

if (present(level)) then
  dbafilter_init%level=level
else if (nopreserve) then
  dbafilter_init%level=dbalevel()
end if

if (present(timerange)) then
  dbafilter_init%timerange=timerange
else if (nopreserve) then
  dbafilter_init%timerange=dbatimerange()
end if

if (present(network)) then
  dbafilter_init%network=network
else if (nopreserve) then
  dbafilter_init%network=dbanetwork()
end if

if (present(datetimemin)) then
  dbafilter_init%datetimemin=datetimemin
else if (nopreserve) then
  dbafilter_init%datetimemin=dbadatetime()
end if

if (present(datetimemax)) then
  dbafilter_init%datetimemax=datetimemax
else if (nopreserve) then
  dbafilter_init%datetimemax=dbadatetime()
end if

if (present(coordmin)) then
  dbafilter_init%coordmin=coordmin
else if (nopreserve) then
  dbafilter_init%coordmin=dbacoord()
end if

if (present(coordmax)) then
  dbafilter_init%coordmax=coordmax
else if (nopreserve) then
  dbafilter_init%coordmax=dbacoord()
end if

if (present(limit)) then
  dbafilter_init%limit=limit
else if (nopreserve) then
  dbafilter_init%limit=imiss
end if

if (present(ana_filter)) then
  dbafilter_init%ana_filter=ana_filter
else if (nopreserve) then
  dbafilter_init%ana_filter=cmiss
end if

if (present(data_filter)) then
  dbafilter_init%data_filter=data_filter
else if (nopreserve) then
  dbafilter_init%data_filter=cmiss
end if

if (present(attr_filter)) then
  dbafilter_init%attr_filter=attr_filter
else if (nopreserve) then
  dbafilter_init%attr_filter=cmiss
end if

if (present(varlist)) then
  dbafilter_init%varlist=varlist
else if (nopreserve) then
  dbafilter_init%varlist=cmiss
end if

if (present(starvarlist)) then
  dbafilter_init%starvarlist=starvarlist
else if (nopreserve) then
  dbafilter_init%starvarlist=cmiss
end if

if (present(anavarlist)) then
  dbafilter_init%anavarlist=anavarlist
else if (nopreserve) then
  dbafilter_init%anavarlist=cmiss
end if

if (present(anastarvarlist)) then
  dbafilter_init%anastarvarlist=anastarvarlist
else if (nopreserve) then
  dbafilter_init%anastarvarlist=cmiss
end if

if (present(vars)) then
  if (allocated(vars%dcv)) then
    allocate(dbafilter_init%vars%dcv(size(vars%dcv)))
    do i =1,size(vars%dcv)
      allocate(dbafilter_init%vars%dcv(i)%dat,source=vars%dcv(i)%dat)
    end do

    dbafilter_init%varlist=""
    do i=1,size(vars%dcv) 
      dbafilter_init%varlist=trim(dbafilter_init%varlist)//vars%dcv(i)%dat%btable
      if (i /=  size(vars%dcv)) dbafilter_init%varlist=trim(dbafilter_init%varlist)//","
    end do
  endif
end if

if (present(starvars)) then
  if (allocated(starvars%dcv)) then
    allocate(dbafilter_init%starvars%dcv(size(starvars%dcv)))
    do i =1,size(starvars%dcv)
      allocate(dbafilter_init%starvars%dcv(i)%dat,source=starvars%dcv(i)%dat)
    end do
    
    dbafilter_init%starvarlist=""
    do i=1,size(starvars%dcv)
      dbafilter_init%starvarlist=trim(dbafilter_init%starvarlist)//starvars%dcv(i)%dat%btable
      if (i /=  size(starvars%dcv)) dbafilter_init%starvarlist=trim(dbafilter_init%starvarlist)//","
    end do
  end if
end if


if (present(anavars)) then
  if (allocated(anavars%dcv)) then
    allocate(dbafilter_init%anavars%dcv(size(anavars%dcv)))
    do i =1,size(anavars%dcv)
      allocate(dbafilter_init%anavars%dcv(i)%dat,source=anavars%dcv(i)%dat)
    end do

    dbafilter_init%anavarlist=""
    do i=1,size(anavars%dcv) 
      dbafilter_init%anavarlist=trim(dbafilter_init%anavarlist)//anavars%dcv(i)%dat%btable
      if (i /=  size(anavars%dcv)) dbafilter_init%anavarlist=trim(dbafilter_init%anavarlist)//","
    end do
  endif
end if

if (present(anastarvars)) then
  if (allocated(anastarvars%dcv)) then
    allocate(dbafilter_init%anastarvars%dcv(size(anastarvars%dcv)))
    do i =1,size(anastarvars%dcv)
      allocate(dbafilter_init%anastarvars%dcv(i)%dat,source=anastarvars%dcv(i)%dat)
    end do
    
    dbafilter_init%anastarvarlist=""
    do i=1,size(anastarvars%dcv)
      dbafilter_init%anastarvarlist=trim(dbafilter_init%anastarvarlist)//anastarvars%dcv(i)%dat%btable
      if (i /=  size(anastarvars%dcv)) dbafilter_init%anastarvarlist=trim(dbafilter_init%anastarvarlist)//","
    end do
  end if
end if

if (present(priority)) then
  dbafilter_init%priority=priority
else if (nopreserve) then
  dbafilter_init%priority=imiss
end if

if (present(priomin)) then
  dbafilter_init%priomin=priomax
else if (nopreserve) then
  dbafilter_init%priomin=imiss
end if

if (present(priomax)) then
  dbafilter_init%priomax=priomax
else if (nopreserve) then
  dbafilter_init%priomax=imiss
end if

if (present(contextana)) then
  dbafilter_init%contextana=contextana
else if (nopreserve) then
  dbafilter_init%contextana=.false.
end if

if (present(anaonly)) then
  dbafilter_init%anaonly=anaonly
else if (nopreserve) then
  dbafilter_init%anaonly=.false.
end if
if (present(dataonly)) then
  dbafilter_init%dataonly=dataonly
else if (nopreserve) then
  dbafilter_init%dataonly=.false.
end if

if (present(query)) then
  dbafilter_init%query=query
else if (nopreserve) then
  dbafilter_init%query=cmiss
end if

end function dbafilter_init

!> print a summary of object content
subroutine dbafilter_display(filter)
class(dbafilter), intent(in) :: filter

Print *,"------------------ filter ---------------"
call filter%ana%display()
call filter%datetime%display()
call filter%level%display()
call filter%timerange%display()
call filter%network%display()
print *, " >>>> minimum:"
call filter%datetimemin%display()
call filter%coordmin%display()
print *, " >>>> maximum:"
call filter%datetimemax%display()
call filter%coordmax%display()
print *, " >>>> vars:"
call filter%vars%display()
print *, " >>>> starvars:"
call filter%starvars%display()
print *, " >>>> anavars:"
call filter%anavars%display()
print *, " >>>> anastarvars:"
call filter%anastarvars%display()
print *,"var=",filter%var
print *,"limit=",filter%limit
print *,"ana_filter=",trim(filter%ana_filter)
print *,"data_filter=",trim(filter%data_filter)
print *,"attr_filter=",trim(filter%attr_filter)
print *,"varlist=",trim(filter%varlist)
print *,"*varlist=",trim(filter%starvarlist)
print *,"anavarlist=",trim(filter%anavarlist)
print *,"ana*varlist=",trim(filter%anastarvarlist)
print *,"priority=",filter%priority
print *,"priomin=",filter%priomin
print *,"priomax=",filter%priomax
print *,"contextana=",filter%contextana
print *,"anaonly=",filter%anaonly
print *,"dataonly=",filter%dataonly
print *,"query=",trim(filter%query)

Print *,"-----------------------------------------"

end subroutine dbafilter_display

!> set parameters in dballe API
subroutine dbafilter_set(filter,session)
class(dbafilter), intent(in) :: filter
type(dbasession), intent(in) :: session

integer :: ier,year,month,day,hour,minute,sec,msec

call session%unsetall()

call filter%ana%dbaset(session)
call filter%network%dbaset(session)
ier = idba_set(session%sehandle,"var",filter%var)

ier = idba_set(session%sehandle,"limit",filter%limit)
ier = idba_set(session%sehandle,"priority",filter%priority)
ier = idba_set(session%sehandle,"priomin",filter%priomin)
ier = idba_set(session%sehandle,"priomax",filter%priomax)

ier = idba_set(session%sehandle,"latmin",getilat(filter%coordmin%geo_coord))
ier = idba_set(session%sehandle,"lonmin",getilon(filter%coordmin%geo_coord))
ier = idba_set(session%sehandle,"latmax",getilat(filter%coordmax%geo_coord))
ier = idba_set(session%sehandle,"lonmax",getilon(filter%coordmax%geo_coord))

ier = idba_set(session%sehandle,"ana_filter",filter%ana_filter)
ier = idba_set(session%sehandle,"data_filter",filter%data_filter)
ier = idba_set(session%sehandle,"attr_filter",filter%attr_filter)

ier = idba_set(session%sehandle,"query",filter%query)

if (filter%contextana) then

 call session%setcontextana()

 ier = idba_set(session%sehandle,"varlist",filter%anavarlist)
 ier = idba_set(session%sehandle,"*varlist",filter%anastarvarlist)

else

  if (c_e(filter%datetime%datetime)) call filter%datetime%dbaset(session)
  if (c_e(filter%level%vol7d_level)) call filter%level%dbaset(session)
  if (c_e(filter%timerange%vol7d_timerange)) call filter%timerange%dbaset(session)

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


  ier = idba_set(session%sehandle,"varlist",filter%varlist)
  ier = idba_set(session%sehandle,"*varlist",filter%starvarlist)
end if

end subroutine dbafilter_set


!> set dballe station data context for all metadata  (in object, not dballe session)
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


!> print a summary of object content
subroutine dbametaanddata_display(metaanddata)
class(dbametaanddata), intent(in) :: metaanddata

call metaanddata%metadata%display()
call metaanddata%dataattrv%display()

end subroutine dbametaanddata_display

!> put data on DSN
subroutine dbametaanddata_extrude(metaanddata,session,noattr,filter,attronly,template)
class(dbametaanddata), intent(in) :: metaanddata
type(dbasession), intent(in) :: session !< dballe session
logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
logical, intent(in),optional :: attronly          !< set to .true. to export attr only (no data) 
character(len=*),intent(in),optional :: template

type(dbafilter)    :: myfilter

!print *,"------------------"
!call metaanddata%display()
!print *,"contextana false"

myfilter=dbafilter(filter=filter,contextana=.false.)
call extrude(metaanddata,session,noattr,myfilter,attronly,template)

!print *,"contextana true"
myfilter=dbafilter(filter=filter,contextana=.true.)
call extrude(metaanddata,session,noattr,myfilter,attronly,template)

contains

subroutine extrude(metaanddata,session,noattr,filter,attronly,template)
class(dbametaanddata), intent(in) :: metaanddata
type(dbasession), intent(in) :: session !< dballe session
logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in) :: filter     !< use this to filter wanted data                
logical, intent(in),optional :: attronly          !< set to .true. to export attr only (no data) 
character(len=*),intent(in),optional :: template

if (.not. filter == metaanddata%metadata) return

call session%unsetall()
!write metadata
call session%set(metadata=metaanddata%metadata)

!write data and attribute
!call session%extrude(metaanddata%dataattrv,noattr,filter)
call metaanddata%dataattrv%extrude(session,noattr,filter,attronly)

!to close message on file
call session%close_message(template)

end subroutine extrude
end subroutine dbametaanddata_extrude


!> print a summary of object content
subroutine dbametaanddatav_display(metaanddatav)
class(dbametaanddatav), intent(in) :: metaanddatav

call metaanddatav%metadata%display()
call metaanddatav%datav%display()

end subroutine dbametaanddatav_display

!> put data on DSN
subroutine dbametaanddatav_extrude(metaanddatav,session,noattr,filter,template)
class(dbametaanddatav), intent(in) :: metaanddatav
type(dbasession), intent(in) :: session !< dballe session
logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
character(len=*),intent(in),optional :: template

type(dbafilter)    :: myfilter

myfilter=dbafilter(filter=filter,contextana=.false.)
call extrude(metaanddatav,session,noattr,myfilter,template)

myfilter=dbafilter(filter=filter,contextana=.true.)
call extrude(metaanddatav,session,noattr,myfilter,template)

contains

subroutine extrude(metaanddatav,session,noattr,filter,template)
class(dbametaanddatav), intent(in) :: metaanddatav
type(dbasession), intent(in) :: session !< dballe session
logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in) :: filter     !< use this to filter wanted data                
character(len=*),intent(in),optional :: template

if (.not. filter == metaanddatav%metadata)return
!write metadata
call session%set(metadata=metaanddatav%metadata)

!write ana data and attribute
!!$call session%set(datav=metaanddatav%datav)
call metaanddatav%datav%extrude(session,noattr,filter,template)

print*,"dbaana_metaanddatav"
!to close message on file
call session%close_message(template)

end subroutine extrude
end subroutine dbametaanddatav_extrude


!> put data on DSN; extrude metaanddata list
subroutine dbametaanddatal_extrude(metaanddatal,session,noattr,filter,attronly,template)
class(dbametaanddatalist), intent(inout) :: metaanddatal !< ingestd data
class(dbasession), intent(in) :: session
logical, intent(in),optional :: noattr          !< set to .true. to put data only (no attribute) 
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
type(dbametaanddata) :: metaanddata
logical, intent(in),optional :: attronly          !< set to .true. to export attr only (no data) 
character(len=*),intent(in),optional :: template

call metaanddatal%rewind()
do while(metaanddatal%element())
  !call session%extrude(metaanddatal%current(),noattr,filter)
  metaanddata=metaanddatal%current()
  call metaanddata%extrude(session,noattr,filter,attronly,template)
  call metaanddatal%next()
end do

end subroutine dbametaanddatal_extrude


!> print a summary of object content
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

!> Get dbametaanddatai pointed by iterator
type(dbametaanddatai) function currentdbametaanddatai(this)
class(dbametaanddataiList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatai)
  currentdbametaanddatai = v
end select
end function currentdbametaanddatai


!> get data from DSN
subroutine dbasession_ingest_metaanddatail(session,metaanddatal,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddatailist), intent(inout) :: metaanddatal !< ingestd data
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                

type(dbametaanddatai) :: element


if (session%memdb .and. .not. session%loadfile)then

  do while (session%messages_read_next())
    call session%set(filter=filter)
    call session%ingest_metaanddatai()
    call session%ingest_metaanddatai(element)
    call metaanddatal%append(element)
    call session%remove_all()
  end do

else

  call session%set(filter=filter)
  call session%ingest_metaanddatai()
  do while (c_e(session%count) .and. session%count >0)
    call session%ingest_metaanddatai(element)
    call metaanddatal%append(element)
    if (session%file) call session%ingest()
  end do

end if

end subroutine dbasession_ingest_metaanddatail

!> return an array of dbametaanddatai
function toarray_dbametaanddatai(this)
type(dbametaanddatai),allocatable :: toarray_dbametaanddatai(:) !< array
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


!> print a summary of object content
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

!> Get dbametaanddatar pointed by iterator
type(dbametaanddatar) function currentdbametaanddatar(this)
class(dbametaanddatarList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatar)
  currentdbametaanddatar = v
end select
end function currentdbametaanddatar


!> get data from DSN
subroutine dbasession_ingest_metaanddatarl(session,metaanddatal,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddatarlist), intent(inout) :: metaanddatal !<ingestd data
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                

type(dbametaanddatar) :: element

if (session%memdb .and. .not. session%loadfile)then

  do while (session%messages_read_next())
    call session%set(filter=filter)
    call session%ingest_metaanddatar()
    call session%ingest_metaanddatar(element)
    call metaanddatal%append(element)
    call session%remove_all()
  end do

else

  call session%set(filter=filter)
  call session%ingest_metaanddatar()
  do while (c_e(session%count) .and. session%count >0)
    call session%ingest_metaanddatar(element)
    call metaanddatal%append(element)
    if (session%file) call session%ingest()
  end do

end if


end subroutine dbasession_ingest_metaanddatarl


!> return an array of dbametaanddatar
function toarray_dbametaanddatar(this)
type(dbametaanddatar),allocatable :: toarray_dbametaanddatar(:) !< array
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


!> print a summary of object content
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

!> Get dbametaanddatad pointed by iterator
type(dbametaanddatad) function currentdbametaanddatad(this)
class(dbametaanddatadList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatad)
  currentdbametaanddatad = v
end select
end function currentdbametaanddatad

!> get data from DSN
subroutine dbasession_ingest_metaanddatadl(session,metaanddatal,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddatadlist), intent(inout) :: metaanddatal !<ingestd data
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                

type(dbametaanddatad) :: element

if (session%memdb .and. .not. session%loadfile)then

  do while (session%messages_read_next())
    call session%set(filter=filter)
    call session%ingest_metaanddatad()
    call session%ingest_metaanddatad(element)
    call metaanddatal%append(element)
    call session%remove_all()
  end do

else

  call session%set(filter=filter)
  call session%ingest_metaanddatad()
  do while (c_e(session%count) .and. session%count >0)
    call session%ingest_metaanddatad(element)
    call metaanddatal%append(element)
    if (session%file) call session%ingest()
  end do

end if

end subroutine dbasession_ingest_metaanddatadl


!> return an array of dbametaanddatad
function toarray_dbametaanddatad(this)
type(dbametaanddatad),allocatable :: toarray_dbametaanddatad(:) !< array
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


!> print a summary of object content
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

!> Get dbametaanddatab pointed by iterator
type(dbametaanddatab) function currentdbametaanddatab(this)
class(dbametaanddatabList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatab)
  currentdbametaanddatab = v
end select
end function currentdbametaanddatab


!> get data from DSN
subroutine dbasession_ingest_metaanddatabl(session,metaanddatal,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddatablist), intent(inout) :: metaanddatal !<ingestd data
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                

type(dbametaanddatab) :: element

if (session%memdb .and. .not. session%loadfile)then

  do while (session%messages_read_next())
    call session%set(filter=filter)
    call session%ingest_metaanddatab()
    call session%ingest_metaanddatab(element)
    call metaanddatal%append(element)
    call session%remove_all()
  end do

else

  call session%set(filter=filter)
  call session%ingest_metaanddatab()
  do while (c_e(session%count) .and. session%count >0)
    call session%ingest_metaanddatab(element)
    call metaanddatal%append(element)
    if (session%file) call session%ingest()
  end do

end if

end subroutine dbasession_ingest_metaanddatabl


!> return an array of dbametaanddatab
function toarray_dbametaanddatab(this)
type(dbametaanddatab),allocatable :: toarray_dbametaanddatab(:) !< array
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


!> print a summary of object content
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

!> Get dbametaanddatac pointed by iterator
type(dbametaanddatac) function currentdbametaanddatac(this)
class(dbametaanddatacList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (dbametaanddatac)
  currentdbametaanddatac = v
end select
end function currentdbametaanddatac


!> get data from DSN
subroutine dbasession_ingest_metaanddatacl(session,metaanddatal,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddataclist), intent(inout) :: metaanddatal!< ingestd data
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                

type(dbametaanddatac) :: element

if (session%memdb .and. .not. session%loadfile)then

  do while (session%messages_read_next())
    call session%set(filter=filter)
    call session%ingest_metaanddatac()
    call session%ingest_metaanddatac(element)
    call metaanddatal%append(element)
    call session%remove_all()
  end do

else

  call session%set(filter=filter)
  call session%ingest_metaanddatac()
  do while (c_e(session%count) .and. session%count >0)
    call session%ingest_metaanddatac(element)
    call metaanddatal%append(element)
    if (session%file) call session%ingest()
  end do

end if

end subroutine dbasession_ingest_metaanddatacl


!> return an array of dbametaanddatac
function toarray_dbametaanddatac(this)
type(dbametaanddatac),allocatable :: toarray_dbametaanddatac(:) !< array
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


!> print a summary of object content
subroutine dbametaanddatai_display(data)
class(dbametaanddatai), intent(in) :: data

call data%metadata%display()
call data%dbadatai%display()

end subroutine dbametaanddatai_display

!> print a summary of object content
subroutine dbametaanddatab_display(data)
class(dbametaanddatab), intent(in) :: data

call data%metadata%display()
call data%dbadatab%display()

end subroutine dbametaanddatab_display

!> print a summary of object content
subroutine dbametaanddatad_display(data)
class(dbametaanddatad), intent(in) :: data

call data%metadata%display()
call data%dbadatad%display()

end subroutine dbametaanddatad_display

!> print a summary of object content
subroutine dbametaanddatar_display(data)
class(dbametaanddatar), intent(in) :: data

call data%metadata%display()
call data%dbadatar%display()

end subroutine dbametaanddatar_display


!> print a summary of object content
subroutine dbametaanddatac_display(data)
class(dbametaanddatac), intent(in) :: data

call data%metadata%display()
call data%dbadatac%display()

end subroutine dbametaanddatac_display


!> put data on DSN
subroutine dbametaanddatai_extrude(metaanddatai,session)
class(dbametaanddatai), intent(in) :: metaanddatai
type(dbasession), intent(in) :: session !< dballe session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatai%metadata)
!write ana data and attribute
call session%set(data=metaanddatai%dbadatai)

if (metaanddatai%dbadatai%c_e()) then
  call session%prendilo()
else
  call session%dimenticami()
endif

end subroutine dbametaanddatai_extrude

!> put data on DSN
subroutine dbametaanddatab_extrude(metaanddatab,session)
class(dbametaanddatab), intent(in) :: metaanddatab
type(dbasession), intent(in) :: session !< dballe session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatab%metadata)
!write ana data and attribute
call session%set(data=metaanddatab%dbadatab)

if (metaanddatab%dbadatab%c_e()) then
  call session%prendilo()
else
  call session%dimenticami()
endif

end subroutine dbametaanddatab_extrude

!> put data on DSN
subroutine dbametaanddatad_extrude(metaanddatad,session)
class(dbametaanddatad), intent(in) :: metaanddatad
type(dbasession), intent(in) :: session !< dballe session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatad%metadata)
!write ana data and attribute
call session%set(data=metaanddatad%dbadatad)

if (metaanddatad%dbadatad%c_e()) then
  call session%prendilo()
else
  call session%dimenticami()
endif

end subroutine dbametaanddatad_extrude

!> put data on DSN
subroutine dbametaanddatar_extrude(metaanddatar,session)
class(dbametaanddatar), intent(in) :: metaanddatar
type(dbasession), intent(in) :: session !< dballe session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatar%metadata)
!write ana data and attribute
call session%set(data=metaanddatar%dbadatar)

if (metaanddatar%dbadatar%c_e()) then
  call session%prendilo()
else
  call session%dimenticami()
endif

end subroutine dbametaanddatar_extrude

!> put data on DSN
subroutine dbametaanddatac_extrude(metaanddatac,session)
class(dbametaanddatac), intent(in) :: metaanddatac
type(dbasession), intent(in) :: session !< dballe session

call session%unsetall()
!write metadata
call session%set(metadata=metaanddatac%metadata)
!write ana data and attribute
call session%set(data=metaanddatac%dbadatac)

if (metaanddatac%dbadatac%c_e()) then
  call session%prendilo()
else
  call session%dimenticami()
endif

end subroutine dbametaanddatac_extrude

!> get data from DSN
subroutine dbasession_ingest_ana(session,ana)
class(dbasession), intent(inout) :: session
type(dbaana), intent(out),optional :: ana !< ana

integer :: ier

if (.not. present(ana)) then
  ier = idba_quantesono(session%sehandle, session%count)
  !print *,"numero ana",session%count
else
  ier = idba_elencamele(session%sehandle)
  call ana%dbaenq(session)
  session%count=session%count-1
end if

end subroutine dbasession_ingest_ana


!> get data from DSN
subroutine dbasession_ingest_anav(session,anav)
class(dbasession), intent(inout) :: session
type(dbaana), intent(out),allocatable :: anav(:) !< array of ana
integer :: i

call session%ingest_ana()

if (c_e(session%count)) then
  allocate(anav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%ingest_ana(anav(i))
  end do
else
  allocate(anav(0))
end if

end subroutine dbasession_ingest_anav


!> get data from DSN
subroutine dbasession_ingest_anal(session,anal)
class(dbasession), intent(inout) :: session
type(dbaanalist), intent(out) :: anal !< double linked list of ana
type(dbaana) :: element

call session%ingest_ana()
do while (c_e(session%count) .and. session%count >0)
  call session%ingest_ana(element)
  call anal%append(element)
  call session%ingest_ana()
end do
end subroutine dbasession_ingest_anal


!> get data from DSN
subroutine dbasession_ingest_metaanddata(session,metaanddata,noattr,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddata), intent(inout),optional :: metaanddata !< ingested data
logical,intent(in),optional :: noattr       !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in),optional :: filter !< use this to filter wanted data                

type(dbametadata) :: metadata
integer :: ier,acount,i,j,k
character(len=9) :: btable
character(255) :: value
logical :: lvars,lstarvars
type(dbadcv)       :: vars,starvars


                                ! if you do not pass metaanddata we presume to have to initialize the query
if (.not. present(metaanddata)) then
  ier = idba_voglioquesto(session%sehandle, session%count)

                                ! preroll one read because after I have to read one more to check metadata
  if (c_e(session%count) .and. session%count > 0) then
    ier = idba_dammelo(session%sehandle, btable)
  end if

else

                                ! you pass metaanddata so we continue with the query

  if (allocated(metaanddata%dataattrv%dataattr)) then
    deallocate (metaanddata%dataattrv%dataattr)
  end if

  lvars=.false.
  lstarvars=.false.
  if (present(filter)) then

    if (filter%contextana) then

      !todo try to use this:     vars=filter%anavars
      if (allocated(filter%anavars%dcv)) then
        lvars=.true.
        allocate(vars%dcv(size(filter%anavars%dcv)))
        do i =1,size(filter%anavars%dcv)
          allocate(vars%dcv(i)%dat,source=filter%anavars%dcv(i)%dat)
        end do
      end if
      
      if (allocated(filter%anastarvars%dcv)) then
        lstarvars=.true.
        allocate(starvars%dcv(size(filter%anastarvars%dcv)))
        do i =1,size(filter%anastarvars%dcv)
          allocate(starvars%dcv(i)%dat,source=filter%anastarvars%dcv(i)%dat)
        end do
      end if

    else

      if (allocated(filter%vars%dcv)) then
        lvars=.true.
        allocate(vars%dcv(size(filter%vars%dcv)))
        do i =1,size(filter%vars%dcv)
          allocate(vars%dcv(i)%dat,source=filter%vars%dcv(i)%dat)
        end do
      end if
      
      if (allocated(filter%starvars%dcv)) then
        lstarvars=.true.
        allocate(starvars%dcv(size(filter%starvars%dcv)))
        do i =1,size(filter%starvars%dcv)
          allocate(starvars%dcv(i)%dat,source=filter%starvars%dcv(i)%dat)
        end do
      end if
      
    end if

  end if

  if (lvars) then

    ! create an empty vector for data 
    allocate (metaanddata%dataattrv%dataattr(size(vars%dcv)))
    do i = 1, size(vars%dcv)
      allocate (metaanddata%dataattrv%dataattr(i)%dat,source=vars%dcv(i)%dat)
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
          
          if (optio_log(noattr))then
                                ! initialize to (0) the attribute vector
            allocate (metaanddata%dataattrv%dataattr(i)%attrv%dcv(0))

          else

            if (lstarvars) then

              allocate (metaanddata%dataattrv%dataattr(i)%attrv%dcv(size(starvars%dcv)))
              do j = 1, size(starvars%dcv)
                allocate (metaanddata%dataattrv%dataattr(i)%attrv%dcv(j)%dat,source=starvars%dcv(j)%dat)
              end do

              if (c_e(session%count) .and. session%count > 0) then
              
                ier = idba_voglioancora(session%sehandle, acount)
                do k =1,acount
                  ier = idba_ancora(session%sehandle, btable)
                  ier = idba_enq(session%sehandle, btable,value)

                  do j=1,size(metaanddata%dataattrv%dataattr(i)%attrv%dcv)

                    if (metaanddata%dataattrv%dataattr(i)%attrv%dcv(j)%dat%btable == btable) then
          
                      select type ( dat =>  metaanddata%dataattrv%dataattr(i)%attrv%dcv(j)%dat )
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
              
                allocate (metaanddata%dataattrv%dataattr(i)%attrv%dcv(acount))
                do j =1,acount
                  ier = idba_ancora(session%sehandle, btable)
                  ier = idba_enq(session%sehandle, btable,value)
                  allocate (metaanddata%dataattrv%dataattr(i)%attrv%dcv(j)%dat,source=dbadatac(btable,value))
                end do
              else
                allocate (metaanddata%dataattrv%dataattr(i)%attrv%dcv(0))
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


    if (optio_log(noattr))then

      allocate (metaanddata%dataattrv%dataattr(1)%attrv%dcv(0))

    else

      if (lstarvars) then
        
        allocate (metaanddata%dataattrv%dataattr(1)%attrv%dcv(size(starvars%dcv)))
        do j = 1, size(starvars%dcv)
          allocate (metaanddata%dataattrv%dataattr(1)%attrv%dcv(j)%dat,source=starvars%dcv(j)%dat)
        end do
        
        if (c_e(session%count) .and. session%count > 0) then
          
          ier = idba_voglioancora(session%sehandle, acount)
          do k =1,acount
            ier = idba_ancora(session%sehandle, btable)
            ier = idba_enq(session%sehandle, btable,value)
            
            do j=1,size(metaanddata%dataattrv%dataattr(1)%attrv%dcv)
              
              if (metaanddata%dataattrv%dataattr(1)%attrv%dcv(j)%dat%btable == btable) then
                
                select type ( dat =>  metaanddata%dataattrv%dataattr(1)%attrv%dcv(j)%dat )
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
          
          allocate (metaanddata%dataattrv%dataattr(1)%attrv%dcv(acount))
          do j =1,acount
            ier = idba_ancora(session%sehandle, btable)
            ier = idba_enq(session%sehandle, btable,value)
            allocate (metaanddata%dataattrv%dataattr(1)%attrv%dcv(j)%dat,source=dbadatac(btable,value))
          end do
        else
          allocate (metaanddata%dataattrv%dataattr(1)%attrv%dcv(0))
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
!!$  SOLVED by https://github.com/ARPA-SIMC/dballe/issues/73
!!$  !reading from file get some variable not in filter so we can have some attrv%dcv not allocated
  do i=1,size(metaanddata%dataattrv%dataattr)
    if (.not.allocated(metaanddata%dataattrv%dataattr(i)%attrv%dcv))  then
      allocate (metaanddata%dataattrv%dataattr(i)%attrv%dcv(0))
    endif
  end do
  
end if

end subroutine dbasession_ingest_metaanddata


!> get data from DSN
subroutine dbasession_ingest_metaanddatav(session,metaanddatav,noattr,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddata), intent(inout),allocatable :: metaanddatav(:) !< ingestd data
logical, intent(in),optional :: noattr        !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in),optional :: filter   !< use this to filter wanted data

type(dbametaanddata),allocatable :: metaanddatavbuf(:)
integer :: i

!todo aggiungere anche altrove dove passato filter
if (present(filter)) then
  call filter%dbaset(session)
else
  call session%unsetall()
endif

call session%ingest()
!print*," count: ",session%count

if (c_e(session%count)) then
  ! allocate to max dimension
  allocate(metaanddatavbuf(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%ingest(metaanddatavbuf(i),noattr=noattr,filter=filter)
  end do

! compact data to real dimension
  IF (SIZE(metaanddatavbuf) == i) THEN
! space/time optimization in common case of no filter
    CALL move_ALLOC(metaanddatavbuf, metaanddatav)
  ELSE
! allocate (metaanddatav(i))
    metaanddatav=metaanddatavbuf(:i)
    DEALLOCATE(metaanddatavbuf)
  ENDIF

else
  if (allocated(metaanddatav)) deallocate(metaanddatav)
  allocate(metaanddatav(0))
end if


end subroutine dbasession_ingest_metaanddatav


!> Get data fron DSN; ingest metaanddata list
subroutine dbasession_ingest_metaanddatal(session,metaanddatal,noattr,filter)
class(dbasession), intent(inout) :: session
type(dbametaanddatalist), intent(out) :: metaanddatal !< ingestd data
logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                

type(dbametaanddata),allocatable :: metaanddatavbuf(:)
integer :: i

if (session%memdb .and. .not. session%loadfile)then

  do while (session%messages_read_next())
    call session%set(filter=filter)
    call session%ingest()
    call session%ingest(metaanddatavbuf,noattr=noattr,filter=filter)
    do i=1,size(metaanddatavbuf)
      call metaanddatal%append(metaanddatavbuf(i))
    end do
    
    call session%remove_all()
    deallocate (metaanddatavbuf)
  end do

else

  call session%ingest()
  
  do while (c_e(session%count) .and. session%count >0)
    call session%ingest(metaanddatavbuf,noattr=noattr,filter=filter)
    do i=1,size(metaanddatavbuf)
      if (present(filter)) then 
                                ! exclude contextana data from file 
        if (filter%contextana) then
          if (datetime_new() /= metaanddatavbuf(i)%metadata%datetime%datetime) cycle
        end if
      end if
      call metaanddatal%append(metaanddatavbuf(i))
    end do
    if (session%file) call session%ingest()
    deallocate (metaanddatavbuf)
  end do
end if

end subroutine dbasession_ingest_metaanddatal

!> Get data from DSN
subroutine dbasession_ingest_metaanddatai(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatai), intent(inout),optional :: metaanddata !< ingestd data

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
end subroutine dbasession_ingest_metaanddatai


!> Get data from DSN
subroutine dbasession_ingest_metaanddataiv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatai), intent(inout),allocatable :: metaanddatav(:) !< ingestd data

integer :: i

call session%ingest_metaanddatai()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%ingest_metaanddatai(metaanddatav(i))
  end do
else
  allocate(metaanddatav(0))
end if

end subroutine dbasession_ingest_metaanddataiv


!> Get data from DSN
subroutine dbasession_ingest_metaanddatab(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatab), intent(inout),optional :: metaanddata !< ingestd data

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
end subroutine dbasession_ingest_metaanddatab


!> Get data from DSN
subroutine dbasession_ingest_metaanddatabv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatab), intent(inout),allocatable :: metaanddatav(:) !<ingestd data

integer :: i

call session%ingest_metaanddatab()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%ingest_metaanddatab(metaanddatav(i))
  end do
else
  allocate(metaanddatav(0))
end if

end subroutine dbasession_ingest_metaanddatabv


!> get data from DSN
subroutine dbasession_ingest_metaanddatad(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatad), intent(inout),optional :: metaanddata !< ingestd data

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
end subroutine dbasession_ingest_metaanddatad


!> Get data from DSN
subroutine dbasession_ingest_metaanddatadv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatad), intent(inout),allocatable :: metaanddatav(:) !<ingestd data

integer :: i

call session%ingest_metaanddatad()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%ingest_metaanddatad(metaanddatav(i))
  end do
else
  allocate(metaanddatav(0))
end if
end subroutine dbasession_ingest_metaanddatadv


!> get data from DSN
subroutine dbasession_ingest_metaanddatar(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatar), intent(inout),optional :: metaanddata !< ingestd data

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
end subroutine dbasession_ingest_metaanddatar


!> Get data from DSN
subroutine dbasession_ingest_metaanddatarv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatar), intent(inout),allocatable :: metaanddatav(:) !<ingestd data

integer :: i

call session%ingest_metaanddatar()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%ingest_metaanddatar(metaanddatav(i))
  end do
else
  allocate(metaanddatav(0))
end if
end subroutine dbasession_ingest_metaanddatarv



!> get data from DSN
subroutine dbasession_ingest_metaanddatac(session,metaanddata)
class(dbasession), intent(inout) :: session
type(dbametaanddatac), intent(inout),optional :: metaanddata !< ingestd data

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
end subroutine dbasession_ingest_metaanddatac


!> Get data from DSN
subroutine dbasession_ingest_metaanddatacv(session,metaanddatav)
class(dbasession), intent(inout) :: session
type(dbametaanddatac), intent(inout),allocatable :: metaanddatav(:) !<ingestd data

integer :: i

call session%ingest_metaanddatac()
if (c_e(session%count)) then
  allocate(metaanddatav(session%count))
  i=0
  do while (session%count >0)
    i=i+1
    call session%ingest_metaanddatac(metaanddatav(i))
  end do
else
  allocate(metaanddatav(session%count))
end if
end subroutine dbasession_ingest_metaanddatacv

!> Constructor
!! Without parameter it is initialized to missing
type(dbaconnection) function dbaconnection_init(dsn, user, password,categoryappend,idbhandle)
character (len=*), intent(in), optional :: dsn !< DSN (dballe/ODBC definition)
character (len=*), intent(in), optional :: user !< username to use
character (len=*), intent(in), optional :: password !< password to use
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
integer,INTENT(in),OPTIONAL :: idbhandle !< dsn connection; if present it will be used

integer :: ier
character(len=512) :: a_name,quidsn

if (present(categoryappend))then
  call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
  call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
dbaconnection_init%category=l4f_category_get(a_name)

! impostiamo la gestione dell'errore
ier=idba_error_set_callback(0,C_FUNLOC(dballe_error_handler), &
 dbaconnection_init%category,dbaconnection_init%handle_err)
if (.not. c_e(optio_i(idbhandle))) then

  quidsn = "test"
  IF (PRESENT(dsn)) THEN
    IF (c_e(dsn)) quidsn = dsn
  ENDIF

  ier=idba_presentati(dbaconnection_init%dbhandle,quidsn)
else
  dbaconnection_init%dbhandle=optio_i(idbhandle)
end if

end function dbaconnection_init

!> remove dballe connection
subroutine dbaconnection_delete(handle)
#ifdef F2003_FULL_FEATURES
type (dbaconnection), intent(inout) :: handle
#else
class (dbaconnection), intent(inout) :: handle
#endif

integer :: ier

if (c_e(handle%dbhandle)) then 
  ier = idba_arrivederci(handle%dbhandle)
  ier = idba_error_remove_callback(handle%handle_err)
end if

end subroutine dbaconnection_delete

!> Constructor
!! Without parameter it is initialized to missing
recursive type(dbasession) function dbasession_init(connection,anaflag, dataflag, attrflag,&
 filename,mode,format,template,write,wipe,repinfo,simplified,memdb,loadfile,categoryappend)
type(dbaconnection),intent(in),optional :: connection !< dballe connection
character (len=*), intent(in), optional :: anaflag !< controls access to pseudoana records and can have these values "read": pseudoana records cannot be modified; "write": pseudoana records can be added and removed. 
character (len=*), intent(in), optional :: dataflag !< control access to observed data and can have these values "read": data cannot be modified in any way; "add": data can be added to the database, but existing data cannot be modified. Deletions are disabled. This is used to insert new data in the database while preserving the data that was already present in it; "write": data can freely be added, overwritten and deleted.
character (len=*), intent(in), optional :: attrflag !< controls access to data attributes and can have these values "read": attributes cannot be modified in any way; "write": attributes can freely be added, overwritten and deleted. Note that some combinations are illegal, such as pseudoana=read and data=add (when adding a new data, it's sometimes necessary to insert new pseudoana records), or data=rewrite and attr=read (when deleting data, their attributes are deleted as well). 
character (len=*), intent(in), optional :: filename !< optional file name to use 
character (len=*), intent(in), optional :: mode !< "r"/"w"/"a" the open mode ("r" for read, "w" for write or create, "a" for append) 
character (len=*), intent(in), optional :: template !< template to use for write on file
logical,INTENT(in),OPTIONAL :: write !< enable write on DSN/file ( default=.false. )
logical,INTENT(in),OPTIONAL :: wipe !<  clean DSN/file and initialize it ( default=.false. )
character(len=*), INTENT(in),OPTIONAL :: repinfo !< optional file repinfo.csv to use with wipe ( default="" )
character(len=*),intent(in),optional :: format !< the file format. It can be "BUFR", "CREX" or "JSON". (default="BUFR")
logical,intent(in),optional :: simplified !< rappresentation for interpreted message (simplified/precise)
logical,intent(in),optional :: memdb     !< if true set a memdb ready for import data from file (default=true if filename present else false)
logical,intent(in),optional :: loadfile     !< if true import from file to memdb (default=true if filename present else false)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< name to append to namespace category of log4fortran

integer :: ier
character (len=5) :: lanaflag,ldataflag,lattrflag
character (len=1) :: lmode
logical :: lwrite,lwipe
character(len=255) :: lrepinfo
character(len=40) :: lformat
logical :: exist,lsimplified,read_next,lfile,lmemdb,lloadfile
character(len=512) :: a_name
character(len=40) :: ltemplate

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


lwrite=.false.
if (present(write))then
  lwrite=write
endif

lwipe=.false.
lrepinfo=""
if (present(wipe))then
  lwipe=wipe
  if (present(repinfo))then
    lrepinfo=repinfo
  endif
endif

lmemdb=.false.
lloadfile=.false.
lfile=.false.

if (present(template))then
  ltemplate=template
else
  ltemplate=cmiss
endif

lsimplified=.true.
if (present(simplified))then
  lsimplified=simplified
end if

lformat="BUFR"
if (present(format))then
  lformat=format
end if

lmode="r"

if (present(filename)) then

  lfile=.true.

  IF (filename == '') THEN
! if stdio do not check existence, stdin always exist, stdout never exist
    exist = .NOT.lwrite
  ELSE
    INQUIRE(file=filename,EXIST=exist)
  ENDIF
  
  if (lwrite)then
    if (lwipe.or..not.exist) then
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

  if (.not.present(memdb))then
    dbasession_init%memdb=.true. ! default with filename
  end if

  if (.not.present(loadfile))then
    dbasession_init%loadfile=.true. ! default with filename
  end if

end if

if (present(memdb))then
  lmemdb=memdb
end if

if (present(loadfile))then
  lloadfile=loadfile
end if


call optio(anaflag,lanaflag)
if (.not. c_e(lanaflag))then
  if (lwrite) then
    lanaflag = "write"
  else
    lanaflag = "read"
  end if
end if

call optio(dataflag,ldataflag)
if (.not. c_e(ldataflag)) then
  if (lwrite) then
    ldataflag = "write"
  else
    ldataflag = "read"
  end if
end if

call optio(attrflag,lattrflag)
if (.not. c_e(lattrflag))then
  if (lwrite) then
    lattrflag = "write"
  else
    lattrflag = "read"
  end if
end if


!!$print*,"---------------- call session_init --------------------------------"
!!$print *,"session_init,lformat,ltemplate,lmemdb,lfile,lloadfile,lanaflag,ldataflag,lattrflag"
!!$print *,"session_init",lformat,ltemplate,lmemdb,lfile,lloadfile,lanaflag,ldataflag,lattrflag
!!$print*,"------------------------------------------------"

if (lfile) then

  if (present(anaflag).or.present(dataflag).or.present( attrflag)) then
    call l4f_category_log(dbasession_init%category,L4F_ERROR,"option anaflag, dataflag, attrflag defined with filename access")
    CALL raise_error()
  end if

else

  if(.not. present(connection)) then
    call l4f_category_log(dbasession_init%category,L4F_ERROR,"connection not present accessing DBA")
    CALL raise_error()
  end if
  
  if (present(mode).or.present(format).or.present(template).or.present(simplified)) then
    call l4f_category_log(dbasession_init%category,L4F_ERROR,&
     "option mode or format or template or simplified defined without filename")
    CALL raise_error()
  end if

end if


! check filename for recursive call
if (present(filename))then
  if (lmemdb)then
    if (.not. present(connection)) then
                                ! connect to dsn type DBA
      dbasession_init%memconnection=dbaconnection(dsn="mem:")
      !call self with memconnection without filename 
      dbasession_init=dbasession(dbasession_init%memconnection,&
       write=.true.,wipe=lwrite,repinfo=lrepinfo,&
       memdb=lmemdb,loadfile=lloadfile) ! without categoryappend
      
    else
      dbasession_init%memconnection=connection
      !call self with memconnection without filename 
      dbasession_init=dbasession(dbasession_init%memconnection,&
       write=.true.,wipe=lwrite,repinfo=lrepinfo,&
       memdb=lmemdb,loadfile=lloadfile) ! without categoryappend

    end if

    if (lmode == "r") then
      call dbasession_init%messages_open_input(filename=filename,mode=lmode,&
       format=lformat,simplified=lsimplified)
      
      if (lloadfile)then
        read_next = dbasession_init%messages_read_next()
        do while (read_next) 
          read_next = dbasession_init%messages_read_next()
        end do
      end if
    else
      
      call dbasession_init%messages_open_output(filename=filename,&
       mode=lmode,format=lformat)
      
    end if
    
  else
    
    ier =  idba_messaggi(dbasession_init%sehandle,filename, lmode, lformat)
    
  end if
  
else
  
  ier = idba_preparati(connection%dbhandle,dbasession_init%sehandle, lanaflag, ldataflag, lattrflag)
  if (lwipe)ier=idba_scopa (dbasession_init%sehandle,lrepinfo)

end if

dbasession_init%file=lfile
if (dbasession_init%file) dbasession_init%filename=filename
dbasession_init%mode=lmode
dbasession_init%format=lformat
dbasession_init%simplified=lsimplified
dbasession_init%memdb=lmemdb
dbasession_init%loadfile=lloadfile
dbasession_init%template=ltemplate

!!$print*,"--------------- at end ---------------------------------"
!!$print *,'file',dbasession_init%file
!!$print *,'filename',trim(dbasession_init%filename)
!!$print *,'mode',dbasession_init%mode
!!$print *,'format',dbasession_init%format
!!$print *,'simplified',dbasession_init%simplified
!!$print *,'memdb',dbasession_init%memdb
!!$print *,'loadfile',dbasession_init%loadfile
!!$print *,'template',dbasession_init%template
!!$print*,"------------------------------------------------"

end function dbasession_init


!> clean all setting on dballe API
subroutine dbasession_unsetall(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then
  ier = idba_unsetall(session%sehandle)
end if

end subroutine dbasession_unsetall


!> dballe remove_all
subroutine dbasession_remove_all(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then
  ier = idba_remove_all(session%sehandle)
end if

end subroutine dbasession_remove_all


!> dballe prendilo
subroutine dbasession_prendilo(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then
  ier = idba_prendilo(session%sehandle)
end if

end subroutine dbasession_prendilo

!> dballe var_related
subroutine dbasession_var_related(session,btable)
class (dbasession), intent(in) :: session
character(len=*),INTENT(IN) :: btable !< descrittore variabile
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_set(session%sehandle,"*var_related",btable)
end if

end subroutine dbasession_var_related

!> set parameters in dballe API needed for station data
subroutine dbasession_setcontextana(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_setcontextana(session%sehandle)
end if

end subroutine dbasession_setcontextana

!> dballe dimenticami
subroutine dbasession_dimenticami(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_dimenticami(session%sehandle)
end if

end subroutine dbasession_dimenticami

!> dballe critica
subroutine dbasession_critica(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_critica(session%sehandle)
end if

end subroutine dbasession_critica

!> dballe scusa
subroutine dbasession_scusa(session)
class (dbasession), intent(in) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_scusa(session%sehandle)
end if

end subroutine dbasession_scusa

!> set parameters in dballe API
subroutine dbasession_set(session,metadata,datav,data,datetime,ana,network,level,timerange,filter)
class (dbasession), intent(in) :: session
type (dbametadata),optional :: metadata !< metadata
class(dbadcv),optional :: datav !< data array
class(dbadata),optional :: data !< data
type (dbadatetime),optional :: datetime !< date and time
type (dbaana),optional :: ana !< ana
type (dbanetwork),optional :: network !< network
type (dbalevel),optional :: level !< level
type (dbatimerange),optional :: timerange !< timerange
type (dbafilter),optional :: filter !< filter for wanted data

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


!!!   Those are for reverse order call  session%extrude(object) 

!!$!> put data on DSN
!!$subroutine dbasession_extrude_ana(session,ana)
!!$class(dbasession), intent(in)   :: session
!!$class(dbaana)          :: ana !< ana 
!!$call ana%extrude(session)
!!$end subroutine dbasession_extrude_ana
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_dataattr(session,dataattr)
!!$class(dbasession), intent(in)   :: session
!!$class(dbadataattr)     :: dataattr !< dataattr
!!$call dataattr%extrude(session)
!!$end subroutine dbasession_extrude_dataattr
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_dataattrv(session,dataattrv,noattr,filter)
!!$class(dbasession), intent(in)   :: session
!!$class(dbadataattrv)    :: dataattrv !< array datatattr
!!$logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
!!$type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
!!$
!!$call dataattrv%extrude(session,noattr,filter)
!!$end subroutine dbasession_extrude_dataattrv
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_metaanddata(session,metaanddata,noattr,filter)
!!$class(dbasession), intent(in)   :: session
!!$class(dbametaanddata)  :: metaanddata !< metaanddata
!!$logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
!!$type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
!!$
!!$call metaanddata%extrude(session,noattr,filter)
!!$end subroutine dbasession_extrude_metaanddata
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_metaanddatai(session,metaanddatai)
!!$class(dbasession), intent(in)   :: session
!!$class(dbametaanddatai) :: metaanddatai !< metaanddatai
!!$call metaanddatai%extrude(session)
!!$end subroutine dbasession_extrude_metaanddatai
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_metaanddatab(session,metaanddatab)
!!$class(dbasession), intent(in)   :: session
!!$class(dbametaanddatab) :: metaanddatab !< metaanddatab
!!$call metaanddatab%extrude(session)
!!$end subroutine dbasession_extrude_metaanddatab
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_metaanddatad(session,metaanddatad)
!!$class(dbasession), intent(in)   :: session
!!$class(dbametaanddatad) :: metaanddatad !< metaanddatad
!!$call metaanddatad%extrude(session)
!!$end subroutine dbasession_extrude_metaanddatad
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_metaanddatac(session,metaanddatac)
!!$class(dbasession), intent(in)   :: session
!!$class(dbametaanddatac) :: metaanddatac !< metaanddatac
!!$call metaanddatac%extrude(session)
!!$end subroutine dbasession_extrude_metaanddatac
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_metaanddatar(session,metaanddatar)
!!$class(dbasession), intent(in)   :: session
!!$class(dbametaanddatar) :: metaanddatar !< metaanddatar
!!$call metaanddatar%extrude(session)
!!$end subroutine dbasession_extrude_metaanddatar
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude_metaanddatav(session, metaanddatav,noattr,filter)
!!$class(dbasession), intent(in)   :: session
!!$class(dbametaanddatav) :: metaanddatav !< array metaanddata
!!$logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
!!$type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
!!$
!!$call metaanddatav%extrude(session,noattr,filter)
!!$end subroutine dbasession_extrude_metaanddatav
!!$
!!$subroutine dbasession_extrude_metaanddatal(session, metaanddatal,noattr,filter)
!!$class(dbasession), intent(in)   :: session
!!$class (dbametaanddatalist) :: metaanddatal !< metaanddata list
!!$logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
!!$type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
!!$
!!$call metaanddatal%extrude(session,noattr,filter)
!!$end subroutine dbasession_extrude_metaanddatal
!!$
!!$!> put data on DSN
!!$subroutine dbasession_extrude(session,ana,dataattr,dataattrv,metaanddata,&
!!$ metaanddatai,metaanddatab,metaanddatad,metaanddatac,metaanddatar,&
!!$ metaanddatav ,metaanddatal,noattr,filter)
!!$class(dbasession), intent(in)   :: session
!!$class(dbaana),optional          :: ana !< ana 
!!$class(dbadataattr),optional     :: dataattr !< dataattr
!!$class(dbadataattrv),optional    :: dataattrv !< array datatattr
!!$class(dbametaanddata),optional  :: metaanddata !< metaanddata
!!$class(dbametaanddatai),optional :: metaanddatai !< metaanddatai
!!$class(dbametaanddatab),optional :: metaanddatab !< metaanddatab
!!$class(dbametaanddatad),optional :: metaanddatad !< metaanddatad
!!$class(dbametaanddatac),optional :: metaanddatac !< metaanddatac
!!$class(dbametaanddatar),optional :: metaanddatar !< metaanddatar
!!$class(dbametaanddatav),optional :: metaanddatav !< array metaanddata
!!$class(dbametaanddatalist),optional :: metaanddatal !< metaanddata list
!!$logical, intent(in),optional :: noattr          !< set to .true. to get data only (no attribute) 
!!$type(dbafilter),intent(in),optional :: filter     !< use this to filter wanted data                
!!$
!!$if (present(ana)) then
!!$  call ana%extrude(session)
!!$end if
!!$
!!$if (present(dataattr)) then
!!$  call dataattr%extrude(session)
!!$end if
!!$
!!$if (present(dataattrv)) then
!!$  call dataattrv%extrude(session,noattr,filter)
!!$end if
!!$
!!$if (present(metaanddata)) then
!!$  call metaanddata%extrude(session)
!!$end if
!!$
!!$if (present(metaanddatai)) then
!!$  call metaanddatai%extrude(session)
!!$end if
!!$
!!$if (present(metaanddatab)) then
!!$  call metaanddatab%extrude(session)
!!$end if
!!$
!!$if (present(metaanddatad)) then
!!$  call metaanddatad%extrude(session)
!!$end if
!!$
!!$if (present(metaanddatac)) then
!!$  call metaanddatac%extrude(session)
!!$end if
!!$
!!$if (present(metaanddatar)) then
!!$  call metaanddatar%extrude(session)
!!$end if
!!$
!!$if (present(metaanddatav)) then
!!$  call metaanddatav%extrude(session,noattr,filter)
!!$end if
!!$
!!$if (present(metaanddatal)) then
!!$  call metaanddatal%extrude(session,noattr,filter)
!!$end if
!!$
!!$end subroutine dbasession_extrude

# ifndef F2003_FULL_FEATURES
!> clear a dballe session
subroutine dbasession_delete(session)
class (dbasession), intent(inout) :: session
integer :: ier
type(dbasession) :: defsession

if (c_e(session%sehandle)) then 
  ier = idba_fatto(session%sehandle)
end if

call session%memconnection%delete()

select type (session)
type is (dbasession)
  session = defsession
end select

!!$session%sehandle=imiss
!!$session%file=.false.
!!$session%template=cmiss
!!$session%filename=cmiss
!!$session%mode=cmiss
!!$session%format=cmiss
!!$session%simplified=.true.
!!$session%memdb=.false.
!!$session%category=imiss
!!$session%count=imiss

end subroutine dbasession_delete

#else

!> clear a dballe session
subroutine dbasession_delete(session)
type (dbasession), intent(inout) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_fatto(session%sehandle)
end if

!!$session%sehandle=imiss
!!$session%file=.false.
!!$session%template=cmiss
!!$session%filename=cmiss
!!$session%mode=cmiss
!!$session%format=cmiss
!!$session%simplified=.true.
!!$session%memdb=.false.
!!$session%category=imiss
!!$session%count=imiss

end subroutine dbasession_delete

#endif



!> rewind a file associated to a session (needed to restart reading)
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

!>\example example_dballe.F03
!!\brief Sample program to demostrate the dballe_class module.
!!
!! This module have examples to read/write/manipulate data from/to DB or BUFR.
