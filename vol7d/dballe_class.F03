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

!USE char_utilities
!USE geo_coord_class


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
    procedure :: unsetall => dbasession_unsetall
    procedure :: set => dbasession_set
    procedure :: prendilo => dbasession_prendilo
# endif
!  procedure :: dammelo => dbasession_dammelo
!  procedure :: prendilo => dbasession_prendilo
end type dbasession

interface dbasession
  procedure  dbasession_init      ! add constructor to shape generic interface
end interface


type,public,extends(vol7d_level) :: dbalevel
  contains

# ifdef F2003_FULL_FEATURES
    final :: dbalevel_delete
# else
    procedure :: delete => dbalevel_delete
# endif
  procedure :: display => dbalevel_display
  procedure :: set => dbalevel_set
!!$procedure :: spiega => dbalevel_spiega
end type dbalevel

interface dbalevel
  procedure   dbalevel_init     ! add constructor to shape generic interface
end interface

type,public,extends(vol7d_timerange) :: dbatimerange
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbatimerange_delete
# else
    procedure :: delete => dbatimerange_delete
# endif
  procedure :: display => dbatimerange_display
  procedure :: set => dbatimerange_set
!!$procedure :: spiega => dbatimerange_spiega
end type dbatimerange

interface dbatimerange
  procedure   dbatimerange_init     ! add constructor to shape generic interface
end interface

type,public,abstract :: dbadata
  character(len=9) :: btable
contains
  procedure(dbadata_set),deferred :: set
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
  procedure :: set => dbadatai_set
  procedure :: display => dbadatai_display
end type dbadatai

interface dbadatai
  procedure ::  dbadatai_init     ! add constructor to shape generic interface
end interface dbadatai

type,public, extends(dbadata) :: dbadatar
  real :: value
contains
  procedure :: set => dbadatar_set
  procedure :: display => dbadatar_display
end type dbadatar

interface dbadatar
  procedure ::  dbadatar_init     ! add constructor to shape generic interface
end interface dbadatar

type,public :: dbametadata
  type(dbalevel) :: level
  type(dbatimerange) :: timerange
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbametadata_delete
# else
    procedure :: delete => dbametadata_delete
# endif
  procedure :: set => dbametadata_set
  procedure :: display => dbametadata_display
end type dbametadata

interface dbametadata
  procedure   dbametadata_init     ! add constructor to shape generic interface
end interface

type, public :: dbadatac
class(dbadata),allocatable :: var
contains
procedure :: display => dbadatac_display
procedure :: set => dbadatac_set
end type dbadatac

type, public :: dbadatav
type(dbadatac),allocatable :: datac(:)
contains
procedure :: display => dbadatav_display
procedure :: set => dbadatav_set
end type dbadatav

contains

elemental subroutine dbalevel_delete(level)
class (dbalevel), intent(inout) :: level
!print *,"delete level"
end subroutine dbalevel_delete

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


subroutine dbadatac_set(data,session)
class(dbadatac), intent(in) :: data
type(dbasession), intent(in) :: session

call data%var%set(session)

end subroutine dbadatac_set

subroutine dbadatac_display(data)
class(dbadatac), intent(in) :: data

call data%var%display()

end subroutine dbadatac_display

subroutine dbadatav_set(data,session)
class(dbadatav), intent(in) :: data
type(dbasession), intent(in) :: session
integer :: i

do i=1, size(data%datac)
  call data%datac(i)%set(session)
enddo

end subroutine dbadatav_set

subroutine dbadatav_display(data)
class(dbadatav), intent(in) :: data
integer :: i

do i=1, size(data%datac)
  call data%datac(i)%display()
end do

end subroutine dbadatav_display

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

elemental subroutine dbatimerange_delete(timerange)
class (dbatimerange), intent(inout) :: timerange
!print *,"delete timerange"
end subroutine dbatimerange_delete

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


type(dbametadata) function dbametadata_init(level,timerange)

type(dbalevel), intent(in), optional :: level
type(dbatimerange), intent(in), optional :: timerange

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

end function dbametadata_init

subroutine dbametadata_display(metadata)
class(dbametadata), intent(in) :: metadata
call metadata%level%display()
call metadata%timerange%display()

end subroutine dbametadata_display

subroutine dbametadata_set(metadata,session)
class(dbametadata), intent(in) :: metadata
type(dbasession), intent(in) :: session

call metadata%level%set(session)
call metadata%timerange%set(session)

end subroutine dbametadata_set

elemental subroutine dbametadata_delete(metadata)
class (dbametadata), intent(inout) :: metadata
!print *,"delete level"
end subroutine dbametadata_delete

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
    call l4f_category_log(dbasession_init%category,L4F_INFO,"option anaflag, dataflag, attrflag defined with filename")
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
    call l4f_category_log(dbasession_init%category,L4F_INFO,"option mode, type defined without filename")
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
class (dbasession), intent(inout) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_unsetall(session%sehandle)
end if

end subroutine dbasession_unsetall

subroutine dbasession_prendilo(session)
class (dbasession), intent(inout) :: session
integer :: ier

if (c_e(session%sehandle)) then 
  ier = idba_prendilo(session%sehandle)
end if

end subroutine dbasession_prendilo



subroutine dbasession_set(session,metadata,datav,data,level,timerange)
class (dbasession), intent(in) :: session
type (dbametadata),optional ::metadata
class(dbadatav),optional ::datav
class(dbadata),optional ::data
type (dbalevel),optional ::level
type (dbatimerange),optional ::timerange
integer :: ier,i


if (present(metadata)) then
  call metadata%level%set(session)
  call metadata%timerange%set(session)
endif

if (present(level)) then
  call level%set(session)
endif

if (present(timerange)) then
  call timerange%set(session)
endif

if (present(datav)) then
  call datav%set(session)
end if

if (present(data)) then
  call data%set(session)
end if


end subroutine dbasession_set


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
