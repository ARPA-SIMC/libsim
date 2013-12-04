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
public dbalevel, dbatimerange, dbarecord, dbahandle, dba
public dbadata,dbadatai, dbadatar

character (len=255),parameter:: subcategory="dballe_class"

type,extends(vol7d_level) :: dbalevel
  contains

# ifdef F2003_FULL_FEATURES
    final :: dbalevel_delete
# else
    procedure :: delete => dbalevel_delete
# endif
  procedure :: display => dbalevel_display
!!$procedure :: spiega => dbalevel_spiega
end type dbalevel

interface dbalevel
  procedure   dbalevel_init     ! add constructor to shape generic interface
end interface

type,extends(vol7d_timerange) :: dbatimerange
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbatimerange_delete
# else
    procedure :: delete => dbatimerange_delete
# endif
  procedure :: display => dbatimerange_display
!!$procedure :: spiega => dbatimerange_spiega
end type dbatimerange

interface dbatimerange
  procedure   dbatimerange_init     ! add constructor to shape generic interface
end interface

type dbadata
  character(len=9) :: btable
end type dbadata

type , extends(dbadata) :: dbadatai
  integer :: value
end type dbadatai

interface dbadatai
  procedure   dbadatai_init     ! add constructor to shape generic interface
end interface dbadatai

type , extends(dbadata) :: dbadatar
  real :: value
end type dbadatar

interface dbadatar
  procedure   dbadatar_init     ! add constructor to shape generic interface
end interface dbadatar

type  dbarecord
  type(dbalevel) :: level
  type(dbatimerange) :: timerange
  class(dbadata),allocatable :: data(:)
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbarecord_delete
# else
    procedure :: delete => dbarecord_delete
# endif
  procedure :: display => dbarecord_display
end type dbarecord

interface dbarecord
  procedure   dbarecord_init     ! add constructor to shape generic interface
end interface

type  dbahandle
  integer :: dbhandle=imiss
  integer :: handle_err=imiss
  integer :: category !< log4fortran
  contains
# ifdef F2003_FULL_FEATURES
    final :: dbahandle_delete
# else
    procedure :: delete => dbahandle_delete
# endif
end type dbahandle

interface dbahandle
  procedure  dbahandle_init      ! add constructor to shape generic interface
end interface

type, extends(dbahandle) ::  dba
  integer :: sehandle=imiss
  logical :: file=.false.
  contains
# ifdef F2003_FULL_FEATURES
    final :: dba_delete
# else
    procedure :: delete => dba_delete
# endif
!  procedure :: dammelo => dba_dammelo
!  procedure :: prendilo => dba_prendilo
end type dba

interface dba
  procedure  dba_init      ! add constructor to shape generic interface
end interface


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

type(dbatimerange) function dbatimerange_init(timerange, p1, p2)
INTEGER,INTENT(IN),OPTIONAL :: timerange !< tipo di intervallo temporale
INTEGER,INTENT(IN),OPTIONAL :: p1 !< valore per il primo istante temporale
INTEGER,INTENT(IN),OPTIONAL :: p2 !< valore per il secondo istante temporale

call init (dbatimerange_init%vol7d_timerange,timerange, p1, p2)
end function dbatimerange_init


type(dbarecord) function dbarecord_init(level,timerange,data)

type(dbalevel), intent(in), optional :: level
type(dbatimerange), intent(in), optional :: timerange
class(dbadata),intent(in),optional :: data(:)


if (present(level)) then
  dbarecord_init%level=level
else
  dbarecord_init%level=dbalevel()
end if

if (present(timerange)) then
  dbarecord_init%timerange=timerange
else
  dbarecord_init%timerange=dbatimerange()
end if

if (present(data)) then
  allocate(dbarecord_init%data(size(data)),source=data)
else
  allocate(dbadata::dbarecord_init%data(0))
end if


end function dbarecord_init

subroutine dbarecord_display(record)
class(dbarecord), intent(in) :: record
call record%level%display()
call record%timerange%display()

end subroutine dbarecord_display

elemental subroutine dbarecord_delete(record)
class (dbarecord), intent(inout) :: record
!print *,"delete level"
end subroutine dbarecord_delete

type(dbahandle) function dbahandle_init(dsn, user, password,categoryappend,idbhandle)
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
dbahandle_init%category=l4f_category_get(a_name)

! impostiamo la gestione dell'errore
ier=idba_error_set_callback(0,dballe_error_handler, &
 dbahandle_init%category,dbahandle_init%handle_err)

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

  ier=idba_presentati(dbahandle_init%dbhandle,quidsn,quiuser,quipassword)
else
  dbahandle_init%dbhandle=optio_i(idbhandle)
end if

end function dbahandle_init

subroutine dbahandle_delete(handle)
class (dbahandle), intent(inout) :: handle
integer :: ier

print *,"dbhandle delete"

if (c_e(handle%dbhandle)) then 
  ier = idba_arrivederci(handle%dbhandle)
end if

end subroutine dbahandle_delete

type(dba) function dba_init(anaflag, dataflag, attrflag,filename,mode,type,write,wipe,repinfo)
character (len=*), intent(in), optional :: anaflag
character (len=*), intent(in), optional :: dataflag
character (len=*), intent(in), optional :: attrflag
character (len=*), intent(in), optional :: filename
character (len=*), intent(in), optional :: mode
logical,INTENT(in),OPTIONAL :: write !< abilita la scrittura sul DSN/file ( default=.false. )
logical,INTENT(in),OPTIONAL :: wipe !<  svuota il DSN/file e/o lo prepara per una scrittura ( default=.false. )
character(len=*), INTENT(in),OPTIONAL :: repinfo !< eventuale file repinfo.csv usato con wipe ( default="" )
character(len=*),intent(in),optional :: type !< the file format. It can be "BUFR" or "CREX". (default="BUFR")

integer :: ier
character (len=5) :: lanaflag,ldataflag,lattrflag
character (len=1) :: lmode,ltype
logical :: quiwrite,quiwipe
character(len=255) :: quirepinfo
logical :: exist

print *,"dba init"

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
    call l4f_category_log(dba_init%dbahandle%category,L4F_INFO,"option anaflag, dataflag, attrflag defined with filename")
    CALL raise_error()
  end if

  inquire(file=filename,EXIST=exist)

  lmode="r"
  if (quiwrite)then
    if (quiwipe.or..not.exist) then
      lmode="w"
    else
      lmode="a"
      call l4f_category_log(dba_init%dbahandle%category,L4F_INFO,"file exists; appending data to file")
    end if
  else
    if (.not.exist) then
      call l4f_category_log(dba_init%dbahandle%category,L4F_ERROR,"file does not exist; cannot open file for read")
      CALL raise_fatal_error()
    end if
  end if

  if (present(mode)) lmode = mode

  dba_init%file=.true.

  ier =  idba_messaggi(dba_init%sehandle,filename, lmode, ltype)
else

  if (present(mode).or.present(type)) then
    call l4f_category_log(dba_init%dbahandle%category,L4F_INFO,"option mode, type defined without filename")
    CALL raise_error()
  end if

  call optio(anaflag,lanaflag)
  if (.not. c_e(lanaflag)) lanaflag = "read"

  call optio(dataflag,ldataflag)
  if (.not. c_e(ldataflag)) ldataflag = "read"

  call optio(attrflag,lattrflag)
  if (.not. c_e(lattrflag)) lattrflag = "read"

  ier = idba_preparati(dba_init%dbahandle%dbhandle,dba_init%sehandle, lanaflag, ldataflag, lattrflag)

  dba_init%file=.true.

  if (quiwipe)ier=idba_scopa (dba_init%sehandle,quirepinfo)

end if


end function dba_init

subroutine dba_delete(handle)
class (dba), intent(inout) :: handle
integer :: ier

print *,"dba delete"

if (c_e(handle%sehandle)) then 
  ier = idba_fatto(handle%sehandle)
end if

end subroutine dba_delete


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
call l4f_category_log(category,l4f_level,message)

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
