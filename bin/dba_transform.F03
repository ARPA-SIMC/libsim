program dbatransform

#include "config.h"

USE log4fortran
use dballe_class
USE optionparser_class
use err_handling
use missing_values

implicit none

TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
type(dbaconnection) :: connection
type(dbasession) :: session
type(dbametaanddatalist) :: metaanddatal
type(dbafilter) :: filter
logical :: status,version,display
character(len=9) :: ident
CHARACTER(len=512):: a_name,input_file,output_file 
INTEGER :: category,ier

!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name,a_name_force="dba_transform")
!init di log4fortran
ier = l4f_init()
!imposta a_name
category = l4f_category_get(a_name//".main")

opt = optionparser_new(description_msg= &
 'Filter data')

!"/home/ppatruno/tmp/rmap.bufr"

CALL optionparser_add(opt, 'i', 'input-file', input_file, default="", help='file to read')
CALL optionparser_add(opt, 'o', 'output-file', output_file, default="", help='file to read')
CALL optionparser_add(opt, 'u', 'ident', ident, default=cmiss,help='filter data for this ident')
CALL optionparser_add(opt, 'd', 'display', display,help='display data after filter')

! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0) ! generate a clean manpage
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line parameters')
  CALL raise_fatal_error()
ENDIF

IF (version) THEN
  WRITE(*,'(A,1X,A)')'dba_transform',VERSION
  CALL exit(0)
ENDIF

filter=dbafilter(ana=dbaana(ident=ident))

! connect to dsn type BUFR file for read
connection=dbaconnection(dsn="mem:")
session=dbasession(connection,write=.true.,wipe=.true.)
call session%messages_open_input(filename=input_file,mode="r",format="BUFR",simplified=.true.)

call session%messages_open_output(filename=output_file,mode="w",format="BUFR")


do while (session%messages_read_next())
  call session%set(filter=filter)
  call session%ingest(metaanddatal,filter=filter)

  if (display) call metaanddatal%display()

  call session%messages_write_next()
  call session%remove_all()
  status = metaanddatal%delete()

end do

# ifndef F2003_FULL_FEATURES
!close everythings
call session%delete()
#endif

end program dbatransform
