program dba_qcfilter

#include "config.h"

USE log4fortran
use kinds
use dballe_class
use modqc
use optional_values
USE optionparser_class
use err_handling

implicit none

type(dbasession) :: sessionfrom,sessionto
type(dbaconnection) :: connectionfrom


type(dbametaanddata),allocatable :: metaanddatav(:)
type(dbafilter) :: filter,filterd
integer :: i,j
logical :: qcsummaryflag,version,preserve
integer (kind=int_b):: flag0,flag1,flag2,flag3
TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
CHARACTER(len=512):: a_name,input_file,output_file 
INTEGER :: category,ier
type(dbadcv) :: starvars


!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name,a_name_force="dba_qcfilter")
!init di log4fortran
ier = l4f_init()
!imposta a_name
category = l4f_category_get(a_name//".main")

opt = optionparser_new(description_msg= &
 'Filter data using Quality Control information. &
 &Flag used are *B33192,*B33193,*B33194,*B33196. &
 &Station constant data are reported as is')

CALL optionparser_add(opt, 'i', 'input-file', input_file, default="(stdin)", help='file to read')
CALL optionparser_add(opt, 'o', 'output-file', output_file, default="(stdout)", help='file to read')
CALL optionparser_add(opt, 'p', 'preserve', preserve, help='preserve wrong data, remove attribute, insert B33007=0 for wrong data')

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
  WRITE(*,'(A,1X,A)')'dba_qcfilter',VERSION
  CALL exit(0)
ENDIF


!!$! check input/output files
!!$i = iargc() - optind
!!$IF (i < 0) THEN
!!$  CALL optionparser_printhelp(opt)
!!$  CALL l4f_category_log(category,L4F_ERROR,'input file missing')
!!$  CALL raise_fatal_error()
!!$ELSE IF (i < 1) THEN
!!$  CALL optionparser_printhelp(opt)
!!$  CALL l4f_category_log(category,L4F_ERROR,'output file missing')
!!$  CALL raise_fatal_error()
!!$ENDIF
!CALL getarg(iargc()-1, input_file)
!CALL getarg(iargc(), output_file)

allocate (starvars%dcv(4))
allocate (starvars%dcv(1)%dat,source=dbadatab("*B33192"))
allocate (starvars%dcv(2)%dat,source=dbadatab("*B33193"))
allocate (starvars%dcv(3)%dat,source=dbadatab("*B33194"))
allocate (starvars%dcv(4)%dat,source=dbadatab("*B33196"))
filter=dbafilter(starvars=starvars)
!call filter%display()

! connect to dsn type BUFR file for read
connectionfrom=dbaconnection(dsn="mem:")
sessionfrom=dbasession(connectionfrom,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")
call sessionfrom%messages_open_input(filename=input_file,mode="r",format="BUFR",simplified=.true.)

call sessionfrom%messages_open_output(filename=output_file,mode="w",format="BUFR")

!!$! connect to dsn type BUFR file for write
!!$sessionto=dbasession(filename=output_file,wipe=.true.,write=.true.,memdb=.false.)


do while (sessionfrom%messages_read_next())
  call sessionfrom%set(filter=filter)
  call sessionfrom%extrude(metaanddatav,filter=filter)
  do i =1,size(metaanddatav)
!    call metaanddatav(i)%display()

    do j=1, size(metaanddatav(i)%dataattrv%dataattr)

!      internal compiler error
!      associate(dataattr =>  metaanddatav(i)%dataattrv%dataattr(j))

      call metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(4)%dat%get(flag0)
      call metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(1)%dat%get(flag1)
      call metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(2)%dat%get(flag2)
      call metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(3)%dat%get(flag3)

      qcsummaryflag =qcsummaryflagb(flag0,flag1,flag2,flag3)
      

      if (preserve) then

!         remove attr from mem db 
        call sessionfrom%dissolveattr([metaanddatav(i)%metadata])

        if (.not.qcsummaryflag) then
!         and insert confidence
          deallocate(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
          allocate (metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(1))
          allocate (metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(1)%dat,source=dbadatai("*B33007",0))
          call sessionfrom%ingest(dataattr=metaanddatav(i)%dataattrv%dataattr(j))
        end if

      else

        if (.not.qcsummaryflag) then

!         set data to missing
!!$          allocate(metaanddatav(i)%dataattrv%dataattr(j)%dbadat%dat,source=dbadatac())
!!$          allocate(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dat(4)%dat,source=dbadatac())
!!$          allocate(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dat(1)%dat,source=dbadatac())
!!$          allocate(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dat(2)%dat,source=dbadatac())
!!$          allocate(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dat(3)%dat,source=dbadatac())


!         remove data and attr from mem db
          filterd=dbafilter(var=metaanddatav(i)%dataattrv%dataattr(j)%dat%btable)
          call sessionfrom%set(filter=filterd)
          call sessionfrom%dissolve([metaanddatav(i)%metadata])

        else
!         remove attr from mem db 
          call sessionfrom%dissolveattr([metaanddatav(i)%metadata])
        end if

      end if
!      end associate
    end do
!!$    call sessionto%ingest(metaanddata=metaanddatav(i))
  end do
!!$  call sessionto%prendilo()

!  print *,"contextana"
!!$  call sessionfrom%set(filter=dbafilter(contextana=.true.))
!!$  call sessionfrom%extrude(metaanddatav)
!!$    
!!$  do i =1,size(metaanddatav)
!!$    call metaanddatav(i)%display()
!!$    call sessionto%ingest(metaanddata=metaanddatav(i))
!!$  end do
!!$  call sessionto%close_message()

  call sessionfrom%messages_write_next()
  call sessionfrom%remove_all()
end do

!close everythings
call sessionto%delete()
call sessionfrom%delete()


end program dba_qcfilter
