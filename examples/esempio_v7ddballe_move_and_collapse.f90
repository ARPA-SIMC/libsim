PROGRAM v7ddballe_move_and_collapse
! Example program to reduce to one the dimensions of levels and time without loss of data
! read from in.bufr and write to out.bufr

USE vol7d_dballe_class
USE vol7d_class

IMPLICIT NONE

TYPE(vol7d_dballe) :: v7d_in,v7d_out

integer :: indana,indtime,indlevel,indtimerange,indnetwork
integer :: indananew,indtimenew,indlevelnew,indtimerangenew,indnetworknew
integer :: inddativar,inddatiattr,inddativarattr


CHARACTER(len=80) :: filein="../data/in.bufr",fileout="../data/out.bufr"

character(len=512):: a_name
integer :: category,ier

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="ensascii")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(trim(a_name)//".main")
call l4f_category_log(category,L4F_INFO,"start")


! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d_in,filename=filein,file=.true.)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_out,filename=fileout,write=.true.,wipe=.true.,file=.true.)

CALL import(v7d_in)
call display(v7d_in%vol7d)

! move data to the firth index for time and level
do indana=1,size(v7d_in%vol7d%ana)
  do indtime=2,size(v7d_in%vol7d%time)
    do indlevel=2,size(v7d_in%vol7d%level)
      do indtimerange=1,size(v7d_in%vol7d%timerange)
        do indnetwork=1,size(v7d_in%vol7d%network)
          indananew=indana
          indtimenew=1
          indlevelnew=1
          indtimerangenew=indtimerange
          indnetworknew=indnetwork

          call move_datac (v7d_in%vol7d,&
           indana,indtime,indlevel,indtimerange,indnetwork,&
           indananew,indtimenew,indlevelnew,indtimerangenew,indnetworknew)

        end do
      end do
    end do
  end do
end do

! set to missing level and time > 1
do  indlevel=2,size(v7d_in%vol7d%level)
  call init (v7d_in%vol7d%level(indlevel))
end do

do  indtime=2,size(v7d_in%vol7d%time)
  call init (v7d_in%vol7d%time(indtime))
end do

!copy with remove
call vol7d_copy(v7d_in%vol7d,v7d_out%vol7d,miss=.true.)
CALL delete (v7d_in)

!set to missing the ambigous descriptions
call init (v7d_out%vol7d%level(1))
call init (v7d_out%vol7d%time(1))

call display(v7d_out%vol7d)
CALL export(v7d_out)
CALL delete(v7d_out) 

!chiudo il logger
call l4f_category_log(category,L4F_INFO,"end")
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM v7ddballe_move_and_collapse
