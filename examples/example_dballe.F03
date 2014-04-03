program example_dballe

use dballe_class
use datetime_class
implicit none

integer :: i
type(dbasession) :: session,sessionfrom,sessionto
type(dbaconnection) :: connection,connectionfrom

! connect to dsn type DBA
connection=dbaconnection(dsn="sqlite:/tmp/dballe.sqlite")
session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

call write1()    ! write etherogeneous ensamble of data with attributes and constant data using macro object
call write2()    ! write an omogeneous vector of data
call write3()    ! write an etherogeneous ensamble of data
call write4()    ! write an etherogeneous ensamble of data and attributes
call write5()    ! write a content of DSN to a file with filter
call read2()     ! read data and attributes for data filtered and ordered by btable with type predefined
call delete1()   ! delete one var from the entire DB
call delete2()   ! delete one var only where are some defined metadati
call delete3()   ! delete some attributes from one var only where are some defined metadati
call read1()     ! read ana, data and attributes for constant data, data and attributes for data
call read3()     ! read an omogeneous vector of data
call read4l()    ! read data and attributes for data in a double linked list
call copy1()     ! copy data and attributes of everythings to an other dsn
call copy2()     ! copy data and constant data to an other dsn

!close everythings
call session%delete()
call connection%delete()


! connect to dsn type BUFR file for write
session=dbasession(filename="example_dballe.bufr",wipe=.true.,write=.true.,memdb=.false.)

call write0()    ! write ana on file
call write1()    ! write etherogeneous ensamble of data with attributes and constant data using macro object
call write2()    ! write an omogeneous vector of data
call write3()    ! write an etherogeneous ensamble of data
call write4()    ! write an etherogeneous ensamble of data and attributes

!close everythings
call session%delete()


! connect to dsn type BUFR file for read
session=dbasession(filename="example_dballe.bufr",memdb=.false.)

call read1lf()     ! read ana, data and attributes for constant data, data and attributes for data in list
call read2lf()     ! read data and attributes for data filtered and ordered by btable with type predefined in list

!! note: I cannot read from file without filter (filter do not work on file) and put everythings in real matrix
!! we get error putting somethings that do not fit in real (like station name)
!! call read3lf()     ! read an omogeneous vector of data

! use memdb
call read3lfmem()    ! read an omogeneous vector of data from file using list and mem (and filters)
call readmem()       ! read an list of data from file  using mem (and filters)

!close everythings
call session%delete()


! connect to dsn type BUFR file for read
sessionfrom=dbasession(filename="example_dballe.bufr",memdb=.false.)
! connect to dsn type BUFR file for write
sessionto=dbasession(filename="example_dballe_copy1f.bufr",wipe=.true.,write=.true.,memdb=.false.)
call copy1f()     ! copy data and attributes of everythings to an other file
! todo
!call copy2f()     ! copy data and constant data to an other file
!close everythings
call sessionto%delete()
call sessionfrom%delete()


! use memdb
! connect to dsn type DBA
connectionfrom=dbaconnection(dsn="mem:")
sessionfrom=dbasession(connectionfrom,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")
call sessionfrom%messages_open_input(filename="example_dballe.bufr",mode="r",format="BUFR",simplified=.true.)

! connect to dsn type BUFR file for write
sessionto=dbasession(filename="example_dballe_copy1fmem.bufr",wipe=.true.,write=.true.,memdb=.false.)
call copy1fmem()     ! copy data and attributes of everythings to an other file using mem for input
call sessionto%delete()

call sessionfrom%messages_open_output(filename="example_dballe_copyf2mem.bufr")
call copy1f2mem()     ! copy data and attributes of everythings to an other file using mem for input and output

!close everythings
call sessionfrom%delete()
call connectionfrom%delete()

contains

subroutine write0()

type(dbasession) :: sessionana
type(dbaanalist) :: anal
logical :: status

! connect to dsn type BUFR file for write
sessionana=dbasession(filename="example_dballe_ana.bufr",wipe=.true.,write=.true.,memdb=.false.)


call anal%append(dbaana(lon=11.d0,lat=45.d0))
call anal%append(dbaana(lon=12.d0,lat=45.d0))
call anal%append(dbaana(lon=13.d0,lat=45.d0))

call anal%display()

call anal%rewind()
!ingest ana
do while (anal%element())
  call sessionana%ingest(ana=anal%current())
  call anal%next()
end do

call sessionana%delete()
status=anal%delete()

end subroutine write0


subroutine write1()

type(dbametaanddata),allocatable :: metaanddata(:)
type(dbadcv) :: attrv

print *,"----------------------------------------------"
print *,"--------------- write1 ------------------------"

allocate(metaanddata(2))   ! one metadata for data and one for constant data

metaanddata(1)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! create  an etherogeneous ensamble of data
allocate (metaanddata(1)%dataattrv%dataattr(2))

! first data
allocate (metaanddata(1)%dataattrv%dataattr(1)%dat,source=dbadatai("B13003",85))

! create an etherogeneous ensamble of attr
allocate (attrv%dcv(3))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
allocate (attrv%dcv(3)%dat,source=dbadatar("*B33194",70.))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(1)%attrv=attrv

! second data
allocate (metaanddata(1)%dataattrv%dataattr(2)%dat,source=dbadatai("B12101",27315))
! create an etherogeneous ensamble of attr
deallocate(attrv%dcv)
allocate (attrv%dcv(2))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(2)%attrv=attrv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! station costant data
! copy the same metadata setting that here we have constant in time data
metaanddata(2)%metadata=metaanddata(1)%metadata%dbacontextana()
! create  an etherogeneous ensamble of data
allocate (metaanddata(2)%dataattrv%dataattr(2))
allocate (metaanddata(2)%dataattrv%dataattr(1)%dat,source=dbadatai("B07030",223))
allocate (metaanddata(2)%dataattrv%dataattr(1)%attrv%dcv(0))          ! we do not want attributes
allocate (metaanddata(2)%dataattrv%dataattr(2)%dat,source=dbadatac("B01019","My beautifull station"))
allocate (metaanddata(2)%dataattrv%dataattr(2)%attrv%dcv(0))          ! we do not want attributes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! display and save everythings
do i=1,size(metaanddata)
  call metaanddata(i)%display()
  call session%ingest(metaanddata=metaanddata(i))
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine write1


subroutine write2()

type(dbadatar),allocatable :: data(:)

type(dbalevel) :: level
type(dbatimerange) :: timerange
type(dbaana) :: ana
type(dbanetwork) :: network
type(dbadatetime) :: datetime

print *,"----------------------------------------------"
print *,"--------------- write2 ------------------------"


!clear the dballe session
call session%unsetall()

! set and display metadata
level=dbalevel(level1=105, l1=2000)
call level%display()
timerange=dbatimerange(timerange=4, p1=3600,p2=7200)
call timerange%display()
!ana=dbaana(coord=dbacoord(ilon=1000000,ilat=4500000))
ana=dbaana(lon=11.d0,lat=45.d0)
call ana%display()
network=dbanetwork("generic")
call network%display()
datetime=dbadatetime(datetime_new(2014,01,06,18,00))
call datetime%display()

! can set metadata  step by step
call session%set(level=level)
call session%set(timerange=timerange)
call session%set(ana=ana)
call session%set(network=network)
call session%set(datetime=datetime)

! I can use the reverse vision  step by step
!call level%dbaset(session)
!call timerange%dbaset(session)
!call ana%dbaset(session)
!call network%dbaset(session)
!call datetime%dbaset(session)

! create an omogeneous vector of data
allocate (data(2),source=[dbadatar("B12102",265.33),dbadatar("B12101",273.15)])

!set and display omogeneous data
do i =1,size(data)
  call data(i)%display()
  call session%set(data=data(i))
  !call data(i)%dbaset(session)
end do

!write it in dsn or file
call session%prendilo()
!!$session%enq(metadata)

!close message if I am writing on file
call session%close_message()


end subroutine write2


subroutine write3()

type(dbametadata) :: metadata
type(dbadcv) :: datav
type(dbalevel) :: level
type(dbatimerange) :: timerange
type(dbaana) :: ana
type(dbanetwork) :: network
type(dbadatetime) :: datetime

print *,"----------------------------------------------"
print *,"--------------- write3 ------------------------"


!clear the dballe session
call session%unsetall()

! set metadata
level=dbalevel(level1=105, l1=2000)
timerange=dbatimerange(timerange=4, p1=3600,p2=7200)
ana=dbaana(lon=12.d0,lat=45.d0)
network=dbanetwork("generic")
datetime=dbadatetime(datetime_new(2014,01,06,18,00))

!assemble metadata
metadata=dbametadata(level=level,timerange=timerange,ana=ana,network=network,datetime=datetime)
call metadata%display()

! I can set metadata one shot
call session%set(metadata)
! or in the reverse vision
!call metadata%dbaset(session)

call metadata%display()

! create and display an etherogeneous ensamble of data
allocate (datav%dcv(2))
allocate (datav%dcv(1)%dat,source=dbadatai("B12102",26312))
allocate (datav%dcv(2)%dat,source=dbadatar("B12101",273.15))
call datav%display()
!set data
call session%set(datav=datav)

! or in the reverse vision
!call datav%dbaset(session)

!write it in dsn
call session%prendilo()
!!$session%enq(metadata)

!close message if I am writing on file
call session%close_message()

end subroutine write3


subroutine write4()

type(dbametadata) :: metadata
type(dbadataattrv) :: dataattrv
type(dbadcv) :: attrv
type(dbadcv) :: datav

print *,"----------------------------------------------"
print *,"--------------- write4 ------------------------"

! clear the dballe session
call session%unsetall()

! define metadata
metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=13.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

call session%set(metadata)

! create and display an etherogeneous ensamble of data
allocate (datav%dcv(2))
allocate (datav%dcv(1)%dat,source=dbadatai("B12102",26312))
allocate (datav%dcv(2)%dat,source=dbadatar("B12101",273.15))

! create and display an etherogeneous ensamble of attr
allocate (attrv%dcv(3))
allocate (attrv%dcv(1)%dat,source=dbadatai("*B33192",30))
allocate (attrv%dcv(2)%dat,source=dbadatac("*B33193","70"))
allocate (attrv%dcv(3)%dat,source=dbadatad("*B33194",50.d0))
call attrv%display()

! assemble data and attribute
allocate (dataattrv%dataattr(2))
! first with attribute
allocate (dataattrv%dataattr(1)%dat,source=datav%dcv(1)%dat)
dataattrv%dataattr(1)%attrv=attrv
! second without attribute
allocate (dataattrv%dataattr(2)%dat,source=datav%dcv(2)%dat)
allocate (dataattrv%dataattr(2)%attrv%dcv(0))

call dataattrv%display()

! write data and attribute
call session%ingest(dataattrv=dataattrv)


! work on constant station data
call session%set(metadata%dbacontextana())

! write the same data and attribute as constant station data
call session%ingest(dataattrv=dataattrv)

end subroutine write4


subroutine write5()

print *,"----------------------------------------------"
print *,"--------------- write5 ------------------------"

call session%messages_open_output(filename="example_dballe_write5.bufr")

call session%set(filter=dbafilter(coordmin=dbacoord(lon= 9.d0,lat=44.d0),&
                 coordmax=dbacoord(lon=11.d0,lat=46.d0)))
call session%messages_write_next()

end subroutine write5



subroutine read1()

type(dbametaanddata),allocatable :: metaanddatav(:)
type(dbaana),allocatable :: ana(:)
type(dbafilter) :: filter
integer :: i

print *,"----------------------------------------------"
print *,"--------------- read1 ------------------------"

call session%set(filter=dbafilter())

print *, "anagrafica:"
filter=dbafilter(coordmin=dbacoord(lon= 9.d0,lat=44.d0),&
                 coordmax=dbacoord(lon=11.d0,lat=46.d0))
call session%set(filter=filter)
call session%extrudea(ana)
do i=1,size(ana)
  call ana(i)%display()
end do
deallocate(ana)

print *, "dati di anagrafica:"
call session%set(filter=dbafilter(contextana=.true.))
call session%extrude(metaanddatav)
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
end do
deallocate(metaanddatav)

print *, "dati dati:"
filter=dbafilter(var="B12102")
call filter%display()
call session%set(filter=filter)
call session%extrude(metaanddatav)

do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
end do

deallocate(metaanddatav)

end subroutine read1

subroutine read1lf()

type(dbametaanddatalist) :: metaanddatal
type(dbaanalist) :: anal
logical :: status

print *,"----------------------------------------------"
print *,"--------------- read1lf ------------------------"

! here (reading from file) we cannot use filters !!!

! rewind
call session%filerewind()

print *, "anagrafica:"
call session%extrudea(anal)
call anal%display()
status=anal%delete()

! rewind
call session%filerewind()

print *, "dati di anagrafica:"
call session%extrude(metaanddatal,filter=dbafilter(contextana=.true.))
call metaanddatal%display()
status = metaanddatal%delete()

! rewind
call session%filerewind()

print *, "dati dati:"
metaanddatal=dbametaanddatalist()
call session%extrude(metaanddatal)
call metaanddatal%display()
status = metaanddatal%delete()

end subroutine read1lf

subroutine read2()

type(dbametaanddata),allocatable :: metaanddatav(:)
type(dbafilter) :: filter
integer :: i
type(dbadcv) :: vars,starvars
integer :: ivalue
real :: rvalue
character(len=128) :: cvalue

print *,"----------------------------------------------"
print *,"--------------- read2 ------------------------"

allocate (vars%dcv(2))
allocate (vars%dcv(1)%dat,source=dbadatai("B12101"))
allocate (vars%dcv(2)%dat,source=dbadatac("B12102"))

filter=dbafilter(vars=vars)
print *, "filter:"
call filter%display()
call session%set(filter=filter)
call session%extrude(metaanddatav,filter=filter,dataonly=.true.)

print *, "dati dati:"
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
end do


!!!!!!!!!!!!!!!!
! how use values
associate (dato => metaanddatav(1)%dataattrv%dataattr(1)%dat)
print *,dato%btable
! with cast
select type (dato)
type is (dbadatai)
  print *,"cast integer value",dato%value
type is (dbadatar)
  print *,"cast real value",dato%value
type is (dbadatac)
  print *,"cast character value",dato%value
end select

! calling %get 
call dato%get(ivalue)
if (c_e(ivalue)) print *,"get  integer value",ivalue
call dato%get(rvalue)
if (c_e(rvalue)) print *,"get  real value",rvalue
call dato%get(cvalue)
if (c_e(cvalue)) print *,"get  char value",cvalue
end associate
!!!!!!!!!!!!!!!!


deallocate (vars%dcv)
allocate (vars%dcv(2))
allocate (vars%dcv(1)%dat,source=dbadatai("B13003"))
allocate (vars%dcv(2)%dat,source=dbadatac("B12102"))
allocate (starvars%dcv(3))
allocate (starvars%dcv(1)%dat,source=dbadatai("*B33192"))
allocate (starvars%dcv(2)%dat,source=dbadatac("*B33193"))
allocate (starvars%dcv(3)%dat,source=dbadatad("*B33194"))

filter=dbafilter(vars=vars,starvars=starvars)
print *, "filter:"
call filter%display()
call session%set(filter=filter)
call session%extrude(metaanddatav,filter=filter)

print *, "dati dati:"
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
end do

deallocate(metaanddatav)

end subroutine read2



subroutine read2lf()

type(dbametaanddatalist) :: metaanddatal
type(dbafilter) :: filter
type(dbadcv) :: vars,starvars
logical :: status

print *,"----------------------------------------------"
print *,"--------------- read2lf ------------------------"

! rewind
call session%filerewind()

allocate (vars%dcv(2))
allocate (vars%dcv(1)%dat,source=dbadatai("B13003"))
allocate (vars%dcv(2)%dat,source=dbadatac("B12102"))
allocate (starvars%dcv(3))
allocate (starvars%dcv(1)%dat,source=dbadatai("*B33192"))
allocate (starvars%dcv(2)%dat,source=dbadatac("*B33193"))
allocate (starvars%dcv(3)%dat,source=dbadatad("*B33194"))

filter=dbafilter(vars=vars,starvars=starvars)
!print *, "filter:"
!call filter%display()

call session%extrude(metaanddatal,filter=filter)

print *, "dati dati:"
call metaanddatal%display()

status=metaanddatal%delete()

end subroutine read2lf


subroutine read3()

type(dbametaanddatar),allocatable :: metaanddatarv(:)
type(dbafilter) :: filter
integer :: i

print *,"----------------------------------------------"
print *,"--------------- read3 ------------------------"

print *, "dati dati:"
filter=dbafilter(var="B12102")
call filter%display()
call session%set(filter=filter)
call session%extrude(metaanddatarv)

do i=1,size(metaanddatarv)
  call metaanddatarv(i)%display()
end do

print *,"max=",maxval(metaanddatarv(:)%dbadatar%value)
print *,"livelli", metaanddatarv(:)%metadata%level

deallocate(metaanddatarv)

end subroutine read3



!!$subroutine read3l()
!!$
!!$type(dbametaanddatarlist) :: metaanddatarl
!!$type(dbametaanddatar),allocatable :: metaanddatarv(:)
!!$type(dbafilter) :: filter
!!$logical :: status
!!$
!!$print *,"----------------------------------------------"
!!$print *,"--------------- read3l ------------------------"
!!$
!!$! rewind
!!$call session%filerewind()
!!$
!!$print *, "dati dati:"
!!$filter=dbafilter(var="B12102")
!!$call session%set(filter=filter)
!!$call session%extrude(metaanddatarl)
!!$
!!$call metaanddatarl%display()
!!$
!!$metaanddatarv=metaanddatarl%toarray()
!!$print *,"max=",maxval(metaanddatarv(:)%dbadatar%value)
!!$
!!$status = metaanddatarl%delete()
!!$
!!$end subroutine read3l


subroutine read3lfmem()

type(dbametaanddatarlist) :: metaanddatarl
type(dbametaanddatar),allocatable :: metaanddatarv(:)
type(dbafilter) :: filter
logical :: status
integer :: i

print *,"----------------------------------------------"
print *,"--------------- read3lfmem ------------------------"

! connect to dsn type DBA
connection=dbaconnection(dsn="mem:")
session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write",memdb=.true.)
call session%messages_open_input(filename="example_dballe.bufr",mode="r",format="BUFR",simplified=.true.)

filter=dbafilter(var="B12101")

! data
do while (session%messages_read_next())

  call session%set(filter=filter)

  call session%extrude()
  call session%extrude(metaanddatarv)

  do i=1,size(metaanddatarv)
    call metaanddatarv(i)%display()
    call metaanddatarl%append(metaanddatarv(i))
  end do

  call session%remove_all()
  deallocate (metaanddatarv)
end do


print *, "dati dati:"
call metaanddatarl%display()

metaanddatarv=metaanddatarl%toarray()
print *,"max=",maxval(metaanddatarv(:)%dbadatar%value)

status = metaanddatarl%delete()

end subroutine read3lfmem


subroutine readmem()

type(dbametaanddatalist) :: metaanddatal
type(dbafilter) :: filter
logical :: status

print *,"----------------------------------------------"
print *,"--------------- readmem ------------------------"

! create mem db where put bufr from file
session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write",&
 filename="example_dballe.bufr",mode="r",format="BUFR",memdb=.true.)

filter=dbafilter(var="B12101")
! in this case we have to pass filter and do not set it
call session%extrude(metaanddatal,filter=filter)

print *, "dati dati:"
call metaanddatal%display()

status = metaanddatal%delete()

end subroutine readmem


subroutine read4l()

type(dbametaanddata) :: myelement
type(dbametaanddatalist) :: metaanddatal
type(dbafilter) :: filter

print *,"----------------------------------------------"
print *,"--------------- read4l ------------------------"

print *, "dati dati:"
filter=dbafilter(var="B12102")
call session%set(filter=filter)

print*,"prima di extrude"
call session%extrude(metaanddatal)
print*,"prima di display"
call metaanddatal%display()

print *,"number of list elements=",metaanddatal%countelements()
print *,"seek return status =", metaanddatal%seek(2)
myelement=metaanddatal%current()
print *,"list index 2 ="
call myelement%display()
print *,"status delete=", metaanddatal%delete()

end subroutine read4l


subroutine delete1()

type(dbafilter) :: filter

print *,"----------------------------------------------"
print *,"--------------- delete1 ----------------------"

filter=dbafilter(var="B12101")
call session%set(filter=filter)
call session%dissolve()

end subroutine delete1


subroutine delete2()

type(dbametadata),allocatable :: metadata(:)
type(dbafilter) :: filter

print *,"----------------------------------------------"
print *,"--------------- delete2 ----------------------"


allocate(metadata(1))

metadata(1)=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=11.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))


filter=dbafilter(var="B12102")
call session%set(filter=filter)
call session%dissolve(metadata)

deallocate(metadata)

end subroutine delete2



subroutine delete3()

type(dbametadata),allocatable :: metadata(:)
type(dbafilter) :: filter

print *,"----------------------------------------------"
print *,"--------------- delete3 ----------------------"


allocate(metadata(1))

metadata(1)=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=13.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

filter=dbafilter(var="B12102",starvarlist="*B33194,*B33193")
call session%set(filter=filter)
call session%dissolveattr(metadata)

deallocate(metadata)

end subroutine delete3

subroutine copy1()
type(dbasession)    :: sessioncp
type(dbaconnection) :: connectioncp
type(dbametaanddata):: metaanddata

print *,"----------------------------------------------"
print *,"--------------- copy1 ----------------------"

! connect to dsn
connectioncp=dbaconnection(dsn="sqlite:/tmp/dballecopy1.sqlite")
!connectioncp=dbaconnection(dsn="mem:/tmp/dballecopy1")
sessioncp=dbasession(connectioncp,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

! data
call session%set(filter=dbafilter())
call session%extrude_metaanddata()
do while (c_e(session%count))
  call session%extrude_metaanddata (metaanddata)
  call sessioncp%ingest(metaanddata=metaanddata)
  if (session%file) call session%close_message()
end do

! constant data
call session%set(filter=dbafilter(contextana=.true.))
call session%extrude_metaanddata()
do while (c_e(session%count))
  call session%extrude_metaanddata (metaanddata)
  call sessioncp%ingest(metaanddata=metaanddata)
  if (session%file) call session%close_message()
end do


!close everythings
call sessioncp%delete()
call connectioncp%delete()

end subroutine copy1


subroutine copy1f()
type(dbametaanddata), allocatable:: metaanddatav(:)
integer :: i

print *,"----------------------------------------------"
print *,"--------------- copy1f ----------------------"

! data
call sessionfrom%filerewind()

call sessionfrom%set(filter=dbafilter())
call sessionfrom%extrude(metaanddatav)
do while (size(metaanddatav) >0)
  print*,"read/write data; count: ",sessionfrom%count
  do i =1,size(metaanddatav)
    print *, "display metaanddatav index: ", i
!    call metaanddatav(i)%display()
!    call sessionto%ingest(metaanddata=metaanddatav(i))
  end do
!  call sessionto%close_message()
  call sessionfrom%extrude(metaanddatav)
end do

! constant data
call sessionfrom%filerewind()

call sessionfrom%set(filter=dbafilter(contextana=.true.))
call sessionfrom%extrude(metaanddatav)
do while (size(metaanddatav) >0)
  print*,"read/write data; count: ",sessionfrom%count
  do i =1,size(metaanddatav)
    print *, "display metaanddatav index: ", i
!    call metaanddatav(i)%display()
!    call sessionto%ingest(metaanddata=metaanddatav(i))
  end do
!  call sessionto%close_message()
  call sessionfrom%extrude(metaanddatav)
end do

end subroutine copy1f


subroutine copy1fmem()
type(dbametaanddata),allocatable :: metaanddatav(:)
integer :: i

print *,"----------------------------------------------"
print *,"--------------- copy1fmem ----------------------"


do while (sessionfrom%messages_read_next())
  print*,"read/write message"
  call sessionfrom%set(filter=dbafilter())
  call sessionfrom%extrude(metaanddatav)
  do i =1,size(metaanddatav)
    call metaanddatav(i)%display()
    call sessionto%ingest(metaanddata=metaanddatav(i))
  end do
  call sessionto%prendilo()

  print *,"contextana"
  call sessionfrom%set(filter=dbafilter(contextana=.true.))
  call sessionfrom%extrude(metaanddatav)
  do i =1,size(metaanddatav)
    call metaanddatav(i)%display()
    call sessionto%ingest(metaanddata=metaanddatav(i))
  end do
  call sessionto%close_message()
  call sessionfrom%remove_all()
end do

end subroutine copy1fmem

subroutine copy1f2mem()

print *,"----------------------------------------------"
print *,"--------------- copy1f2mem ----------------------"

do while (sessionfrom%messages_read_next())
  call sessionfrom%messages_write_next() 
  call sessionfrom%remove_all()
end do

end subroutine copy1f2mem


subroutine copy2()
type(dbasession)    :: sessioncp
type(dbaconnection) :: connectioncp
type(dbametaanddatac):: metaanddatac

print *,"----------------------------------------------"
print *,"--------------- copy2 ----------------------"

! connect to dsn
connectioncp=dbaconnection(dsn="sqlite:/tmp/dballecopy2.sqlite")
sessioncp=dbasession(connectioncp,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

! use character type

! data
call session%set(filter=dbafilter())
call session%extrude_metaanddatac()
do while (c_e(session%count))
  call session%extrude_metaanddatac (metaanddatac)
  call sessioncp%ingest(metaanddatac=metaanddatac)
end do

! constant data
call session%set(filter=dbafilter(contextana=.true.))
call session%extrude_metaanddatac()
do while (c_e(session%count))
  call session%extrude_metaanddatac (metaanddatac)
  call sessioncp%ingest(metaanddatac=metaanddatac)
end do


!close everythings
call sessioncp%delete()
call connectioncp%delete()

end subroutine copy2

end program example_dballe
