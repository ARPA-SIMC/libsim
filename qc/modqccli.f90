module modqccli

use modqc
use vol7d_class
use geo_coord_class
use vol7d_utilities

implicit none

public

type :: qcclitype

  type (vol7d),pointer :: v7d
  type (vol7d) :: clima
  integer,pointer :: data_id_in(:,:,:,:,:)
  integer,pointer :: data_id_out(:,:,:,:,:)

  integer, pointer :: in_macroa(:)

  TYPE(geo_coordvect),POINTER :: macroa(:)
  CHARACTER(len=512) :: filesim

end type qcclitype


interface init
  module procedure qccliinit
end interface

interface alloc
  module procedure qcclialloc
end interface

interface delete
  module procedure qcclidelete
end interface


contains

subroutine qccliinit(qccli,v7d,ier,data_id_in,macropath,climapath)

type(qcclitype),intent(in out) :: qccli
integer ,intent(out) :: ier
integer :: istat,iuni
type (vol7d),intent(in),target:: v7d
integer,intent(in),optional,target:: data_id_in(:,:,:,:,:)
character(len=512),intent(in),optional :: macropath,climapath
character(len=512) :: filepath

ier=0

! riporto il volume dati nel mio oggetto
qccli%v7d => v7d

if (present(data_id_in))then
  qccli%data_id_in => data_id_in
end if

nullify ( qccli%in_macroa )

if (present(macropath))then
  filepath=macropath
else
 filepath=get_package_filepath('polipciv4.dat', filetype_data)
end if

CALL import(qccli%macroa, shpfilesim=filepath)

if (present(climapath))then
  filepath=macropath
else
 filepath=get_package_filepath('climaprec.v7d', filetype_data)
end if


call init(qccli%clima)

iuni=getunit()
open (unit=iuni,file=filepath,form="UNFORMATTED")
call read_from_file (qccli%clima,unit=iuni)
close (unit=iuni)

return
end subroutine qccliinit



subroutine qcclialloc(qccli,ier)
                                ! pseudo costruttore con distruttore automatico

type(qcclitype),intent(in out) :: qccli

integer ,intent(out):: ier
integer :: istat,istatt,nv
integer :: sh(5)

! se ti sei dimenticato di deallocare ci penso io
call  qcclidealloc(qccli,ier)
if ( ier /= 0 )then
  ier=2
  return
end if

istatt=0
ier=0

!!$if (associated (qccli%v7d%dativar%r )) then
!!$  nv=size(qccli%v7d%dativar%r)
!!$
!!$  allocate(qccli%valminr(nv),stat=istat)
!!$  istatt=istatt+istat
!!$  allocate(qccli%valmaxr(nv),stat=istat)
!!$  istatt=istatt+istat
!!$
!!$  if (istatt /= 0) ier=1
!!$
!!$end if

if (associated (qccli%v7d%ana )) then
  allocate (qccli%in_macroa(size(qccli%v7d%ana )),stat=istatt)
  if (istatt /= 0) ier=1
end if

if (associated(qccli%data_id_in))then
  sh=shape(qccli%data_id_in)
  allocate (qccli%data_id_out(sh(1),sh(2),sh(3),sh(4),sh(5)),stat=istatt)
  if (istatt /= 0)then
    ier=1
  else
    qccli%data_id_out=imiss
  end if
end if

return

end subroutine qcclialloc


subroutine qcclidealloc(qccli,ier)
                                ! pseudo distruttore

type(qcclitype),intent(in out) :: qccli
integer,intent(out) :: ier

ier=0

!!$if ( associated ( qccli%valminr)) then
!!$  deallocate(qccli%valminr)
!!$end if
!!$
!!$if ( associated ( qccli%valmaxr)) then
!!$  deallocate(qccli%valmaxr)
!!$end if

if (associated (qccli%in_macroa)) then
  deallocate (qccli%in_macroa)
end if

if (associated(qccli%data_id_out))then
  deallocate (qccli%data_id_out)
end if

return
end subroutine qcclidealloc



subroutine qcclidelete(qccli,ier)
                                ! decostruttore a mezzo
type(qcclitype),intent(in out) :: qccli
integer ,intent(out) :: ier
integer :: istat

ier=0

call qcclidealloc(qccli,ier)

call delete(qccli%clima)

return
end subroutine qcclidelete


SUBROUTINE QuaConCLI (qccli,ier,tbattrin,tbattrout,&
 anamask,timemask,levelmask,timerangemask,varmask,networkmask)

type(qcclitype),intent(in out) :: qccli
integer ,intent(out) :: ier
logical ,intent(in),optional :: anamask(:),timemask(:),levelmask(:),timerangemask(:),varmask(:),networkmask(:)
character (len=10) ,intent(in),optional :: tbattrin,tbattrout
CHARACTER(len=vol7d_ana_lenident) :: ident
REAL(kind=fp_geo) :: lat,lon
integer :: imese
integer :: perc = 80
                                !local
integer :: i,j,indtbattrin,indtbattrout,i1,i2,i3,i4,i5,i6
logical :: anamaskl(size(qccli%v7d%ana)), timemaskl(size(qccli%v7d%time)), levelmaskl(size(qccli%v7d%level)), &
 timerangemaskl(size(qccli%v7d%timerange)), varmaskl(size(qccli%v7d%dativar%r)), networkmaskl(size(qccli%v7d%network)) 

integer :: indana ,indanavar, indtime ,indlevel ,indtimerange ,inddativarr, indnetwork
integer :: indcana,           indctime,indclevel,indctimerange,indcdativarr,indcnetwork
real :: datoqui,climaqui
integer :: altezza,mh,iarea


TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time
TYPE(vol7d_level):: level
type(vol7d_var)  :: anavar


!call qccli_validate (qccli)

if (present(tbattrin))then
  indtbattrin = firsttrue(qccli%v7d%dativarattr%r(:)%btable == tbattrin)
else
  indtbattrin=1
end if

if (present(tbattrout))then
  indtbattrout = firsttrue(qccli%v7d%dativarattr%r(:)%btable == tbattrout)
else
  indtbattrout=2
end if

if (indtbattrin <=0 .or. indtbattrout <= 0 ) then
  ier=5
  return
end if

if(present(anamask)) then
  anamaskl = anamask
else
  anamaskl = .true.
endif
if(present(timemask)) then
  timemaskl = timemask
else
  timemaskl = .true.
endif
if(present(levelmask)) then
  levelmaskl = levelmask
else
  levelmaskl = .true.
endif
if(present(timerangemask)) then
  timerangemaskl = timerangemask
else
  timerangemaskl = .true.
endif
if(present(varmask)) then
  varmaskl = varmask
else
  varmaskl = .true.
endif
if(present(networkmask)) then
  networkmaskl = networkmask
else
  networkmaskl = .true.
endif

qccli%v7d%voldatiattrr(:,:,:,:,:,:,indtbattrout)=bmiss


! valuto in quale macroarea sono le stazioni
qccli%in_macroa = imiss

DO i = 1, SIZE(qccli%v7d%ana)
  DO j = 1, SIZE(qccli%macroa)
    IF (geo_coord_inside(qccli%v7d%ana(i)%coord, qccli%macroa(j))) THEN
      qccli%in_macroa(i) = j
      EXIT
    ENDIF
  ENDDO
ENDDO


do indana=1,size(qccli%v7d%ana)

  ! rielabora le macroarea facendole Valentine thinking

  if (qccli%in_macroa(indana) >= 1 .and. qccli%in_macroa(indana) <= 4 ) iarea=3
  if (qccli%in_macroa(indana) == 5 .or.  qccli%in_macroa(indana) == 6 ) iarea=2
  if (qccli%in_macroa(indana) == 7 .or.  qccli%in_macroa(indana) == 8 ) iarea=1

  do  indnetwork=1,size(qccli%v7d%network)
    do indlevel=1,size(qccli%v7d%level)
      do indtimerange=1,size(qccli%v7d%timerange)
        do inddativarr=1,size(qccli%v7d%dativar%r)

          do indtime=1,size(qccli%v7d%time)
            if (anamaskl(indana).and.timemaskl(indtime).and.levelmaskl(indlevel).and. &
             timerangemaskl(indtimerange).and.varmaskl(inddativarr).and.networkmaskl(indnetwork).and.&
             c_e(qccli%v7d%voldatir(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork)))then
              if( invalidated(qccli%v7d%voldatiattrb&
               (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrin))) cycle
              
              lat=0.0d0
              lon=0.0d0
              write(ident,'("BOX-",2i2.2)')iarea,perc   ! macro-area e percentile
              call init(ana,lat=lat,lon=lon,ident=ident)

              CALL getval(qccli%v7d%time(indtime), month=imese)
              call init(time, year=1001, month=imese, day=1, hour=00,minute=00)
              call init(anavar,"B07001" )
              indanavar        = firsttrue(qccli%v7d%anavar%i  == anavar)

              if (indanavar <=0 )cycle
              altezza= qccli%v7d%volanai(indana,indanavar,indnetwork)
              call macro_height(altezza,mh)
              call init(level, 103,mh,0)

              indcnetwork      = 1
              indcana          = firsttrue(qccli%clima%ana     == ana)
              indctime         = firsttrue(time                              == qccli%clima%time)
              indclevel        = firsttrue(level                             == qccli%clima%level)
              indctimerange    = firsttrue(qccli%v7d%timerange(indtimerange) == qccli%clima%timerange)
              indcdativarr     = firsttrue(qccli%v7d%dativar%r(inddativarr)  == qccli%clima%dativar%r)

              !print *,"dato  ",qccli%v7d%timerange(indtimerange) 
              !print *,"clima ",qccli%clima%timerange
              !print *,indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork
              if (indcana <= 0 .or. indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
               .or. indcnetwork <= 0 ) cycle

              datoqui = qccli%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
              climaqui= qccli%clima%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)

              if (c_e(climaqui).and. c_e(datoqui))then
                !print*,"dato,clima ",datoqui,climaqui
                if ( datoqui > climaqui) then
                  
                  qccli%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=100-perc

                else
                  
                  qccli%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=perc
                  
                end if
 
                if ( associated ( qccli%data_id_in)) then
                  qccli%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)=&
                   qccli%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)
                end if
                
              end if
            end if
          end do
        end do
      end do
    end do
  end do
end do


ier=0
return

end subroutine QUACONCLI


subroutine macro_height(altezza,mh)

integer :: altezza,mh
integer :: livello(10) = (/50,175,375,625,875,1125,1375,1625,1875,2125/)

if (c_e(altezza))then
  mh=livello(min(max((altezza+150)/250,1),10))
else
  mh=imiss
end if

end subroutine macro_height


!!$subroutine qccli_validate(qccli)
!!$type(qcclitype),intent(in) :: qccli
!!$
!!$!todo da validare
!!$
!!$return
!!$end subroutine qccli_validate

end module modqccli

