! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

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

! derived from a work of:
!!$CC**********************************************************************CC
!!$CC**********************************************************************CC
!!$CC									CC
!!$CC		SERVIZIO METEOROLOGICO REGIONE EMILA ROMAGNA		CC
!!$CC				E.R.S.A.				CC
!!$CC									CC
!!$CC									CC
!!$CC	PAOLO PATRUNO			   PIER PAOLO ALBERONI		CC
!!$CC									CC
!!$CC	BOLOGNA 1992							CC
!!$CC**********************************************************************CC
!!$CC**********************************************************************CC

#include "config.h"

!>\brief Controllo di qualità temporale.
!! Questo modulo effettua un controllo temporale dei dati
!!
!!\ingroup qc
!! Esegue un test temporale sui dati.
!! 
!! L'asse dei tempi che viene espresso in minuti e` contenuto in IMINUTI.
!! Per effettuare il controllo i dati non devono essere distanziati nel tempo
!! piu` di INTMAX nel qual caso la serie viene spezzata in serie piu` piccole.
!! Gli estremi delle serie non potranno essere controllati con il test dei 
!! massimi e minimi
!!
!! 1) test di variazione assoluta nel tempo
!! Fa un controllo sui dati in maniera temporale controllando che la variazione
!! assoluta tra due dati non superi RJUMP. Nel caso vengono settati tutti e due i
!! dati errati e ricomincia col controllare il dato successivo. Se il test
!! viene superato la flag non viene alterata altrimenti la flag viene 
!! incrementata di una unita`
!! 
!! 2) test massimi e minimi	
!! Solo se viene superato il primo test viene controllato che il dato considerato
!! non sia un massimo o un minimo e che contemporaneamente non differisca dai
!! valori intorno piu` di GRADMAX  per minuto. Se il test
!! viene superato la flag viene decrementata altrimenti la flag viene 
!! incrementata di una unita`. Gli estremi delle serie non potranno essere 
!! controllati con il test dei massimi e minimi e la flag non verra` alterata.
!> \todo ottimizzare la lettura degli extreme nel caso il periodo da controllare sia a cavallo di due anni.
!!\todo Bisognerebbe validare il volume sottoposto al controllo per vedere se ha i requisiti.
!!
!! Programma Esempio del controllo spaziale :
!! \include  v7d_qctem.f90


module modqctem

use log4fortran
use char_utilities
use file_utilities
use datetime_class
use vol7d_class
use modqc
use modqccli

!use array_utilities
!use io_units
#ifdef HAVE_DBALLE
use vol7d_dballe_class
#endif

implicit none


character (len=255),parameter:: subcategorytem="QCtem"

integer, parameter :: tem_nvar=1
CHARACTER(len=10) :: tem_btable(tem_nvar)=(/"B12101"/) !< variable wmo code table for normalization.
!> standard coefficients for orizontal gradient normalization
real, parameter :: tem_a(tem_nvar) = (/1.e5/)
!> standard coefficients for orizontal gradient normalization
real, parameter :: tem_b(tem_nvar) = (/250./)

!>\brief Oggetto principale per il controllo di qualità
type :: qctemtype
  type (vol7d),pointer :: v7d => null() !< Volume dati da controllare
  integer,pointer :: data_id_in(:,:,:,:,:) => null()  !< Indici dati del DB in input
  integer,pointer :: data_id_out(:,:,:,:,:) => null() !< Indici dati del DB in output
  integer :: category !< log4fortran
  type (qcclitype) :: qccli !< qccli part for normalization
  type (vol7d) :: clima !< Clima spaziale di tutte le variabili da controllare
  character(len=20):: operation !< Operation to execute ("gradient"/"run")
  integer :: timeconfidence !< max time for data correlation in sec
end type qctemtype


!>\brief Inizializzazione
interface init
  module procedure qcteminit
end interface

!>\brief  Allocazione di memoria
interface alloc
  module procedure qctemalloc
end interface

!>\brief Cancellazione
interface delete
  module procedure qctemdelete
end interface

PRIVATE
PUBLIC tem_nvar, tem_btable, tem_a, tem_b, qctemtype, init, alloc, delete, &
 quacontem


contains

!>\brief Init del controllo di qualità temporale.
!!Effettua la lettura dei file e altre operazioni di inizializzazione.

subroutine qcteminit(qctem,v7d,var, timei, timef, coordmin, coordmax, data_id_in,extremepath,temporalpath,&
#ifdef HAVE_DBALLE
 dsne,usere,passworde,&
 dsntem,usertem,passwordtem,&
#endif
 height2level,operation,timeconfidence,categoryappend)

type(qctemtype),intent(in out) :: qctem !< Oggetto per il controllo temporale
type (vol7d),intent(in),target:: v7d !< Il volume Vol7d da controllare
character(len=*),INTENT(in) :: var(:)!< variabili da importare secondo la tabella B locale o relativi alias
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'importazione
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
!>estremi temporali (inizio e fine) dell'estrazione per l'importazione
TYPE(datetime),INTENT(in),optional :: timei, timef
integer,intent(in),optional,target:: data_id_in(:,:,:,:,:) !< Indici dei dati in DB
character(len=*),intent(in),optional :: extremepath !< file con il volume del extreme
character(len=*),intent(in),optional :: temporalpath !< file with spatial ndi
logical ,intent(in),optional :: height2level   !< use conventional level starting from station height
character(len=*), optional :: operation !< Operation to execute ("gradient"/"run")
integer,intent(in),optional :: timeconfidence !< max time for data correlation in sec
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< aggiunge questo suffisso al namespace category di log4fortran

#ifdef HAVE_DBALLE
type (vol7d_dballe) :: v7d_dballetmp
character(len=*),intent(in),optional :: dsne
character(len=*),intent(in),optional :: usere
character(len=*),intent(in),optional :: passworde
character(len=*),intent(in),optional :: dsntem
character(len=*),intent(in),optional :: usertem
character(len=*),intent(in),optional :: passwordtem
character(len=512) :: ldsntem
character(len=512) :: lusertem
character(len=512) :: lpasswordtem
#endif

type (vol7d) :: v7dtmp
TYPE(vol7d_network):: network 
integer :: iuni,i
character(len=512) :: filepathtem
character(len=512) :: a_name
character(len=9) ::netname(2)=(/"qctemgndi","qctemsndi"/)


call l4f_launcher(a_name,a_name_append=trim(subcategorytem)//"."//trim(categoryappend))
qctem%category=l4f_category_get(a_name)

nullify ( qctem%data_id_in )
nullify ( qctem%data_id_out )

! riporto il volume dati nel mio oggetto
qctem%v7d => v7d

qctem%operation=optio_c(operation,20)
filepathtem=optio_c(temporalpath,512)

!check options
if (qctem%operation  /= "gradient" .and. qctem%operation  /= "run") then
  call l4f_category_log(qctem%category,L4F_ERROR,"operation is wrong: "//qctem%operation)
  call raise_error()
end if

!inglobe id
if (present(data_id_in))then
  qctem%data_id_in => data_id_in
end if

qctem%timeconfidence = optio_i(timeconfidence)

! load extreme
call init(qctem%qccli,v7d,var, timei, timef, data_id_in,&
 macropath=cmiss, climapath=cmiss, extremepath=extremepath, &
#ifdef HAVE_DBALLE
 dsncli=cmiss,dsnextreme=dsne,user=usere,password=passworde,&
#endif
 height2level=height2level,categoryappend=categoryappend)


! now load temporal clima 

do i=1,size(netname)

  if (qctem%operation == "run") then
    call init(network,netname(i))

#ifdef HAVE_DBALLE
    call optio(dsntem,ldsntem)
    call optio(usertem,lusertem)
    call optio(passwordtem,lpasswordtem)

    if (c_e(filepathtem) .and. (c_e(ldsntem).or.c_e(lusertem).or.c_e(lpasswordtem))) then
      call l4f_category_log(qctem%category,L4F_ERROR,"filepath  defined together with dba options")
      call raise_error()
    end if

    if (.not. c_e(ldsntem)) then

#endif

      if (.not. c_e(filepathtem)) then
        filepathtem=get_package_filepath(netname(i)//'.v7d', filetype_data)
      end if
      
      if (c_e(filepathtem))then

        select case (trim(lowercase(suffixname(filepathtem))))

        case("v7d")
          iuni=getunit()
          call import(v7dtmp,filename=filepathtem,unit=iuni)
          close (unit=iuni)
          
#ifdef HAVE_DBALLE
        case("bufr")
          call init(v7d_dballetmp,file=.true.,filename=filepathtem,categoryappend=trim(a_name)//".clima")
          call import(v7d_dballetmp,var=var, &
           varkind=(/("r",i=1,size(var))/),attr=(/"*B33209"/),attrkind=(/"b"/),network=network)
          call copy(v7d_dballetmp%vol7d,v7dtmp)
          call delete(v7d_dballetmp)
#endif
          
        case default
          call l4f_category_log(qctem%category,L4F_ERROR,&
           "file type not supported (use .v7d or .bufr suffix only): "//trim(filepathtem))
          call raise_error()
        end select
        
      else
        call l4f_category_log(qctem%category,L4F_WARN,"spatial clima volume not iniziatized: spatial QC will not be possible")
        call init(qctem%clima)
        call raise_fatal_error()
      end if
      
#ifdef HAVE_DBALLE
    else
    
      call l4f_category_log(qctem%category,L4F_DEBUG,"init v7d_dballetem")
      call init(v7d_dballetmp,dsn=ldsntem,user=lusertem,password=lpasswordtem,write=.false.,&
       file=.false.,categoryappend=trim(a_name)//".tem")
      call l4f_category_log(qctem%category,L4F_DEBUG,"import v7d_dballetmp")
      call import(v7d_dballetmp,var=var, &
       varkind=(/("r",i=1,size(var))/),attr=(/"*B33209"/),attrkind=(/"b"/),network=network)

      call copy(v7d_dballetmp%vol7d,v7dtmp)

      call delete(v7d_dballetmp)
      
    end if
#endif
    call vol7d_merge(qctem%clima,v7dtmp)

  end if

end do

return
end subroutine qcteminit



!>\brief Allocazioni di memoria
subroutine qctemalloc(qctem)
                                ! pseudo costruttore con distruttore automatico

type(qctemtype),intent(in out) :: qctem !< Oggetto per il controllo climatico

integer :: istatt
integer :: sh(5)

! se ti sei dimenticato di deallocare ci penso io
call  qctemdealloc(qctem)

if (associated(qctem%data_id_in))then
  sh=shape(qctem%data_id_in)
  allocate (qctem%data_id_out(sh(1),sh(2),sh(3),sh(4),sh(5)),stat=istatt)
  if (istatt /= 0)then
    call l4f_category_log(qctem%category,L4F_ERROR,"allocate error")
    call raise_error("allocate error")
  else
    qctem%data_id_out=imiss
  end if
end if

end subroutine qctemalloc


!>\brief Deallocazione della memoria
subroutine qctemdealloc(qctem)
                                ! pseudo distruttore

type(qctemtype),intent(in out) :: qctem !< Oggetto per il controllo temporale

if (associated(qctem%data_id_out))  deallocate (qctem%data_id_out)

if (associated(qctem%data_id_out)) then
  deallocate (qctem%data_id_out)
  nullify (qctem%data_id_out)
end if

end subroutine qctemdealloc


!>\brief Cancellazione


subroutine qctemdelete(qctem)
                                ! decostruttore a mezzo
type(qctemtype),intent(in out) :: qctem !< Oggetto per il controllo temporale

call qctemdealloc(qctem)

call delete(qctem%qccli)

!delete logger
call l4f_category_delete(qctem%category)

return
end subroutine qctemdelete


!>\brief Controllo di Qualità temporale.
!!Questo è il vero e proprio controllo di qualità temporale.

SUBROUTINE quacontem (qctem,battrinv,battrcli,battrout,&
 anamask,timemask,levelmask,timerangemask,varmask,networkmask)


type(qctemtype),intent(in out) :: qctem !< Oggetto per il controllo di qualità
character (len=10) ,intent(in),optional :: battrinv !< attributo invalidated in input
character (len=10) ,intent(in),optional :: battrcli !< attributo con la confidenza climatologica in input
character (len=10) ,intent(in),optional :: battrout !< attributo con la confidenza temporale in output
logical ,intent(in),optional :: anamask(:) !< Filtro sulle anagrafiche
logical ,intent(in),optional :: timemask(:) !< Filtro sul tempo
logical ,intent(in),optional :: levelmask(:) !< Filtro sui livelli
logical ,intent(in),optional :: timerangemask(:) !< filtro sui timerange
logical ,intent(in),optional :: varmask(:) !< Filtro sulle variabili
logical ,intent(in),optional :: networkmask(:) !< Filtro sui network

                                !REAL(kind=fp_geo) :: lat,lon
integer :: asec
                                !local
integer :: indbattrinv,indbattrcli,indbattrout,grunit
logical :: anamaskl(size(qctem%v7d%ana)), timemaskl(size(qctem%v7d%time)), levelmaskl(size(qctem%v7d%level)), &
 timerangemaskl(size(qctem%v7d%timerange)), varmaskl(size(qctem%v7d%dativar%r)), networkmaskl(size(qctem%v7d%network)) 

integer :: indana ,  indtime ,indlevel ,indtimerange ,inddativarr, indnetwork,indtimenear
integer :: indcana ,  indctime ,indclevel ,indctimerange ,indcdativarr, indcnetwork, indcnetworks, indcnetworkg
real :: datoqui,datoprima,datodopo,climaquii, climaquif
                                !integer, allocatable :: indcanav(:)

                                !TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time,prima, ora, dopo
TYPE(vol7d_network):: network
type(timedelta) :: td

double precision :: gradprima,graddopo,grad
                                !call qctem_validate (qctem)
character(len=512) :: filename
logical :: exist
integer :: ind

!localize optional parameter
if (present(battrinv))then
  indbattrinv = index_c(qctem%v7d%datiattr%b(:)%btable, battrinv)
else
  indbattrinv = index_c(qctem%v7d%datiattr%b(:)%btable, qcattrvarsbtables(1))
end if

if (present(battrcli))then
  indbattrcli = index_c(qctem%v7d%datiattr%b(:)%btable, battrcli)
else
  indbattrcli = index_c(qctem%v7d%datiattr%b(:)%btable, qcattrvarsbtables(2))
end if

if (present(battrout))then
  indbattrout = index_c(qctem%v7d%datiattr%b(:)%btable, battrout)
else
  indbattrout = index_c(qctem%v7d%datiattr%b(:)%btable, qcattrvarsbtables(3))
end if


! some checks on input
!if (indbattrinv <=0 .or. indbattrcli <= 0 .or. indbattrout <= 0 ) then
if (indbattrout <= 0 ) then

  call l4f_category_log(qctem%category,L4F_ERROR,"error finding attribute index for output")
  call raise_error()

end if

if (qctem%operation == "gradient") then

                                !check for gradient operation
  if ( size(qctem%v7d%level)      > 1 .or.&
   size(qctem%v7d%timerange)  > 1 .or.&
   size(qctem%v7d%dativar%r)  > 1 ) then
    call l4f_category_log(qctem%category,L4F_ERROR,"gradient operation manage one level/timerange/var only")
    call raise_error()
  end if

                                !check for data to check
  if ( size(qctem%v7d%time)      < 1 ) then
    call l4f_category_log(qctem%category,L4F_INFO,"no data present for gradient operation")
    return
  end if
end if

                                ! set other local variable from optional parameter
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

! do not touch data that will not in/validated by QC
qctem%v7d%voldatiattrb(:,:,:,:,:,:,indbattrout)=ibmiss

! normalize data in space and time
call vol7d_normalize_data(qctem%qccli)


! compute some index for temporal clima
!! compute the conventional generic datetime
!!cyclicdt = cyclicdatetime_new(chardate="/////////") !TMMGGhhmm
time=cyclicdatetime_to_conventional(cyclicdatetime_new(chardate="/////////"))  !TMMGGhhmm
!!call init(time, year=1007, month=1, day=1, hour=01, minute=01)


if (qctem%operation == "run") then
  !!indcana          = firsttrue(qctem%clima%ana     == ana)
  call init(network,"qctemsndi")
  indcnetworks      = index(qctem%clima%network               , network)
  call init(network,"qctemgndi")
  indcnetworkg      = index(qctem%clima%network               , network)
  indctime         = index(qctem%clima%time                  ,  time)
end if

do indana=1,size(qctem%v7d%ana)
  if (.not.anamaskl(indana)) cycle
  call l4f_category_log(qctem%category,L4F_INFO,&
   "Check ana:"//to_char(qctem%v7d%ana(indana)) )

  do indnetwork=1,size(qctem%v7d%network)
    do indlevel=1,size(qctem%v7d%level)
      do indtimerange=1,size(qctem%v7d%timerange)
        do inddativarr=1,size(qctem%v7d%dativar%r)
          ind=index_c(tem_btable,qctem%v7d%dativar%r(inddativarr)%btable)

          if (qctem%operation == "gradient") then
                                ! open file to write gradient

                                !t2c(getilon(qctem%v7d%ana(indana)%coord))
                                !t2c(getilat(qctem%v7d%ana(indana)%coord))

            filename=trim(to_char(qctem%v7d%level(indlevel)))//&
             "_"//trim(to_char(qctem%v7d%timerange(indtimerange)))//&
             "_"//trim(qctem%v7d%dativar%r(inddativarr)%btable)//&
             ".grad"
            
            call l4f_category_log(qctem%category,L4F_INFO,"try to open gradient file; filename below")
            call l4f_category_log(qctem%category,L4F_INFO,filename)
            
            inquire(file=filename, exist=exist)
            
            grunit=getunit()
            if (grunit /= -1) then
                                !open (unit=grunit, file=t2c(timei)//"_"//t2c(timef)//".grad",STATUS='UNKNOWN', form='FORMATTED')
              open (grunit, file=filename ,STATUS='UNKNOWN', form='FORMATTED',position='APPEND')
            end if
                                ! say we have to write header in file
            if  (.not. exist) then
              call l4f_category_log(qctem%category,L4F_INFO,"write header in gradient file")
              write (grunit,*) &
               qctem%v7d%level(indlevel), &
               qctem%v7d%timerange(indtimerange), &
               qctem%v7d%dativar%r(inddativarr)
            end if
          end if

!!$            call l4f_log(L4F_INFO,"Index:"// t2c(indana)//t2c(indnetwork)//t2c(indlevel)//&
!!$             t2c(indtimerange)//t2c(inddativarr)//t2c(indtime))


          do indtime=2,size(qctem%v7d%time)-1

            if (.not.timemaskl(indtime).or. .not. levelmaskl(indlevel).or. &
             .not. timerangemaskl(indtimerange) .or. .not. varmaskl(inddativarr) .or. .not. networkmaskl(indnetwork)) cycle
            
            
            datoqui = qctem%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
            if (.not. c_e(datoqui)) cycle
            ora = qctem%v7d%time  (indtime)

                                ! invalidated
            if (indbattrinv > 0) then
              if( invalidated(qctem%v7d%voldatiattrb&
               (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) then
                call l4f_category_log(qctem%category,L4F_WARN,&
                 "It's better to do a reform on ana to v7d after peeling, before spatial QC")
                cycle
              end if
            end if

                                ! gross error check
            if (indbattrcli > 0) then
              if( .not. vdge(qctem%v7d%voldatiattrb&
               (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrcli))) then
                call l4f_category_log(qctem%category,L4F_WARN,&
                 "It's better to do a reform on ana to v7d after peeling, before spatial QC")
                cycle
              end if
            end if



            if (qctem%operation == "run") then

              indclevel        = index(qctem%clima%level                 ,  qctem%v7d%level(indlevel))
              indctimerange    = index(qctem%clima%timerange             ,  qctem%v7d%timerange(indtimerange))

                                ! attenzione attenzione TODO
                                ! se leggo da bufr il default è char e non reale
              indcdativarr     = index(qctem%clima%dativar%r, qctem%v7d%dativar%r(inddativarr))

#ifdef DEBUG
              call l4f_log(L4F_DEBUG,"QCtem Index:"// to_char(indctime)//to_char(indclevel)//&
               to_char(indctimerange)//to_char(indcdativarr)//to_char(indcnetworks))
#endif
              if ( indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
               .or. indcnetworks <= 0 ) cycle
            end if
            
!!$            nintime=qctem%v7d%time(indtime)+timedelta_new(minute=30)
!!$            CALL getval(nintime, month=mese, hour=ora)
!!$            call init(time, year=1001, month=mese, day=1, hour=ora, minute=00)
!!$ 

                                !find the nearest data in time before
            indtimenear=indtime-1
            datoprima = qctem%v7d%voldatir  (indana ,indtimenear ,indlevel ,indtimerange ,inddativarr, indnetwork )
            prima = qctem%v7d%time  (indtimenear)
            
                                ! invalidated
            if (indbattrinv > 0) then
              if( invalidated(qctem%v7d%voldatiattrb&
               (indana,indtimenear,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) then
                datoprima=rmiss
              end if
            end if
            
                                ! gross error check
            if (indbattrcli > 0) then
              if( .not. vdge(qctem%v7d%voldatiattrb&
               (indana,indtimenear,indlevel,indtimerange,inddativarr,indnetwork,indbattrcli))) then
                datoprima=rmiss
              end if
            end if


                                !find the nearest data in time after
            indtimenear=indtime+1
            datodopo = qctem%v7d%voldatir  (indana ,indtimenear ,indlevel ,indtimerange ,inddativarr, indnetwork )
            dopo = qctem%v7d%time  (indtimenear)

                                ! invalidated
            if (indbattrinv > 0) then
              if( invalidated(qctem%v7d%voldatiattrb&
               (indana,indtimenear,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) then
                datodopo=rmiss
              end if
            end if
            
                                ! gross error check
            if (indbattrcli > 0) then
              if( .not. vdge(qctem%v7d%voldatiattrb&
               (indana,indtimenear,indlevel,indtimerange,inddativarr,indnetwork,indbattrcli))) then
                datodopo=rmiss
              end if
            end if


            IF(.NOT.C_E(datoprima) .and. .NOT.C_E(datodopo) ) cycle
            
            gradprima=rmiss
            graddopo=rmiss
            grad=rmiss

                                !compute time gradient only inside timeconfidence
            td=ora-prima
            call getval(td,asec=asec)
            if ((c_e(qctem%timeconfidence) .and. asec <= qctem%timeconfidence) .or. &
             .not. c_e(qctem%timeconfidence)) then
              if (c_e(datoprima)) gradprima=(datoqui-datoprima) / dble(asec)
            end if
              
            td=dopo-ora
            call getval(td,asec=asec)
            if ((c_e(qctem%timeconfidence) .and. asec <= qctem%timeconfidence) .or. &
              .not. c_e(qctem%timeconfidence)) then
              if (c_e(datodopo))  graddopo =(datodopo-datoqui ) / dble(asec)
            end if


#ifdef DEBUG
            call l4f_log(L4F_DEBUG,"QCtem gradprima:"// to_char(gradprima)//" graddopo:"//to_char(graddopo))
#endif
                                ! we need some gradient
            IF(.NOT.C_E(gradprima) .and. .NOT.C_E(graddopo) ) cycle


                                ! for gap we set negative gradient
                                ! for spike positive gradinet
            IF(.NOT.C_E(gradprima) ) then

              ! set gap for other one
              grad= sign(abs(graddopo),-1.d0)

            else IF(.NOT.C_E(graddopo) ) then

              ! set gap for other one
              grad= sign(abs(gradprima),-1.d0)

            else

              if (abs(max(abs(gradprima),abs(graddopo))-min(abs(gradprima),abs(graddopo))) < &
               max(abs(gradprima),abs(graddopo))/2. .and. (sign(1.d0,gradprima)*sign(1.d0,graddopo)) < 0.) then
                                ! spike
                grad= min(abs(gradprima),abs(graddopo))
              else
                                ! gap
                grad= sign(max(abs(gradprima),abs(graddopo)),-1.d0)
              end if
            end IF

            if (qctem%operation == "gradient") then
              write(grunit,*)grad
            end if

                                !ATTENZIONE TODO : inddativarr È UNA GRANDE SEMPLIFICAZIONE NON VERA SE TIPI DI DATO DIVERSI !!!!
            if (qctem%operation == "run") then

                                ! choice which network we have to use 
              if (grad >= 0) then
#ifdef DEBUG
                call l4f_log(L4F_DEBUG,"QCtem choice gradient type: spike")
#endif
                indcnetwork=indcnetworks
              else
#ifdef DEBUG
                call l4f_log(L4F_DEBUG,"QCtem choice gradient type: gradmax")
#endif
                indcnetwork=indcnetworkg
              end if

              grad=abs(grad)
              call l4f_log(L4F_DEBUG,"gradiente da confrontare con QCtem clima:"//t2c(grad))

              do indcana=1,size(qctem%clima%ana)

                climaquii=(qctem%clima%voldatir(indcana &
                ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)&
                 -tem_b(ind))/tem_a(ind)   ! denormalize
                 
                climaquif=(qctem%clima%voldatir(min(indcana+1,size(qctem%clima%ana)) &
                ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)&
                -tem_b(ind))/tem_a(ind)    ! denormalize

#ifdef DEBUG
                call l4f_log(L4F_DEBUG,"QCtem clima start:"//t2c(climaquii))
                call l4f_log(L4F_DEBUG,"QCtem clima   end:"//t2c(climaquif))
#endif
                if ( c_e(climaquii) .and. c_e(climaquif )) then

                  if ( (grad >= climaquii .and. grad < climaquif) .or. &
                   (indcana == 1 .and. grad < climaquii) .or. &
                   (indcana == size(qctem%clima%ana) .and. grad >= climaquif) ) then

#ifdef DEBUG
                call l4f_log(L4F_DEBUG,"QCtem confidence:"// t2c(qctem%clima%voldatiattrb&
                 (indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1)))
#endif

                    qctem%v7d%voldatiattrb(   indana, indtime, indlevel, indtimerange, inddativarr, indnetwork, indbattrout)=&
                     qctem%clima%voldatiattrb(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1          )

                    if ( associated ( qctem%data_id_in)) then
#ifdef DEBUG
                      call l4f_log (L4F_DEBUG,"id: "//t2c(&
                       qctem%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)))
#endif
                      qctem%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)=&
                       qctem%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)
                    end if
                  end if
                end if
              end do
            end if
          end do
        end do
      end do
    end do
  end do

  if (qctem%operation == "gradient") then
    close (unit=grunit)
  end if

end do

!!$print*,"risultato"
!!$print *,qcspa%v7d%voldatiattrb(:,:,:,:,:,:,indbattrout)
!!$print*,"fine risultato"

return

end subroutine quacontem


end module modqctem


!> \example v7d_qctem.F90
!! Sample program for module qctem
