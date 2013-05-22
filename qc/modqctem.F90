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

#include "config.h"

!>\brief Controllo di qualità stemporale.
!! Questo modulo effettua un controllo temporale dei dati
!!
!!\ingroup qc

!> \todo ottimizzare la lettura degli extreme nel caso il periodo da controllare sia a cavallo di due anni.
!!\todo Bisognerebbe validare il volume sottoposto al controllo per vedere se ha i requisiti.
!!
!! Programma Esempio del controllo spaziale :
!! \include  v7d_qctem.f90


module modqctem

use vol7d_class
use modqc
use modqccli
use file_utilities
use log4fortran
use char_utilities
use datetime_class
!use array_utilities
!use io_units
#ifdef HAVE_DBALLE
use vol7d_dballe_class
#endif

implicit none

public

character (len=255),parameter:: subcategorytem="QCtem"

!>\brief Oggetto principale per il controllo di qualità
type :: qctemtype
  type (vol7d),pointer :: v7d => null() !< Volume dati da controllare
  integer,pointer :: data_id_in(:,:,:,:,:) => null()  !< Indici dati del DB in input
  integer,pointer :: data_id_out(:,:,:,:,:) => null() !< Indici dati del DB in output
  integer :: category !< log4fortran
  type (qcclitype) :: qccli !< qccli part for normalization
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


contains

!>\brief Init del controllo di qualità temporale.
!!Effettua la lettura dei file e altre operazioni di inizializzazione.

subroutine qcteminit(qctem,v7d,var, timei, timef, coordmin, coordmax, data_id_in,extremepath,&
#ifdef HAVE_DBALLE
 dsne,usere,passworde,&
#endif
 height2level,categoryappend)

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
logical ,intent(in),optional :: height2level   !< use conventional level starting from station height
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

#ifdef HAVE_DBALLE
type (vol7d_dballe) :: v7d_dballetmp
character(len=*),intent(in),optional :: dsne
character(len=*),intent(in),optional :: usere
character(len=*),intent(in),optional :: passworde
character(len=512) :: ldsn
character(len=512) :: luser
character(len=512) :: lpassword
TYPE(datetime) :: ltimei, ltimef
integer :: yeari, yearf, monthi, monthf, dayi, dayf,&
 houri, minutei, mseci, hourf, minutef, msecf
#endif
 
integer :: iuni,i
character(len=512) :: filepath
character(len=512) :: a_name


call l4f_launcher(a_name,a_name_append=trim(subcategorytem)//"."//trim(categoryappend))
qctem%category=l4f_category_get(a_name)

nullify ( qctem%data_id_in )
nullify ( qctem%data_id_out )

! riporto il volume dati nel mio oggetto
qctem%v7d => v7d

if (present(data_id_in))then
  qctem%data_id_in => data_id_in
end if

call qccliinit(qctem%qccli,v7d,var, timei, timef, data_id_in,&
 macropath=cmiss, climapath=cmiss, extremepath=extremepath, &
#ifdef HAVE_DBALLE
 dsncli=cmiss,dsnextreme=dsne,user=usere,password=passworde,&
#endif
 height2level=height2level,categoryappend=categoryappend)

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
integer :: mese, ora
                                !local
integer :: indbattrinv,indbattrcli,indbattrout
logical :: anamaskl(size(qctem%v7d%ana)), timemaskl(size(qctem%v7d%time)), levelmaskl(size(qctem%v7d%level)), &
 timerangemaskl(size(qctem%v7d%timerange)), varmaskl(size(qctem%v7d%dativar%r)), networkmaskl(size(qctem%v7d%network)) 

integer :: indana ,  indtime ,indlevel ,indtimerange ,inddativarr, indnetwork
real :: datoqui,datola,datila(size(qctem%v7d%time))
integer :: iarea
                                !integer, allocatable :: indcanav(:)

                                !TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time, nintime
TYPE(vol7d_level):: level
type(timedelta) :: deltato,deltat 

integer :: ivert(50),i,ipos,ineg,it,itrov,iv,ivb,kk,iindtime
double precision :: distmin=1000.d0,distscol=300000.d0
double precision :: dist,grad,gradmin
integer (kind=int_b) :: flag

                                !call qctem_validate (qctem)

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
  indbattrout = index_c(qctem%v7d%datiattr%b(:)%btable, qcattrvarsbtables(4))
end if

!if (indbattrinv <=0 .or. indbattrcli <= 0 .or. indbattrout <= 0 ) then
if (indbattrout <= 0 ) then

  call l4f_category_log(qctem%category,L4F_ERROR,"error finding attribute index for output")
  call raise_error()

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

qctem%v7d%voldatiattrb(:,:,:,:,:,:,indbattrout)=ibmiss

!! TODO devo chiamare questa ma non ho ancor l'oggetto giusto
call vol7d_normalize_data(qctem%qccli)

do indana=1,size(qctem%v7d%ana)
  do indnetwork=1,size(qctem%v7d%network)
    do indlevel=1,size(qctem%v7d%level)
      do indtimerange=1,size(qctem%v7d%timerange)
        do inddativarr=1,size(qctem%v7d%dativar%r)
          do indtime=1,size(qctem%v7d%time)
            
            if (anamaskl(indana).and.timemaskl(indtime).and.levelmaskl(indlevel).and. &
             timerangemaskl(indtimerange).and.varmaskl(inddativarr).and.networkmaskl(indnetwork).and.&
             c_e(qctem%v7d%voldatir(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork))) cycle
            
            
            datoqui = qctem%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
            
            if (.not. c_e(datoqui)) cycle
            
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
            
            nintime=qctem%v7d%time(indtime)+timedelta_new(minute=30)
            CALL getval(nintime, month=mese, hour=ora)
            call init(time, year=1001, month=mese, day=1, hour=ora, minute=00)
            
            
            level=qctem%v7d%level(indlevel)

                                !find the nearest data in time
            datola = qctem%v7d%voldatir  (ivert(i) ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
            
                                ! invalidated
            if (indbattrinv > 0) then
              if( invalidated(qctem%v7d%voldatiattrb&
               (ivert(i),indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) then
                datola=rmiss
              end if
            end if
            
                                ! gross error check
            if (indbattrcli > 0) then
              if( .not. vdge(qctem%v7d%voldatiattrb&
               (ivert(i),indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrcli))) then
                datola=rmiss
              end if
            end if
            
            datila = qctem%v7d%voldatir  (ivert(i) ,: ,indlevel ,indtimerange ,inddativarr, indnetwork )

            if (.not. c_e(datola))then
              deltato=timedelta_miss
              do iindtime=1,size(qctem%v7d%time)
                if (.not. c_e(datila(iindtime))) cycle
                                ! invalidated
                if (indbattrinv > 0 .and. &
                 invalidated(qctem%v7d%voldatiattrb&
                 (ivert(i),iindtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) cycle
                                ! gross error check
                if (indbattrcli > 0 .and. &
                 .not. vdge(qctem%v7d%voldatiattrb&
                 (ivert(i),iindtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrcli))) cycle
                
                if (iindtime < indtime) then
                  deltat=qctem%v7d%time(indtime)-qctem%v7d%time(iindtime)
                else if (iindtime > indtime) then
                  deltat=qctem%v7d%time(iindtime)-qctem%v7d%time(indtime)
                else
                  call l4f_category_log(qctem%category,L4F_WARN,"somethings go wrong on ipotesys make in spatial QC")
                end if
                
                if (deltat < deltato) then
                  datola = datila(iindtime)
                  deltato = deltat
                end if
              end do
            end if
              
            IF(.NOT.C_E(datola)) cycle

            
            IF(IVB < 3) cycle      ! do nothing if valid gradients < 3
          
            IF (ipos == ivb .or. ineg == ivb)THEN  ! se tutti i gradienti sono dello stesso segno
              write(11,*)sign(gradmin,dble(ipos-ineg))
              FLAG=50_int_b
            ELSE
              FLAG=100_int_b
            END IF
            
            
            qctem%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrout)=flag
            
            if ( associated ( qctem%data_id_in)) then
#ifdef DEBUG
              call l4f_log (L4F_DEBUG,"id: "//t2c(&
               qctem%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)))
#endif
              qctem%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)=&
               qctem%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)
            end if
          end do
        end do
      end do
    end do
  end do
end do

return

end subroutine quacontem


end module modqctem


!> \example v7d_qctem.F90
!! Sample program for module qctem

