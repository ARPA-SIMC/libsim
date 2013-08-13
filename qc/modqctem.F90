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

!>\brief Controllo di qualit� temporale.
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

integer, parameter :: tem_nvar=1
CHARACTER(len=10) :: tem_btable(tem_nvar)=(/"B12101"/) !< variable wmo code table for normalization.
!> standard coefficients for orizontal gradient normalization
real, parameter :: tem_a(tem_nvar) = (/1.e6/)
!> standard coefficients for orizontal gradient normalization
real, parameter :: tem_b(tem_nvar) = (/273.15/)

!>\brief Oggetto principale per il controllo di qualit�
type :: qctemtype
  type (vol7d),pointer :: v7d => null() !< Volume dati da controllare
  integer,pointer :: data_id_in(:,:,:,:,:) => null()  !< Indici dati del DB in input
  integer,pointer :: data_id_out(:,:,:,:,:) => null() !< Indici dati del DB in output
  integer :: category !< log4fortran
  type (qcclitype) :: qccli !< qccli part for normalization
  type (vol7d) :: clima !< Clima spaziale di tutte le variabili da controllare
  character(len=20):: operation !< Operation to execute ("gradient"/"run")
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

!>\brief Init del controllo di qualit� temporale.
!!Effettua la lettura dei file e altre operazioni di inizializzazione.

subroutine qcteminit(qctem,v7d,var, timei, timef, coordmin, coordmax, data_id_in,extremepath,&
#ifdef HAVE_DBALLE
 dsne,usere,passworde,&
 dsntem,usertem,passwordtem,&
#endif
 height2level,operation,categoryappend)

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
character(len=*), optional :: operation !< Operation to execute ("gradient"/"run")

#ifdef HAVE_DBALLE
type (vol7d_dballe) :: v7d_dballetmp
character(len=*),intent(in),optional :: dsne
character(len=*),intent(in),optional :: usere
character(len=*),intent(in),optional :: passworde
character(len=*),intent(in),optional :: dsntem
character(len=*),intent(in),optional :: usertem
character(len=*),intent(in),optional :: passwordtem
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


!>\brief Controllo di Qualit� temporale.
!!Questo � il vero e proprio controllo di qualit� temporale.

SUBROUTINE quacontem (qctem,battrinv,battrcli,battrout,&
 anamask,timemask,levelmask,timerangemask,varmask,networkmask)


type(qctemtype),intent(in out) :: qctem !< Oggetto per il controllo di qualit�
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

if (qctem%operation == "gradient") then
  if ( size(qctem%v7d%level)      > 1 .or.&
       size(qctem%v7d%timerange)  > 1 .or.&
       size(qctem%v7d%dativar%r)  > 1 ) then
    call l4f_category_log(qctem%category,L4F_ERROR,"gradient operation manage one level/timerange/var only")
    call raise_error()
  end if

  write (11,*) qctem%v7d%level(1), qctem%v7d%timerange(1), qctem%v7d%dativar%r(1)

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



!!$	SUBROUTINE QUACONTEM (IMINUTI,DATI,FLAG,INTMAX,RJUMP,GRADMAX,
!!$	1	N2,NB,NE,IER)
!!$
!!$COMSTART QUACONTEM
!!$C	SUBROUTINE QUACONTEM (IMINUTI,DATI,FLAG,INTMAX,RJUMP,GRADMAX,
!!$C				N2,NB,NE,IER)
!!$C
!!$c Esegue un test temporale sui dati.
!!$c 
!!$c L'asse dei tempi che viene espresso in minuti e` contenuto in IMINUTI.
!!$c Per effettuare il controllo i dati non devono essere distanziati nel tempo
!!$c piu` di INTMAX nel qual caso la serie viene spezzata in serie piu` piccole.
!!$c Gli estremi delle serie non potranno essere controllati con il test dei 
!!$c massimi e minimi
!!$c Non vengono trattati i dati > 32767 o con flag >=iatt.
!!$c (vedi :	COMMON /FLAGSOGLIA/IATT	
!!$c		DATA IATT/3/	
!!$c	nella function vf(flag)		)
!!$c
!!$c 1) test di variazione assoluta nel tempo
!!$c Fa un controllo sui dati in maniera temporale controllando che la variazione
!!$c assoluta tra due dati non superi RJUMP. Nel caso vengono settati tutti e due i
!!$c dati errati e ricomincia col controllare il dato successivo. Se il test
!!$c viene superato la flag non viene alterata altrimenti la flag viene 
!!$c incrementata di una unita`
!!$c 
!!$c 2) test massimi e minimi	
!!$c Solo se viene superato il primo test viene controllato che il dato considerato
!!$c non sia un massimo o un minimo e che contemporaneamente non differisca dai
!!$c valori intorno piu` di GRADMAX  per minuto. Se il test
!!$c viene superato la flag viene decrementata altrimenti la flag viene 
!!$c incrementata di una unita`. Gli estremi delle serie non potranno essere 
!!$c controllati con il test dei massimi e minimi e la flag non verra` alterata.
!!$c
!!$C===============================================================================
!!$c	INPUT:
!!$c
!!$c	IMINUTI(N2) :	I*4	CONTIENE LA COORDIMATA TEMPO (IN MINUTI)
!!$c	DATI(N2): 	I*4	VETTORE CONTENENTE TUTTI I DATI
!!$c	FLAG(N2)	BYTE	VETTORE DELLE FLAG ASSOCIATE AI DATI
!!$C	INTMAX		I*4	DISTANZA MASSIMA TEMPORALE TRA I DATI
!!$C	RJUMP		R*4	VARIAZIONE TEMPORALE ASSOLUTA ACCETTATA
!!$C				ESPRESSA IN 
!!$C			(UNITA` DI MIS. DATI/UNITA` MIS. COORDINATA TEMPO)
!!$C	GRADMAX		R*4	VARIAZIONE TEMPORALE PER MASSIMI E MINIMI 
!!$C				ACCETTATA ESPRESSA IN 
!!$C			(UNITA` DI MIS. DATI/UNITA` MIS. COORDINATA TEMPO)
!!$c	N2	:	I*4	DIMENSIONE DEI VETTORI: IMINUTI(N2),
!!$C							DATI(N2),
!!$C							FLAG(N2).
!!$C				INDICE DELL' ELEMENTO CON CUI SI FINISCE
!!$c				IL CONTROLLO
!!$C
!!$C===============================================================================
!!$C	OUTPUT:
!!$C
!!$c	FLAG(N2) :    BYTE	FLAG ASSOCIATE AI DATI
!!$C	NB	:	I*4	NUMERI DI DATI A CUI LA FLAG E` STATA
!!$C				DECREMENTATA
!!$C	NE	:	I*4	NUMERO DI DATI A CUI LA FLAG E` STATA
!!$C				INCREMENTATA (DI UNA UNITA`)
!!$C	IER	:	I*4	INDICATORE DI ERRORE
!!$C
!!$C	IER =  0    : 	TUTTO O.K.
!!$C	IER =  1    :   L'indice  N2 < 1
!!$C	IER =  2    :	Nessun dato buono nell' intervallo  
!!$C	IER = -1    :   esiste un buco fra i dati superiore a INTMAX
!!$C	IER = -2    :   Sono state incrementate piu` di tre flag
!!$C
!!$COMEND
!!$
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
!!$
!!$
!!$	INTEGER*4 IMINUTI(N2),DATI(N2)
!!$	BYTE FLAG(N2)
!!$	logical j_c_e,vf
!!$
!!$C  Test che controlla la dimensione del vettore con l'intervallo da controllare
!!$	IER=1
!!$	IF(N2.LT.1) RETURN
!!$
!!$	NB=0		! NUMERO DEI DATI BUONI 	totale
!!$	NE=0		! NUMERO DEI DATI ERRATI	totale
!!$	IER=0	
!!$	IN=0		!numero di dati buoni 		parziale
!!$	IND1=1
!!$	IND2=1		!indici dei dati da analizzare
!!$	IND3=1
!!$	IND_B=0		! indice dato maxmin da decrementare
!!$
!!$	DO I=1,N2
!!$d		type*,'   i=',i,'   in=',in
!!$d		type*,'ind1=',ind1,' ind2=',ind2,' ind3=', ind3
!!$C	scarto i valori mancanti e gia flaggati errati
!!$
!!$	IF(j_c_e(DATI(I)).AND.vf(FLAG(I)))THEN
!!$C	se il dato e` presente
!!$	 IND1=IND2			!dato gia` controllato
!!$	 IND2=IND3			!dato da controllare
!!$	 IND3=I				!ultimo dato buono
!!$c
!!$c		type *,'------------------------------'
!!$c		type*,'   i=',i,'   in=',in
!!$c		type*,'ind1=',ind1,' ind2=',ind2,' ind3=', ind3
!!$
!!$C	verifico che l'intervallo tra loro non sia superiore a intmax	
!!$C	controlla l'intervallo col dato precedente buono
!!$	 IF ((IMINUTI(I)-IMINUTI(IND2)).GT.INTMAX)THEN
!!$
!!$C	  IN=-1
!!$C	  NE=-1
!!$C	  RETURN
!!$
!!$	  IER=-1	! condizione di errore in caso di intervallo >INTMAX
!!$	  IN=1		! NUMERO DEI DATI BUONI 
!!$	  IND1=I
!!$	  IND2=I
!!$	  IND3=I
!!$	  IND_B = 0
!!$	  GOTO 123
!!$
!!$	END IF
!!$C	Faccio un controllo sui dati in maniera temporale controllando
!!$c	che la variazione assoluta tra due dati non superi rjump.
!!$c	Nel caso li setto tutti e due errati e ricomincio col
!!$c	controllare il dato successivo.
!!$
!!$	 IF (IN.GE.1)THEN		!ci sono due dati da controllare
!!$	  GRAD2=FLOAT((DATI(IND2)-DATI(IND3)))/
!!$	1	FLOAT((IMINUTI(IND2)-IMINUTI(IND3)))
!!$	  IF(abs(grad2).GE.RJUMP)THEN
!!$C	    incrementa contatore errori e ricopre il dato errato
!!$	    NE=NE+2
!!$	    FLAG(IND2)=FLAG(IND2)+1
!!$	    FLAG(IND3)=FLAG(IND3)+1
!!$	    IN=1		! NUMERO DEI DATI BUONI 
!!$	    IND1=I
!!$	    IND2=I
!!$	    IND3=I
!!$	    IND_B=-I	! Il dato di inizio serie e' stato segnalato errato
!!$	    GOTO 123
!!$	  END IF
!!$	 END IF
!!$
!!$
!!$C	Faccio un controllo sui dati in maniera temporale controllando
!!$C	che il dato considerato non sia un massimo o un minimo e che
!!$C	contemporaneamente non differisca dai valori intorno piu` di
!!$C	GRADMAX  per minuto.
!!$
!!$	 IF (IN.GT.1)THEN		!ci sono tre dati da controllare
!!$
!!$	  if(IND_B.gt.0)then		! decremento flag maxmin
!!$	    NB=NB+1
!!$	    FLAG(IND_B)=FLAG(IND_B)-1	!dato buono
!!$	  endif
!!$C	  controlla i gradienti
!!$	  GRAD1=FLOAT((DATI(IND2)-DATI(IND1)))/
!!$	1	FLOAT((IMINUTI(IND2)-IMINUTI(IND1)))
!!$C	  GRAD2=FLOAT((DATI(IND2)-DATI(IND3)))/
!!$C	1	FLOAT((IMINUTI(IND2)-IMINUTI(IND3)))
!!$
!!$C	  se sono entrambi superiori a gradmax
!!$	  IF (ABS(GRAD1).GT.GRADMAX.AND.ABS(GRAD2).GT.GRADMAX)THEN
!!$C	   se sono di segno opposto
!!$	   IF ((sign(1.,GRAD1)*sign(1.,GRAD2)).LT.0.)THEN
!!$C	    incrementa contatore errori e ricopre il dato errato
!!$	    NE=NE+1
!!$	    FLAG(IND2)=FLAG(IND2)+1
!!$	    IND2=IND1
!!$c	perde un turno 
!!$c	se il primo dato della tripletta era errato non devo decrementare
!!$c	la flag di quello centrale
!!$c	se il primo dato e' buono mi comporto come inizio serie per evitare
!!$c	di ridecrementare il primo
!!$	    if(ind_b . gt. 0) IND_B = 0
!!$	    GOTO 123
!!$	   END IF
!!$	  END IF
!!$c	  type *,ind2
!!$
!!$c	Verico che la tripletta di dati non inizi con un dato errato
!!$c	se a fine serie decremento la flag del penultimo dato
!!$c	altrimenti memorizzo l'indice del dato in ind_b e continuo
!!$c	il loop per testare il dato seguente
!!$c	
!!$	  if(ind1.ne.-ind_b)then
!!$	    if(ind3 .eq. n2)then
!!$	        NB=NB+1
!!$		FLAG(IND2)=FLAG(IND2)-1	!dato buono
!!$	    else
!!$		ind_B=ind2
!!$	    endif
!!$	  end if
!!$
!!$	 END IF
!!$C	 se il dato c'e` e non e` errato	(non si sa se e` buono)
!!$	 IN=IN+1
!!$	END IF
!!$123	END DO
!!$
!!$C	se ci sono piu` di tre dati considerati errati 
!!$C	termina in condizione di errore
!!$c	uguale se non ne ha trovato neanche uno buono
!!$
!!$	IF(NE.GE.3) IER=-2
!!$	IF(NB.EQ.0) IER= 2
!!$
!!$	RETURN
!!$	END