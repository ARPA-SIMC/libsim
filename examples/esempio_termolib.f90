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
program esempio_termolib

use termolib
use missing_values
use file_utilities
use phys_const


implicit none

!-------------------------------------------------------
!
!     Programma di test della nuova termolib
!
!-------------------------------------------------------

real :: Q,acin,acape,altstaz,c_liftd,con_lev,eql_ii,fcl,h_zero,rhstaz,tamb
real :: s_li,s_k,s_u,p_zero,ur,s_sw ,s_sh,SAMIX, s_tt,teta,tas,TAS_lift
real :: Tete,tetetete,TLCL,Tww,Tv,zz
integer :: ict,k

!      leggo i dati osservati

  integer::npoint
  parameter(npoint=300)
  real,dimension(npoint)::pp,tt,dd,dir,for,alt
  real,dimension(npoint)::ao,aw
  INTEGER::unit,ier

!apertura file di output :
  unit = open_package_file('estra_temp.dat', filetype_data)
  open(unit=22,file='testa_termo.lis',status='unknown')
!apertura file di input :

  ier=0
  ict=0
  do while (ier == 0)

     ict=ict+1
!leggo dal file di input pressione [hPa], temperatura [k], temperatura di
!rugiada [K], Direzione del vento [Sess.], forza del vento [KT], altezza [m]
!che per provare il programma di esempio tornero' a calcolare

     read(unit,*,end=33)pp(ict),tt(ict),dd(ict),dir(ict),for(ict),alt(ict)
        
     for(ict)=for(ict)*0.5148     !converto da KT a m/s
     Ao(ict)=O(Tt(ict),Pp(ict))   !temperatura potenziale
     Aw(ict)=W(dd(ict),Pp(ict))   !rapporto di mescolanza

  end do
      
33 close(unit)
      
  ict=ict-1
!calcolo una serie di indici di stabilita' atmosferica
  s_k  = si_k   (pp,tt,dd,ict)                     !indice K
  s_li = si_li  (pp,tt,dd,ict)                     !lifted index
  s_sh = si_sh  (pp,tt,dd,ict)                     !Showalter
  s_sw = si_sw  (pp,tt,dd,ict,pp,for,dir,ict )     !S.W.E.A.T.
  s_tt = si_tt  (pp,tt,dd,ict)                     !Total-Total
  s_u  = si_u   (pp,tt,dd,ict)                     !indice di umidita' medio

!calcolo per ogni livello di pressione: 
!umidita' relativa [%], umidita' specifica [g/Kg],
!temperatura potenziale [C],
!temperatura equivalente potenziale [C], temperatura di bulbo bagnato [C],
!temperatura virtuale [C], altezza del livello di pressione [m]

  altstaz=103    !altezza pozzetto barometrico di Milano Linate

  write(22,'(''    pres    temp      td      u%       q   teta    tetae'',&
       &''  tetae2      tw   tvirt       z'')')

  do k=1,ict
         
     if(k > 1 )zz=z(pp(k),pp,tt,dd,ict) +altstaz

     ur=fr(tt(k),dd(k))
     Q=W(dd(k) ,Pp(k) )
     teta= o(tt(k),pp(k) ) -t0c
     Tete=(Oe(dd(k) ,Tt(k) ,Pp(k) ))-t0c
     Tetetete=(OOee(dd(k) ,Tt(k) ,Pp(k) ))-t0c
     Tww=(Tw(dd(k) ,Tt(k) ,Pp(k) ))-T0c
     Tv=(Tvir(dd(k) ,Tt(k) ,Pp(k) ))-T0c
     write(22,22)pp(k),tt(k)-t0c,dd(k)-t0c,ur,q,teta,tete,tetetete,tww,tv,zz

  end do

!convective condensation level - il samix resta costante fino al lfc
      SAMIX=PKAVR(Pp,AW,ict,Pp(1),Pp(1)-50.) !wmed primi 50 hPa
      
      con_lev=ccl(pp(1)-50,pp,tt,dd,samix,ict)
!lifted condensation level 
      c_liftd=alclm(samix,ao(1),pp(1))      !prima funzione 
!      tdew=tmr(samix,pp(1))
!      e_liftd=alcl(tdew,tt(1),pp(1))        !seconda funzione stesso
                                             !risultato della prima
      TLCL=tmr(samix,c_liftd)                !Temperatura al LCL
      TAS_lift=OS(TlCL,c_liftd) 

!calcolo il livello di equilibrio
!      eql_i=aeql(pp,tt,ict,c_liftd,tas_lift) meglio il mio    

!level of free convection
      fcl=alfc(pp,tt,aw,ict,50.)
!trovo la temperatura del level of free convection, per definizione
!si trova lungo la curva di stato (temperature osservate)
      tamb=trpt(pp,tt,ict,fcl)       
      tas=os(tamb,fcl)
!calcolo il livello di equilibrio con una funzione alternativa
      eql_ii = eql (pp,tt,ict,fcl,tas)
!calcolo il cape con il secondo livello di equilibrio

      acape= cape (pp,tt,ict,fcl,eql_ii)
      acin=cin(pp,tt,dd,ict)

      rhstaz=80.
      call zero_termico (pp,tt,dd,ict,rhstaz,p_zero,h_zero)

      write(22,'(/)')
      write(22,*)' k index                 : ',s_k
      write(22,*)' LIFTED-INDEX            : ',s_li
      write(22,*)' SHOWALTER               : ',s_sh
      write(22,*)' SWEAT                   : ',s_sw
      write(22,*)' total-total             : ',s_tt
      write(22,*)' Humidity                : ',s_u
      write(22,*)' Convective Cond. Lev.   : ',con_lev
      write(22,*)' Lifted Cond. Lev.       : ',c_liftd
      write(22,*)' Level Free Convect.     : ',fcl
      write(22,*)' Equilibrium Level       : ',eql_ii
      write(22,*)' CAPE                    : ',acape
      write(22,*)' CIN                     : ',acin
      write(22,*)' altezza zero termico    : ',h_zero
      write(22,*)' pressione zero termico  : ',p_zero

 22   format(10(1x,f7.2))


      stop
    end program esempio_termolib


