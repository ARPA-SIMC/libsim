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
module termolib


use missing_values
use phys_const

!-----------------------------------------------------------------------------
!
! Libreria Termodinamica
!
! La libreria, originariamente scritta in fortran77 è stata tradotta in 
! fortran90.
! 
! Sono state eliminate le istruzioni di  salto condizionato, ma soprattutto
! gli entry point, gli if aritmetici e le terminazioni labellate di ciclo 
! con istruzioni condivise che in un prossimo futuro non sarebbero più state
! compilate.
!
!
!------------------------------------------------------------------------------

implicit none

CONTAINS

!------------------------------------------------------------------------------
!
!                 TEMPERATURE CONVENZIONALI
!
!------------------------------------------------------------------------------

elemental real function OE(TD,TT,PT)

! Calcola la  Temperatura  Equivalente  Potenziale Adiabatica , dapprima trova 
! la pressione  di saturazione, riporta la satura alla   1000   hPa e trova la 
! temperatura di bulbo bagnato potenziale,risale fino a che l'adiabatica 
! satura coincide con l'adiabatica secca per poi  tornare alla 1000 hPa lungo
! un'adiabatica secca -  varia di alcuni decimali rispetto alla temperatura
! pseudo-equivalente potenziale calcolata riportando alla 1000 hPa la 
! temperatura equivalente isobarica le due temperature differiscono di
! alcuni decimali poiche' utilizzando il processo adiabatico l'acqua perduta
! per evaporazione del vapore viene persa alla
! temperatura presente in atmosfera al livello in cui coincidono adiabatica 
! satura ed adiabatica secca, ragion per cui la pseudo potenziale e' piu' 
! calda della equivalente potenziale che perde acqua ad una temperatura piu'
! elevata. 
!  
!  Uso : x = OE (TD,T,P) 
! 
!  Input : 
!  TD       real   Temperatura di rugiada                            (K.) 
!  T        real   Temperatura dell'aria                             (K.) 
!  P        real   Pressione aria                                    (hPa) 
! 
!  Output : 
!  OE       real    Temperatura Equivalente Potenziale               (K.) 
!  OE =   rmiss   Se non e' possibile calcolarla 
!
!------------------------------------------------------------------------------

  real,intent(in)::td,tt,pt
  real::atw                    !variabile di lavoro

  if( c_e(td) .and. c_e(tt) .and. c_e(pt) )then 

    atw = TW(td,tt,pt) 
    OE  = OS(atw,pt) 
  
  else

     OE=rmiss 

  end if

  return 

end function OE

!------------------------------------------------------------------------------

elemental real function O(T,PT)

!  Calcola la Temperatura potenziale date la temperatura e la pressione. 
!  secondo la formula : 
! 
!  O = T*((1000./P)**.286) 
! 
!  Dove : T e` la temperatura dell'aria                (K.) 
!         P e` la pressione dell'aria                  (hPa) 
! 
!  La temperatura Potenziale rappresenta anche la temperatura adiabatica secca 
! 
!  Uso : x = O (T,P) 
! 
!  Input : 
!  T      real    Temperatura dell'aria                (K.) 
!  PT     real    Pressione aria                       (hPa) 
! 
!  Output : 
!  O      real    Temperatura potenziale               (K.) 
!  O =  rmiss    Se non e' possibile calcolarla 
!
!------------------------------------------------------------------------------

  real,intent(in)::t,pt

  if( c_e(t) .and. c_e(pt) )then 

    O=t*((1000./pt)**.286) 

  else

    O=rmiss 

  end if

  return 
end function O

!------------------------------------------------------------------------------

elemental real function OW(TD,TT,PT)
 
!  Calcola la Temperatura Potenziale di Bulbo Bagnato in (K) 
!
!  Uso :  x = OW (TD,TT,PT) 
! 
!  Input : 
!  TD      real   Temperatura di rugiada                 (K.) 
!  T       real   Temperatura dell'aria                  (K.) 
!  P       real   Pressione aria                         (hPa) 
! 
!  Output : 
!  OW      real   Temperatura di Bulbo Bagnato           (K.) 
!  OW =  rmiss   Se non e' possibile calcolarla 	
!
!------------------------------------------------------------------------------

  real,intent(in)::td,tt,pt       !input
  real::atw,aos                   !variabili di lavoro
 
  if(c_e(td) .and. c_e(tt) .and. c_e(pt) )then 

    atw = TW  (td,tt,pt) 
    aos = OS  (atw,pt) 
    OW  = TSA (aos,1000.) 

  else

     ow=rmiss 

  end if

  return 
end function OW

!------------------------------------------------------------------------------

elemental real function  OS(T,PT)

!  Calcola la Temperatura Adiabatica Satura 
!  caratterizzata dalla temperatura T e dalla pressione P 
! 
!  Uso :  x = OS (T,P) 
! 
!  Input : 
!  T       real   Temperatura dell'aria                   (K.) 
!  PT      real   Pressione aria                          (hPa) 
! 
!  Output : 
!  OS       real  Temperatura Adiabatica Satura           (K.) 
!  OS =   rmiss  se non e' possibile calcolarla  
!------------------------------------------------------------------------------

  real,intent(in)::t,pt

  if( c_e(t) .and. c_e(pt) )then 

    OS=t*((1000./pt)**.286)/(exp(-2.6518986* W(t,pt) /t)) 

  else

     OS=rmiss 

  end if

  return 

end function OS
 
!------------------------------------------------------------------------------

elemental real function  TE(TD,TT,PT)

!  Calcola la Temperatura Equivalente Isobarica espressa in gradi Kelvin 
! 
!  Uso :  x = TE (TD,T,PT) 
! 
!  Input : 
!  TD   real   Temperatura di rugiada                      (K.) 
!  T    real   Temperatura dell'aria                       (K.) 
!  P    real   Pressione aria                              (hPa) 
! 
!  Output : 
!  TE     real   Temperatura Eqivalente  Isobarica         (K.) 
!  TE =  rmiss Se non e' possibile calcolarla 
!------------------------------------------------------------------------------

  real,intent(in)::td,tt,pt       !input
  real::aoe                        !variabile di lavoro

  if( c_e(td) .and. c_e(tt) .and. c_e(pt) )then 

    aoe = OE  (td,tt,pt) 
    TE  = TDA (aoe,pt) 

  else

     TE=rmiss 

  end if

  return 
end function TE

!------------------------------------------------------------------------------

elemental real function TRUG(UMID,T)

!  Calcola la temperatura di rugiada  secondo la stessa formula 
!  usata per il calcolo dell'umidita` relativa, risolta per 
!  la temperatura di rugiada. 
! 
!  Uso :  x = TRUG (UMID,T) 
! 
!  Input : 
!  UMID       real  Umidita` relativa                        (%) 
!  T          real  Temperatura dell'aria                    (K.) 
! 
!  Output : 
!  TRUG       real  Temperatura di ruguada                   (K.) 
!  TRUG =     rmiss  Se non e' possibile calcolarla 
!-----------------------------------------------------------------------------

  real,intent(in) :: umid,t               !input

  real,parameter  :: D=237.3,C=17.2693882
  real,parameter  :: psva=6.1078        !Hpa pressione di saturazione del vapore acqueo
  real::es,e                            !variabili di lavoro

  if( c_e(umid) .and. c_e(t) )then

    if (umid < 0.1)then
      trug=0.
    else
      es=ESAT(t)                           ! >>> Calcolo la P.v.s 
      e=umid/100.*es                       ! >>> Calcolo la P.v. 
      TRUG=(D*log(E/psva))/(C-log(E/psva)) ! >>> Calcolo la T.d. 
      TRUG=TRUG+t0c                        ! >>> Calcolo la T.d. in Kelvin 
    end if

  else

     TRUG=rmiss 

  end if

  return 
end function TRUG
 
!----------------------------------------------------------------------------


elemental real function  TW( TD,TT,PT )

!  Calcola la Temperatura di Bulbo Bagnato isobarica, differisce sul
!  secondo decimale dalla temepratura di bulbo bagnato adiabatica, poichè
!  si satura con acqua immessa alla temperatura TT (temperatura di
!  saturazione, anzichè alla T. E' la stessa temperatura di bulbo bagnato
!  che si può calcolare graficamente su di un nomogramma termodinamico.
!  
!  Uso : x = TW (TD,T,PT) 
! 
!  Input : 
!  TD   real  Temperatura di rugiada                        (K.) 
!  T    real  Temperatura dell'aria                         (K.) 
!  PT   real  Pressione dell'aria                           (K.) 
! 
!  Output : 
!  TW       real  Temperatura di Bulbo Bagnato isobaria     (K.) 
!  TW =  rmiss  Se non e' possibile calcolarla  
!
!----------------------------------------------------------------------------

  real,intent(in)::td,tt,pt             !input
  real::aw,ao,pi,x,aos,ti                !variabili locali di lavoro
  integer::i

  if( c_e(td) .and. c_e(tt) .and. c_e(pt) )then 

    aw = W (td,pt) 
    ao = O (tt,pt) 
    pi = pt 
!con 10 cicli l'umidità specifica media tra td e tw dovrebbe tendere a
!essere quella alla TW, se converge prima si esce dal ciclo 
    do    i =1,10 
      x  =  .02*( TMR(aw,pi) - TDA(ao,pi) ) 
      if ( abs(X) < 0.01  )then
        ti = TDA (ao , pi)
!trovato il punto di intersezione,calcoliamo l'adiab. satura che ci passa 
        aos  =   OS (ti  , pi) 
        TW   =  TSA (aos , pt) 
        return
      end if
      pi = pi* ( 2.**(X)  ) 
    end do
     
    ti=TDA (ao,pi) 
 
!trovato il punto di intersezione,calcoliamo l'adiab. satura che ci passa
    aos  =   os (ti  , pi) 
    TW   =  TSA (aos , pt) 

  else

     TW=rmiss 

  end if

  return 
end function TW

!--------------------------------------------------------------------------

elemental real function TVIR(TD,T,P)

!  Calcola la Temperatura virtuale dell'aria secondo la formula: 
! 
!  TVIR = (1+0.00061*USPEC(TD,P))*T 
! 
!  Dove : USPEC e` una funzione che calcola l'umidita` specifica in (gr/Kg) 
!         TD    e` la Temperatura di rugiada espressa in (K.)) 
!         T     e` la Temperatura dell'aria espressa in (K.) 
!         P     e` la pressione dell'aria espressa in (hPa) 
! 
!  Uso :  x = TVIR (TD,T,P) 
! 
!  Input : 
!  TD        real  Temperatura di rugiada                   (K.) 
!  T         real  Temperatura dell' aria                   (K.) 
!  P        real  Pressione dell' aria                     (hPa) 
! 
!  Output : 
!  TVIR      real  Temperatura virtuale                     (K.) 
!  TVIR =  rmiss  Se non e' possibile calcolarla 
!----------------------------------------------------------------------------

  real,intent(in)::td,t,p

  if( c_e(td) .and. c_e(t) .and. c_e(p) )then 

    TVIR = tvir2 (t, USPEC(td , p ) ) 

!    TVIR = (1 + 0.00061 * USPEC(td , p ) ) *t 

  else

     TVIR=rmiss 

  end if


  return 
end function TVIR


!>  Calcola la Temperatura virtuale dell'aria secondo la formula: 
!!  TVIR = (1+0.00061*Q)*T 
!!  (K.) 
elemental real function TVIR2(T,q)

real,intent(in) :: t !< specific humidity (gr/Kg) 
real,intent(in) :: q !< temperature (K)


if( c_e(t) .and. c_e(q) )then 

  TVIR2 = (1 + 0.00061 * q ) * t

else

  TVIR2=rmiss 

end if


return 
end function TVIR2


!--------------------------------------------------------------------------
!
!       UMIDITA'  E PRESSIONE DI VAPORE
!
!--------------------------------------------------------------------------


elemental real function USPEC(TD,PT)

!  Calcola l'umidita` specifica secondo la formula:
! 
!  USPEC = (0.622*(ESAT(TD)/(P-(1-0.622)*ESAT(TD)))*1000
!
!  Dove : ESAT()  e` una Funzione che calcola la pressione di vapor saturo
!         TD      e` la Temperatura di rugiada                 (K.)
!         P       e` la Pressione dell' aria                   (hPa)
!
!  Uso :  x = USPEC (TD,P)
!
!  Input :
!  TD    R*4  Temperatura di rugiada                           (K.)
!  P     R*4  Pressione aria                                   (hPa)
!
!  Output :
!  USPEC      R*4   Umidita` specifica                         (gr./kg.)
!  USPEC =  rmiss  Se non e' possibile calcolarla.
!
!---------------------------------------------------------------------------

  real,intent(in)::td,pt

  if(c_e(td) .and. c_e(pt) )then

    USPEC=eps0*(ESAT(td)/(PT-(1-EPS0)*ESAT(td)))
    USPEC=USPEC*1000.

  else

     USPEC=rmiss

  end if

  return

end function USPEC

!--------------------------------------------------------------------------
 
elemental real function FR (T,TD )

! Calcola l'umidita` relativa  secondo la formula: 
!
!    FR  =  ( ESAT(TD)/ESAT(T) ) * 100. 
! 
!    Dove : ESAT()  e` una funzione che calcola la Pressione di vapor saturo 
!           TD      e` la Temperatura dell' aria      (K.) 
!           T       e` la Temperatura di rugiada      (K.) 
! 
!    Uso :   x = FR (T,TD) 
! 
!    Input : 
!    T      Real   Temperatura aria                   (K.) 
!    TD     Real   Temperatura di rugiada             (K.) 
! 
!    Output : 
!    FR     Real   Umidita` relativa                  (%) 
!    FR = rmiss   Se non e' possibile calcolarla. 
!---------------------------------------------------------------------------

  real,intent(in)::t,td

  if( c_e(t) .and. c_e(td) )then 

    FR  =  100.*( ESAT(td)/ESAT(t) ) 

  else

    FR=rmiss 

  end if
  
  return 
end function FR
 
!-----------------------------------------------------------------------------

elemental real function W(TD,PT) 

!  Calcola il rapporto di mescolanza  secondo la formula: 
! 
!  W = 622.*(ESAT(TD)/(P-ESAT(TD))) 
! 
!  Dove : ESAT  e` una Funzione che calcola la pressione di vapor saturo 
!         TD    e` la Temperatura di rugiada                 (K.) 
!         P     e` la Pressione dell' aria                   (hPa) 
! 
!  Uso :   x = W (TD,P) 
! 
!  Input : 
!  TD      real  Temperatura di rugiada                      (K.) 
!  PT      real  Pressione aria                              (hPa) 
! 
!  Output : 
!  W       real  Rapporto di mescolanza                      (gr/kg) 
!  W =   rmiss  Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  real,intent(in)::td,pt
  real::x

  if( c_e(td) .and. c_e(pt) )then 

    x  =  ESAT(td) 
    W  =  622.*x/(pt-x) 

  else

     W=rmiss 
     
  end if

  return 
end function W

!-----------------------------------------------------------------

elemental real function QTORELHUM(Q,PT,T) 
 
! CALCOLA L'UMIDITA RELATIVA A PARTIRE DALLA UMIDITA' SPECIFICA 
! REF.: BAKER, MON.WEA.REV.,1983,111,PAG.328 E SEG. 
!----------------------------------------------------------------------------

  real,intent(in)::q,pt,t

  if( c_e(q) .and. c_e(pt) .and. c_e(t) )then 

    QTORELHUM= q * (pt-c1*ESAT(t)) / (eps0*ESAT(t))*100. 
    if (QTORELHUM  < 0.)QTORELHUM=0.
    
  else

    QTORELHUM=rmiss

  end if

  return 
end function QTORELHUM
 
!-----------------------------------------------------------------

!> Compute dew point temperature from pressure \a p and specific humidity \a q.
!! \a p in hPa, \a q in Kg/Kg, \a td in K.
!! From Fea, Elementi di dinamica e termodinamica dell'atmosfera, (4.11)
ELEMENTAL REAL FUNCTION td_pq(p,q)
IMPLICIT NONE
! td in K, p in Pa, q in Kg/Kg
REAL,INTENT(in) :: p, q
REAL :: e
REAL, PARAMETER :: rd = 287.05, rv = 461.51, &
 eps0 = rd/rv, eps1 = 1. - eps0

!REAL, EXTERNAL :: tesat

! Compute vapour partial pressure
IF (c_e(p) .AND. c_e(q)) THEN
  e=p*q/(eps0+eps1*q)
  td_pq=tesat(e)
ELSE
 td_pq = rmiss
ENDIF

END FUNCTION td_pq


!----------------------------------------------------------------- 

elemental real function RELHUMTOQ(RH,PT,T)
 
! CALCOLA L'UMIDITA SPECIFICA A PARTIRE DALLA UMIDITA RELATIVA 

  real,intent(in)::rh,pt,t

  if (c_e(rh) .and. c_e(pt) .and. c_e(t)) then

    if ( t < 8 )then         !al disotto ritorna NaN
      relhumtoq =rmiss
      return
    end if

    RELHUMTOQ=rh*(eps0*ESAT(t))/(pt-c1*ESAT(t)) 
    if (RELHUMTOQ  <  0.)RELHUMTOQ = 0. 

  else

    relhumtoq =rmiss

  end if

  return 

end function RELHUMTOQ
 
!-----------------------------------------------------------------------
 
elemental real function ESAT(T)  
 
! CALCOLA LA PRESSIONE DI VAPORE SATURO A PARTIRE DALLA TEMPERATURA 
! REF.: BAKER, MON.WEA.REV.,1983,111,PAG.328 E SEG. 
! T in Kelvin, SAT(T) in hPa ()  Sostituisce la vecchia ESAT di NCAR
!-----------------------------------------------------------------------

  real,intent(in)::t
  real::a,b

!escludo le temperature minori di 8 gradi Kelvin, poichè la function
!restituirebbe un numero infinito.

  if ( c_e(t) )then
    if ( t < 8 )then
      ESAT=rmiss
      return
    end if

    if(t > 273.16)then 
      a=17.269 
      b=35.86 
    else 
      a=21.874 
      b=7.66 
    end if
 
    ESAT=6.11*exp(a*(t-273.16)/(t-b)) 

  else

    ESAT=rmiss

  end if

  return 

end function ESAT

!< Compute temperature at which saturated vapor pressure is equal \a e.
!! e in hPa, T in K.
ELEMENTAL REAL FUNCTION tesat(e)
REAL,intent(in) :: e
!! /todo
!! tesat on e=0. produce NaN
!! so now there is a test to give missing value


REAL :: ale
REAL, PARAMETER :: es0=6.11, aw=7.567*2.3025851, bw=239.7-t0c, &
 awn=7.744*2.3025851, bwn=245.2-t0c

IF (c_e(e) .AND. e > 0.0) THEN
  ale=LOG(e/es0)
  IF (ale > 0.) THEN
    tesat=(aw*t0c+bw*ale)/(aw-ale)
  ELSE
    tesat=(awn*t0c+bwn*ale)/(awn-ale)
  ENDIF
ELSE
  tesat = rmiss
ENDIF

END FUNCTION tesat



!-----------------------------------------------------------------


!> compute air density from virtual temperature and pressure
elemental real function AIRDEN(tv,p) 
real,intent(in) :: tv  !< virtual temperature (K)
real,intent(in) :: p   !< pressure (Pa)

if( c_e(tv) .and. c_e(p) )then 

  airden = p / (rd *tv)
  
else
  
  airden=rmiss
  
end if

return 
end function AIRDEN
 

!-----------------------------------------------------------------------------
!
!   PARAMETRI TERMODINAMICI PER COSTRUIRE I PROFILI VERTICALI RELATIVI
!   AI NOMOGRAMMI
!    
!------------------------------------------------------------------------------

real function AEQL(PT,T,NT,PLOW,THS) 

!
!     AEQL- EQUILIBRIUM LEVEL OF A SOUNDING (HPA)
!
!     PT- PRESSURE ARRAY (HPA)
!     T- TEMP ARRAY (KELVIN)
!     N- NUMBER OF CONTACT POINTS
!     PLOW- CCL PRESSURE (HPA)
!     THS- SATURATED ADIABAT (KELVIN)
!
!     OUTPUT PARAMETERS.....
!     IF THS EQUALS 0, PLOW IS ASSUMED TO BE THE CCL AND THS IS CALCULATED
!     ALL OTHERS REMAIN UNCHANGED
!
!------------------------------------------------------------------------------
  
  integer,intent(in)::nt
  real,intent(in),dimension(nt)::pt,t
  real,intent(in)::plow,ths
  real::ths_l
  real::x,pc,del,tccl
  integer::j,ih,il,i

  pc=plow
  ths_l=ths

  if(ths <  0.0)then
     tccl= TRPT(pt,t,nt,plow)
     ths_l=  OS(tccl,plow)
  end if
     
  do  j=1,nt-1

     i=nt-j

     if(pt(I+1) > plow)then
        AEQL=pc
        return
     end if

     il=i
     x=TSA(ths_l,pt(I))-t(I)
     if(x >= 0.0)exit 

  end do

  ih=il+1
  del=pt(il)-pt(ih)
  pc=pt(ih)+0.5*del

  do  j=1,15
     del=del*0.5
     x= TRPT(pt,t,nt,pc)- TSA(ths_l,pc)
     pc=pc+sign(del,x)
  end do

  AEQL=pc

  return
end function AEQL



real function eql (pt,tt,np,plow,tlow)
!
! Calcola il livello di equilibrio, definito come il livello in cui
! la temperatura di una particella sollevata lungo l'adiabatica satura 
! partente dal livello di libera condensazione o dal livello di 
! sollevamento forzato si trova ad una temperatura inferiore a quella
! dell'ambiente circostante.
! Se l'adiabatica satura partente dal llc o dal lsf non intercetta mai 
! la curva di stato (temperatura dell' ambiente) , ossia non c'e' area di 
! instabilità, la function ritorna 0 
!   
!-------------------------------------------------------------------------
 
  integer,intent(in)::np
  real,dimension(np),intent(in)::pt,tt
  real,intent(in)::plow,tlow
  real::pres,temp,tamb

  if ( pt(1) <= pt (np) .or. pt(1) < 0 .or. tt(1) < 0 )then
     eql=rmiss
     return
  end if

  eql=0.
  pres=plow

  do while (pres > 100. )

     pres=pres-1
     temp=TSA(tlow,pres)
     tamb=trpt(pt,tt,np,pres)
     
     if( temp+1 < tamb)then
        eql=pres
        return
     end if
        
  end do

  return
end function eql


!----------------------------------------------------------------


real function alfc (pt,tt,aw,np,strato)

! calcola il livello di libera condensazione convettiva 
! Function completamente riscritta, sostitisce la precedente
! N.C.A.R.
!
! ALFC- LEVEL OF FREE CONVECTION PRESSURE (HPA)
! RETURNS 0.0 IF NONE EXISTS
!
! Pt-     PRESSURE ARRAY (HPA)
! Tt-     TEMP ARRAY (KELVIN)
! AW -    mixing ratio Array [g/Kg]
! NP-     NUMBER OF CONTACT POINTS
! strato- spessore dello strato [hPa]
!
!----------------------------------------------------------------

  integer,intent(in)::np
  real,intent(in),dimension(np)::pt,tt,aw
  real,intent(in)::strato
  real::t_iso,t_sat,p_bar,t_adi
  real::samix,t_bar
  real::temp,tamb,t_ad

!trovo il rapporto di mescolanza medio dello strato
  samix=PKAVR(pt,aw,np,pt(1),pt(1)-strato)
!trovo la temperatura media dello strato
  t_bar=PKAVR(Pt,tt,np,pt(1),pt(1)-strato)
!trovo la pressione media dello strato
  p_bar=(pt(1) + ( pt(1) -strato ))*0.5

!ora trovo l'intercetto tra l'iso-igrometria media dello strato
!e la tempratura lungo l'adiabatica seccca che parte dalla temperatura
!media dello strato
  t_ad=o(t_bar,p_bar)
  t_sat=t_ad
  t_iso=TMR(samix,p_bar)

  if (pt(1) < 0. .or. t_ad < 0 .or. t_sat < 0. )then
     alfc=rmiss
     return
  end if
  
  do while ( t_sat > t_iso )

     p_bar=p_bar-1
!mi fermo a 100 hPa se non trovo il cfcl
     if(p_bar < 100. )then
        alfc=0.                   !non esiste lcf
        return
     end if

     t_adi=TDA(t_ad,p_bar)
     t_iso=TMR(samix,p_bar)
         
     if ( t_iso >= t_adi )then

!se si trova l'lcl si calcola il free convective condensation level           
!partendo dall'adiabatica che sale dal lcl, e questo è anche il livello di 
!equilibrio

        t_sat=os(t_iso,p_bar)

        do while (p_bar > 100. )
           p_bar=p_bar-5
           temp=TSA(t_sat,p_bar)
           tamb=trpt(pt,tt,np,p_bar)
           
           if( temp >= tamb)then
              alfc=p_bar
              return
           end if
        
        end do

     end if
  end do
  
  return
end function alfc


real function cape (pt,tt,np,lfc,eql)

!
! Calcola l'area di istabilità per sollevamento forzato, secondo la
! definizione standard di CAPE (Convective Available Potential Energy
!
! input :
! 
! pt   :    vettore pressioni                     [hPa]
! tt   :    vettore temperature ambiente          [°K]
! np   :    numero elementi vettori pt e tt
! lfc  :    Livello di libera convezione          [hPa]
! eql  :    Livello di Equilibrio                 [hPa]
!
! output :
! 
! cape :    Convective Available Potential Energy [J/kg]
!
! Calcola la sommatoria di tutte le aree positive (trattate come trapezi)
! a passi di 1 hPa
! 
! area = ln ( p0/p) *Ra ) * dT
! 
! P0 = livello di pressione inferiore
! p  = livello di pressione superiore  (p < p0 )
! Ra = costante dei gas per aria secca
! dT = temperatura linea di processo adiabatico saturo - temperatura ambiente
!
! Ritorna 0 se non esiste cape, ritorna rmiss se non è possibile calcolarlo
!----------------------------------------------------------------------------
  integer,intent(in)::np
  real,intent(in),dimension(np)::tt,pt
  real,intent(in)::lfc,eql
  real::tlcl,pres1,pres2
  real::tamb_upp,tamb_low,temp_upp,temp_low

  if ( pt(1) <= pt (np) .or. pt(1) < 0 .or. tt(1) < 0 )then
     cape=rmiss
     return
  end if

  cape=0.

  tlcl=trpt(pt,tt,np,lfc)   !temperatura del lcl
  tlcl=os(tlcl,lfc)

  pres1=lfc+1

  do while (pres1 > eql ) 

     pres1=pres1-1
     pres2=pres1-1
     temp_low=TSA(tlcl,pres1)
     tamb_low=trpt(pt,tt,np,pres1)
     temp_upp=TSA(tlcl,pres2)
     tamb_upp=trpt(pt,tt,np,pres2)
     
!se l'area è positiva incremento la sommatoria del cape
!sommando l'area del trapezoide
     if(temp_low > tamb_low .and. temp_upp > tamb_upp)then

        cape=cape+ (alog (pres1/pres2))* rd * &
             &( temp_low - tamb_low + temp_upp -tamb_upp )/2  

     end if
         
  end do

  
  return
end function cape


!_______________________________________________________________________



real function cin (pt,tt,td,np)


! Calcola l'inibizione convettiva, ossia l'energia da fornire ad una
! particella che stazione al suolo affinchè possa raggiungere il livello
! di libera convezione - se questo esiste -
!
! input:
! pt   :    vettore pressioni                     [hPa]
! tt   :    vettore temperature ambiente          [°K]
! td   :    vettore temperature di rugiada        [°K]
! np   :    numero elementi vettori pt e tt
!
! output:
! cin  :    Convective Inhibition                 [J/Kg]    
!
! Calcola la sommatoria di tutte le aree negative (trattate come trapezi)
! a passi di 1 hPa
! 
! area = ln ( p0/p) *Ra ) * dT
! 
! P0 = livello di pressione inferiore
! p  = livello di pressione superiore  (p < p0 )
! Ra = costante dei gas per aria secca
! dT = temperatura ambiente - temperature linee processi adiabatici, prima
!      secco, poi saturo fino al livello di libera convezione, se esiste
!
! Ritorna 0 se non esiste cin, ritorna rmiss se non è possibile calcolarlo
!
!----------------------------------------------------------------------------
  
  real::strato
  integer,intent(in)::np
  parameter (strato=50.)
  real,intent(in),dimension(np)::tt,pt,td
  real,dimension(np)::aw
  real::pres1,pres2,samix,tmed
  real::tamb_upp,tamb_low,tadi_upp,tadi_low,t_iso
  integer::flag,k

  if ( pt(1) <= pt (np) .or. pt(1) < 0 .or. tt(1) < 0 &
       &.or. td(1) < 0 )then
     cin=rmiss
     return
  end if

  cin=0.

!vettore dei rapporti di mescolanza
  do k=1,np
     Aw(k)=W(td(k),Pt(k)) 
  end do

!trovo il rapporto di mescolanza medio dello strato
  samix=PKAVR(Pt,aw,np,Pt(1),Pt(1)-strato)
!trovo la temperatura media dello strato
  tmed=PKAVR(Pt,tt,np,Pt(1),Pt(1)-strato)
  
  flag=-1
  pres1=pt(1)-strato+1

  tadi_low=99999.9  
  do while ( pres1 > 100 )

!trovo il punto di saturazione ed incremento l'integrale del cin         
     pres1=pres1-1
     pres2=pres1-1

     t_iso=TMR(samix,pres1)     !calcolo la temperatura lungo l'iso-igrometrica

     tamb_low=trpt(pt,tt,np,pres1)  !interpolo la temperatura ambiente a pres1
     tamb_upp=trpt(pt,tt,np,pres2)  !interpolo la temperatura ambiente a pres2
         
     if ( tadi_low < t_iso .and. flag == -1)then  !se saturi calcoliamo
        tmed=os(tadi_low,pres1)                   !il valore della linea
        flag=0                                    !di processo ad. saturo
     end if

     if(flag == -1)then              !non siamo ancora alla saturazione
        tadi_low=TDA(tmed,pres1)
        tadi_upp=TDA(tmed,pres2)
     else                            !siamo saturi
        tadi_low=TSA(tmed,pres1)
        tadi_upp=TSA(tmed,pres2)
     end if

     cin=cin+ (alog (pres1/pres2))* rd * &
          &( tamb_low - tadi_low + tamb_upp -tadi_upp )/2   

!se trovo il livello di libera condensazione  esco dal do restituendoil CIN
!al main program, altrimenti raggiunta la pressione di 100 hPa finisce il
!do while e si ritorna al main program il valore di Cin=0

     if ( tadi_low > tamb_low )then
        cin=-cin                       !cambio di segno
        return
     end if
           
  end do

  cin=0.

  return
end function cin


!--------------------------------------------------------------------------

elemental real function ALCL(TD,TT,PT)

!  Calcola la pressione del punto di saturazione di 
!  di una massa d'aria sollevata adiabaticamente, a prescindere dalla
!  temperatura dell'ambiente circostante
! 
!      Uso : 
!                  x = ALCL (TD,T,P) 
! 
!      Input : 
!  TD   real   Temperatura di rugiada                              (K.) 
!  T    real   Temperatura dell'aria                               (K.) 
!  P    real   Pressione                                           (mb) 
! 
!      Output : 
!  ALCL     real  Pressione del Livello di Condenzazione Forzata   (mb) 
!  ALCL = rmiss Se non e' possibile calcolarla.  
!--------------------------------------------------------------------------

  real,intent(in)::td,tt,pt
  real::x,pi,ao,aw
  integer::i

  if(td < 0 .or. tt <  0 .or. pt < 0)then 
     ALCL=rmiss 
     return 
  end if
 
  aw = W(td,pt) 
  ao = O(tt,pt) 

  pi =  pt 
  do  i =1,10 
     x  =  .02*( TMR(aw,pi) - TDA(ao,pi) ) 
     if ( abs(X) < 0.01  )then
        pi = pi * ( 2.**(X)  ) 
        ALCL =  pi 
        return
     end if
     pi = pi * ( 2.**(X)  ) 
  end do

  return 
end function ALCL
 
!--------------------------------------------------------------------------

elemental real function ALCLM(aw,ao,pt)  

! calcola il lifted condensation level a partire dal rapporto di mescolanza 
! medio dello strato e dalla temperatura potenziale media dello strato
!
! uso x = ALCLM (aw,ao,pt )
!
! input  aw    reale  mixing ratio medio dello strato             [g/Kg]
! inpt   ao    reale  temperatura potenziale media dello strato   [°K]
! input  pt    reale  livello di pressione inferiore dello strato [hPa]
! output ALCLm reale  livello di condensazione forzata            [hPa]
! ALCL = rmiss Se non e' possibile calcolarlo  
!----------------------------------------------------------------------------

  real,intent(in)::aw,ao,pt
  real::x,pi
  integer::i

  pi =  pt 
  do    i =1,10 
     x  =  0.02*( TMR(aw,pi) - TDA(ao,pi) ) 
     if ( abs(X) < 0.01  )then
        ALCLM =  pi 
     end if
     pi = pi * ( 2.**(X)  ) 
  end do

  ALCLM = pi 

  return 
end function ALCLM
 

!--------------------------------------------------------------------------

elemental real function  CT(WBAR,P_CCL,P0 )     
! 
! Calcola la temperatura di attivazione termo-convettiva 
! 
! Uso :   x = CT (UBAR,P_CCL,P0) 
! 
! Input : 
! UBAR    real    Temperatura dell'aria                     (K.) 
! P_TOP   real    Pressione superiore dello stato           (hPa) 
! P0      real    Pressione superiore dello stato           (hPa) 
! 
! Output : 
! CT      real    Temperatura di attivazione 
! CT =  rmiss    Se non e' possibile calcolarli
!--------------------------------------------------------------------------

  real,intent(in)::wbar,p_ccl,p0
  real::tc,ao

  if(wbar <= 0 .or. P_CCL <= 0 .or. P0 <= 0)then 
     CT=rmiss 
     return 
  endif
 
  tc  =  TMR(wbar,p_ccl) 
  ao  =    O(tc,p_ccl ) 
  CT  =  TDA(ao,p0) 

  return 
end function CT

!----------------------------------------------------------------------------

real function CCL(PM,P,T,TD,WBAR,N)  

!  Calcola la Pressione del livello di condenzazione convettiva dello strato 
!  d'aria che va dal suolo a P_TOP 
! 
!  Uso : x = CCL (P_TOP,P,T,TD,WBAR,NT) 
! 
!      Input : 
!  P_TOP   real   Pressione superiore dello stato           (mb) 
!  P       real   Vettore delle Pressioni                   (mb) 
!  T       real   Vettore delle Temperature dell'aria       (K.) 
!  TD      real   Vettore delle Temperature di rugiada      (K.) 
!  UBAR    real   Temperatura dell'aria                     (K.) 
!  NT   integer   Numero di elementi 
! 
!  Output : 
!  CCL   real  Presione del Livello di Condenzazione Convettiva (mb.) 
!  WBAR  real  Rapporto di mescolanza medio dello strato        (gr/Kg) 
! 
!  CCL =  rmiss Se non e' possibile calcolarlo.  
!-------------------------------------------------------------------------

  integer,intent(in)::n
  real,intent(in),dimension(n)::T,P,TD 
  real,intent(in)::wbar,pm
  real::wbar_l
  integer::k,j,i,l,m
  real::pc,del,a,x,tq

  wbar_l=wbar

    if(PM /=  P(1))then      !parte con uno strato (entraiment)
       wbar_l =0 
       K=1 

       do while (  (p(k) -pm ) > 0 )
          k=k+1
       end do
       K=K-1 
       J=K-1 

       if(J >=  1)then 
!     compute the  average mixing ratio. alog is log base e 
          do   I=1,J 
             L=I+1 
             wbar_l=(W(TD(I),P(I))+W(TD(L),P(L)))*alog(P(I)/P(L))+wbar_l 
          end do
       end if

       L=K+1 
       TQ=TD(K)+(TD(L)-TD(K)  )*(alog(PM/P(K)))/(alog(P(L)/P(K))) 
       wbar_l = (wbar_l+(W(TD(K),P(K))+W(TQ   ,PM  ))*alog(P(K)/PM)) 
       wbar_l=  wbar_l/( 2.*alog(P(1)/PM) ) 

!     find the  level at which  TMR -ts  changes  sign.ts -sounding 
    else                      !parte con il primo livello
         
       wbar_l=W(TD(1),P(1)) 
       PC=PM 
         
       if(abs(T(1)-TD(1)).LT.0.05)then
          CCL=PC 
          return
       end if
    end if
 
!     find the  level at which  TMR -ts  changes  sign.ts -sounding 
    do  J=1,N 

       I=N-J+1 
       if(P(I) < 300.0)cycle
       X=TMR(wbar_l,P(I))-T(I)
 
       if(X <=  0.0)then
!         set up bisection routine 
          L=I 
          I=I+1 
          DEL = P(L)- P(I) 
          PC  =  P(I) + .5*DEL 
          A=   ( T(I)-T(L))/alog( P(L)/P(I)) 
          do  m=1,10 
             DEL = DEL/2. 
             X= TMR(wbar_l,PC)-T(L)-A*(alog(P(L)/PC)) 
             PC = PC +     sign(DEL,X) 
!    the  sign function replaces the sign of the first argument with second 
          end do

          CCL=PC 
          return 

       end if

    end do

    CCL=PC 
    return 

  end function CCL

!-------------------------------------------------------------------------- 

real function TRPT(P,A,N,TP) 

! 
!  Calcola il valore interpolato col logaritmo naturale della pressione 
!  di un parametro termodinamico alla pressione PRES 
! 
!       Formula utilizzata : 
! 
!	   A(i) * (ln(PRES)-ln(P(i+1)) - A(i+1)) * (ln(PRES)-ln(P(i)) 
! TRPT = ----------------------------------------------------------------- 
!                       ln(P(i) - ln(P(i+1) 
! 
! Uso :  x = TRPT (P,A,NT,PRES) 
! 
! Input : 
! P       real   Vettore delle pressioni                  (mb) 
! A       real   Vettore del parametro 
! NT   integer   Numero il elementi. 
! PRES    real   Pressione alla quale interpolare         (mb) 
! 
! Output : 
! TRPT     real   Valore del parametro interpolato 
! TRPT =  rmiss Se non e' possibile calcolarlo.
!------------------------------------------------------------------------

  integer,intent(in)::n
  real,intent(in),dimension(n)::P(n),A(n) 
  real,intent(in)::tp
  real::x,x1,x2
  integer::i

  TRPT = rmiss 

  if(TP <  P(N) .OR. TP > P(1))return

  do I=1,N-1 
     if(TP <  P(I+1))cycle 
     X1=alog(P(I)) 
     X2=alog(P(I+1)) 
     X=alog(TP) 
     TRPT=(A(I)*(X-X2)-A(I+1)*(X-X1))/(X1-X2) 
     return 

  end do

end function TRPT


!---------------------------------------------------------------------------

elemental real function TMR(W,P)  

! Calcola la temperatura sull' isoigrometrica 
! caratterizzata dal rapporto di mescolanza W alla pressione P. 
! 
! Uso :  x = TMR (W,P) 
! 
! Input : 
! W        real  Rapporto di mescolanza dell' isoigrometrica       (gr/Kg) 
! P        real  Pressione di intersecazione sull' isoigrometrica  (hPa) 
! 
! Output : 
! TMR      real  Temperatura                                        (K.) 
! TMR =  rmiss Se non e' possibile calcolarla 
!--------------------------------------------------------------------------

  real,intent(in)::w,p
  real::x

  X =  alog10(   W*P/(622.+ W)  ) 
  TMR=10.**(.0498646455*X+2.4082965)-7.07475+38.9114*&
       &((10.**( .0915*X ) - 1.2035 )**2 ) 

  return 
end function TMR

!---------------------------------------------------------------------------

elemental real function TDA(OO,P)    

! Calcola la temperatura sull' adiabatica secca 
! caratterizzata dalla temperatura T_AD  alla pressione P. 
! 
! Uso :  x =  TDA (T_AD,P) 
! 
! Input : 
! T_AD     real  Temperatura adiabatica secca                        (K.) 
! P        real  Pressione di intersecazione sull' isoigrometrica    (hPa) 
! 
! Output : 
! TDA      real  Temperatura                                          (K.) 
! TDA =  rmiss  Se non e' possibile calcolarla 
!---------------------------------------------------------------------------

  real,intent(in)::oo,p

  if(oo < 0 .or. P < 0)then 
     TDA=rmiss 
     return 
  end if

  TDA=oo*((P*0.001)**0.286) 

  return 
end function TDA
  
!-----------------------------------------------------------------------------

elemental real function TSA(OSAT,PT) 

! Calcola la temperatura lungo una adiabatica satura 
! caratterizzata dalla temperatura T_AS alla pressione P 
! 
! Uso :   x = TSA (T_AS,P) 
! 
! Input : 
! T_AS     real  Temperatura adiabatica satura                       (K.) 
! PT       real  Pressione di intersecazione sull' isoigrometrica    (hPa) 
! 
! Output : 
! TSA      real  Temperatura                                         (K.) 
! TSA =  rmiss  Se non e' possibile calcolarla 
!-----------------------------------------------------------------------------

  real,intent(in)::osat,pt
  real::a,tq,d,x
  integer::i
 
  if(osat <  0 .or. pt < 0)then 
     TSA=rmiss 
     return 
  end if

  a   =  osat
  tq  =  253.16 
  
  d =  120 
  do   i = 1,12 
     d = d/2. 
 
!se la differenza x di temperatura è piccola esce dal ciclo e dalla function  
     x=a*exp(-2.6518986*W(tq,pt)/tq)-tq*((1000./pt)**.286) 
     if(abs(x) < 0.01)then
        TSA=tq 
        return
     end if
     tq = tq + sign(d,x)
  end do
  
  TSA=tq 

  return 
end function TSA

!-----------------------------------------------------------------------------

subroutine TROPO(PT,T,AZ,NT,PTROP,ZTROP,RLDC,ZLAY,PLOW,PHI)  
 

! calcola l'altezza ed il livello di pressione della tropopausa 
! esce a rmiss se non trova o se non esiste la tropopausa secondo 
! i criteri inseriti, i criteri inseriti se sono posti = rmiss
! partono con dei valori di default utilizzati operativamente dall'NCAR
! 
! P-     PRESSURE ARRAY (HPA) 
! T-     TEMPERATURE ARRAY (KELVIN) 
! AZ-    HEIGHT ARRAY (METERS-MSL) 
! N-     NUMBER OF CONTACT POINTS 
! RLDC-  LAPSE RATE PER KILOMETER CRITERIA (CENTIGRADE) 
! ZLAY-  DEPTH OF LAYER REQUIRED TO MAINTAIN SPECIFIED LAPSE RATE (METERS) 
! PLOW-  BOTTOM LIMIT OF SEARCH FOR TROPOPAUSE (HPA) 
! PHI-   UPPER LIMIT OF SEARCH FOR TROPOPAUSE (HPA) 
! PTROP- TROPOPAUSE PRESSURE (HPA) 
! ZTROP- TROPOPAUSE HEIGHT (METERS-MSL) 
! 
! OUTPUT PARAMETERS..... 
! PTROP AND ZTROP ARE OUTPUT PARAMETERS. ALL OTHERS REMAIN UNCHANGED 
!-----------------------------------------------------------------------------

  integer,intent(in)::nt                          !input
  real,intent(in),dimension(nt)::PT,T,AZ          !input
  real,intent(in),optional::rldc,zlay,plow,phi    !input
  real::rldc_l,zlay_l,plow_l,phi_l                !settano i default se omessi
  real,intent(out)::ptrop,ztrop                   !output
  integer::i,k                                    !variabili locali
  real::z2,rlap,zmax,p2,t2                        !variabili locali

  PTROP=rmiss !inizializzazione 
  ZTROP=rmiss !inizializzazione

! setto i default di ricerca se non sono stati inizializzati dall'utente
! secondo lo standard operativo N.C.A.R.

  if(present(rldc)) then
     rldc_l=rldc
  else
     rldc_l=2.        ![K]
  end if

  if(present(zlay)) then
     zlay_l=zlay
  else
     zlay_l=500.
  end if

  if(present(plow)) then
     plow_l=plow
  else
     plow_l=300.     ![hpa]
  end if

  if(present(phi)) then
     phi_l=phi
  else
     phi_l=10.    ![hpa]
  end if

  if(pt(nt) > plow_l)return
  rlap=rldc_l*0.001 
	
  do  i=1,nt-1

     if(pt(i) < phi_l)return
     if(pt(i) > plow_l)cycle
     if(t(i)-t(i+1) > (az(I+1)-az(I))*rlap)cycle 
     zmax=az(i)+zlay_l
     z2=az(i)+zlay_l
     if(z2 > az(nt))zmax=az(nt) 
     if(z2 > az(nt))z2=az(nt) 
     k=i+1 
     p2=PTRP(pt,az,nt,z2) 
     t2=TRPT(pt,t,nt,p2) 

     do while (zmax >=  az(k)) 
        if(t(I)-t2 > (z2-az(i))*rlap)exit 
        k=k+1 
        z2=az(k) 
        t2=T(k) 
     end do

     PTROP=pt(i) 
     ZTROP=az(i) 

     return 

  end do

end subroutine TROPO


!----------------------------------------------------------------------


subroutine zero_termico (pt,tt,td,np,rhstaz,p_zero,h_zero)

! Calcola l'altezza ed il livello di pressione dello zero termico.
! Restituisce il dato dell'altezza dello zero termico scandendo dall'alto,
! Quindi in caso di inversioni invernali al suolo nulla vieta che lo zero
! sia presente anche a quote inferiori a quella riportata dalla subroutine
!
! uso  :  call zero_termico (pt,tt,td,np,rhstaz,p_zero,h_zero)
!
!  input :
!  PT     real       Vettore delle pressioni                        (hPa) 
!  TT     real       Vettore delle Temperature dell'aria            (K.) 
!  TD     real       Vettore delle Temperature di rugiada           (K.) 
!  NP     integer    Numero di dati 
!  rhstaz real       Altezza della stazione S.L.M.                  (m)
!
!  output:
!  p_zero real       Livello di pressione dello zero termico        (hPa)
!  h_zero real       Altezza dello zero termico                     (m)
!---------------------------------------------------------------------------

  integer,intent(in)::np
  real,dimension(np)::pt,tt,td
  real,intent(in)::rhstaz
  real,intent(out)::p_zero,h_zero
  real::t_zero,pres

  if(np < 2 .or. pt(1) <  pt(np) )then !filtra input invalidi
     h_zero=rmiss
     p_zero=rmiss
     return
  end if

  t_zero=-999.9  !valore piccolo
  pres=400.
  do while( t_zero <= t0c )
     pres=pres+1    
     t_zero=trpt(pt,tt,np,pres)

     if( pres > pt(1) )then
        p_zero=rmiss
        h_zero=rmiss
        return
     end if
  end do

  p_zero=pres-1                        !livello pressione zero termico
  h_zero=z(p_zero,pt,tt,td,np)+rhstaz  !altezza zero termico

return

end subroutine zero_termico


!----------------------------------------------------------------------
 
real function Z(Plev,P,T,TD,N)  

! Calcola l' altezza geopotenziale al livello di pressione "PT" 
! 
! Uso : x = Z (Plev,P,T,TD,NT) 
! 
!  Input :
!  Plev real   livello per il quale si vuole calcolare l'altezza (hPa) 
!  P    real   Vettore delle pressioni                           (hPa) 
!  T    real   Vettore delle Temperature dell'aria               (K.) 
!  TD   real   Vettore delle Temperature di rugiada              (K.) 
!  NT   I*4    Numero di dati 
! 
!  Output : 
!  Z      real   Altezza del Geopotenziale                       (mgp) 
!  Z =  rmiss  Se non e' possibile calcolarla 
!----------------------------------------------------------------------
 
  integer,intent(in)::n
  real,intent(in),dimension(n)::t,p,td
  real,intent(in)::plev
  real::a1,a2
  integer::i,j

  z = 0.0                   !inizializzo l'altezza 

  if(n == 0 .or. plev <  P(N) .or. plev > p(1) )then !filtra input invalidi
     z=rmiss
     return
  end if
      
  if(plev == p(1) )then
     z=0
     return
  end if

  i =  0
  j =  i+1
  do while (plev < p(i+1) )
     i = i+1 
     j  = i+1 
     if(Plev  >= P(j))then
        a1=t(j)*(1.+.0006078*W(td(j),p(j))) 
        a2=t(i)*(1.+.0006078*W(td(i),p(i))) 
        z = z+14.64285*(a1+a2)*(alog(p(i)/plev )) 
        return
     end if

     a1=t(J)*(1.+.0006078*W(td(j),p(j))) 
     a2=t(I)*(1.+.0006078*W(td(I),p(i))) 
     z = z+14.64285*(a1+a2)*(alog(p(i)/p(j))) 
  end do

  a1=t(j)*(1.+.0006078*W(td(j),p(j))) 
  a2=t(i)*(1.+.0006078*W(td(i),p(i))) 
  z =z+14.64285*(a1+a2)*(alog(p(i)/plev )) 
	
  return 
end function Z
 

!-----------------------------------------------------------------------------
!
!                                  VENTO 
!
!-----------------------------------------------------------------------------

elemental subroutine UV(DD,FF,U,V)   

!  Calcola le componenti U e V  del vento 
! 
!      Uso : 
!                  CALL UV (DD,FF,U,V) 
! 
!      Input : 
!  DD   real   Direzione del vento                  (Gradi sessag, 0 = nord) 
!  FF   real   Velocita` del vento                  
! 
!      Output : 
!  U   real   Componente lungo i paralleli del vento orientata da 
!            West a Est
!  V   real   Componente lungo i meridiani del vento orientata da 
!            Sud a Nord
! 
!  U e V =  rmiss Se non e' possibile calcolarle 	
!
!---------------------------------------------------------------------	
  real,intent(in)::dd,ff
  real,intent(out)::u,v
  real::ar

  if(c_e(dd) .and. c_e(ff))then

    AR=dd*degrad
                                !scambio seno e coseno per rotazione 90 gradi
    U=-ff*sin(AR) 
    V=-ff*cos(AR) 

  else

    U=rmiss 
    V=rmiss

  end if
    
  return 
end subroutine UV

!--------------------------------------------------------------------------


elemental subroutine UVWIND(UBAR,VBAR,SPEED,DIREC)     
! Ricostruisce direzione e velocita` del vento dalle componenti cartesiane 
! 
! Uso :  CALL UVWIND (U,V,FF,DD) 
! 
! Input : 
! U   real   Componente lungo i paralleli del vento orientata da West a Est 
! V   real   Componente lungo i meridiani del vento orientata da Sud a Nord 
! 
! Output : 
! FF   real   Velocita` del vento
! DD   real   Direzione del vento    (Gradi sessag, 0 calma;  360 = nord) 
! 
! DD e FF =  rmiss se non e' possibile calcolarle.  
!------------------------------------------------------------------------------

  real,intent(in)::ubar,vbar
  real,intent(out)::speed,direc

  if(c_e(UBAR) .and. c_e(VBAR))then

    if(ubar == 0. .and. vbar == 0.) then
      speed =  0.
      direc =  0.
      return
    end if

    speed=SQRT(UBAR**2+VBAR**2) 

                                !scambio seno e coseno per rotazione 90 gradi
    direc=ATAN2(-UBAR,-VBAR)*raddeg
    direc=modulo(direc,360.)

    if(direc == 0.)direc=360.0 

  else

    speed=rmiss 
    direc=rmiss

  end if
 
  return 
 
end subroutine UVWIND

!------------------------------------------------------------------------

real function AVVEZ (P1,DD1,FF1,P2,DD2,FF2,ALAT)   
!
! costanti :
! Degrad    : conversione da gradi a radianti
! ra     : costante dei gas per aria secca
!
! calcola l'avvezione termica in kelvin/ora per uno strato isobarico
! input: 
! P1  pressione livello isobarico inferiore
! P2  pressione livello isobarico superiore
! DD1 direzione del vento del livello isobarico inferiore
! DD2 direzione del vento del livello isobarico superiore
! FF1 forza del vento in m/s del livello isobarico inferiore
! FF2 forza del vento in m/s del livello isobarico superiore
! ALAT latitudine del punto
!
! output :
! AVVEZ  qavvezione termica nello strato isobarico in K/h
!------------------------------------------------------------------------------

  real,intent(in)::p1,dd1,ff1,p2,dd2,ff2,alat  !input
  real::u1,u2,v1,v2,uvm,uvt,vvt,vvm
  real::grad,alamda,a,b

  if ( c_e(P1) .and. c_e(DD1) .and. c_e(FF1) .and.&
   c_e(P2) .and. c_e(DD2) .and. c_e(FF2) .and. c_e(ALAT)) then

    ALAMDA=2*OMEARTH*sin(ALAT*DEGRAD)   !Coriolis 
    A=ALAMDA/RD 
    B=P1/P2 
    B=log(B) 
 
! Calcolo le componenti del vento U e V 
    call UV(dd1,ff1,U1,V1) 
    call UV(dd2,ff2,U2,V2) 
  
! Calcolo le componenti del vento termico 
    UVT=U2-U1 
    VVT=V2-V1 
 
! Calcolo le componenti del vento medio 
    UVM=(U2+U1)/2.
    VVM=(V2+V1)/2.
 
! Calcolo il valore del termine avvettivo 
    GRAD=UVT*VVM-VVT*UVM 
    AVVEZ=A/B*GRAD 
    AVVEZ=AVVEZ*3600.            ! in Kelvin/ore 

  else

     AVVEZ=rmiss 

  end if

  return
end function AVVEZ

!------------------------------------------------------------------------------
!
!                 INTERPOLAZIONE VERTICALE
!
!------------------------------------------------------------------------------

subroutine MNWIND(P,SPD,DIR,NW,P1,P2,SPEED,DIREC)    

!  Calcola la media del vento pesata col logaritmo naturale della pressione 
!  nello strato compreso fra le pressioni P1 e P2 
! 
!  Uso : CALL  = MNWIND(P,FF,DD,NW,P1,P2,F,D) 
! 
!  Input : 
!  P      real      Vettore delle pressioni                         (hPa) 
!  FF     real      Vettore delle intensita`                        (m/sec) 
!  DD     real      Vettore delle direzioni                         (Gradi) 
!  NW     integer   Numero di elementi. 
!  P1     real      Valore della pressione al livello inferiore     (hPa) 
!  P2     real      Valore della pressione al livello superiore     (hPa) 
! 
!  P1 maggiore di P2 
! 
!  Output : 
!  F                real   Intensita` media                         (m/sec) 
!  D                real   Direzione  media                         (Gradi) 
! 
!  F e D = rmiss Se non e' possibile calcolarle. 
!------------------------------------------------------------------------------

  integer,intent(in)::nw                          !input
  real,intent(in),dimension(nw)::P,SPD,DIR        !input
  real,dimension(nw)::u,v
  real,intent(out)::speed,direc                   !output
  real::spds,dirs,p1,p2,ubar,vbar
  integer::i

  if(P1 >  P(1) .or. P2 < P(NW))then 
     SPDS=rmiss 
     DIRS=rmiss 
     return 
  end if

  do I=1,NW 
     call UV(DIR(I),SPD(I),U(I),V(I)) 
  end do

  UBAR=PKAVR(P,U,NW,P1,P2) 
  VBAR=PKAVR(P,V,NW,P1,P2) 
  
  call UVWIND(UBAR,VBAR,speed,direc) 

  return 
end subroutine MNWIND

!----------------------------------------------------------------------------

real function PTRP(P,A,N,AX) 

!
! Calcola il valore interpolato della pressione in un punto 
! di un profilo di un parametro termodinamico. 
! 
! Formula utilizzata : 
! 
!                 ln(P(i))*(AX-A(i+1))-ln(P(i+1))*(AX-A(i))) 
! PTRP = EXP --------------------------------------------------------- 
!                                 (A(i)-A(i+1)) 
! 
! Uso :   x = PTRP (P,A,NT,AX) 
! 
! Input : 
! P            real   Vettore delle pressioni                    (hPa) 
! A            real   Vettore del parametro 
! NT        integer   Numero il elementi. 
! AX           real   Valore del parametro al quale interpolare 
! 
! Output : 
! PTRP         real   Valore della pressione interpolata         (hPa) 
! PTRP =     rmiss   se non e' possibile calcolarla.  
!------------------------------------------------------------------------------

  integer,intent(in)::n
  real,intent(in),dimension(n)::P,A
  real,intent(in)::ax
  real::rdir,y1,y2
  integer::i

  PTRP=rmiss 
  rdir=sign(1.0,A(N)-A(1)) 
  if(ax*rdir > A(N)*rdir)return 
      
  do  I=1,N-1 
     if(ax*rdir <  A(I)*rdir)then
        PTRP=rmiss 
        return
     end if

     if(ax*rdir > A(I+1)*rdir)cycle 
     Y1=alog(P(I)) 
     Y2=alog(P(I+1)) 
     PTRP=(Y1*(AX-A(I+1))-Y2*(AX-A(I)))/(A(I)-A(I+1)) 
     PTRP=exp(PTRP) 
     return 
  end do


end function PTRP

!------------------------------------------------------------------------------

subroutine TRPW(P,SPD,DIR,N,TP,S,D)     

!  Esegue l'interpolazione del vento 
!  pesata col logaritmo naturale della pressione alla pressione PRES 
! 
!  Uso : CALL TRPW (P,FF,DD,NW,PRES,F,D) 
! 
!  Input : 
!  P          real   Vettore delle pressioni                       (hPa) 
!  FF         real   Vettore delle intensita` del vento            (m/sec) 
!  DD         real   Vettore delle direzioni  del vento            (Gradi) 
!  NW      integer   Numero il elementi. 
!  PRES       real   Valore della pressione alla quale interpolare (hPa) 
! 
!  Output : 
!  F          real   Intensita` del vento  interpolata             (m/sec) 
!  D          real   Direzione del vento   interpolata             (gradi) 
! 
!  F e D =  rmiss Se non e' possibile calcolarle.  
!------------------------------------------------------------------------------

  integer,intent(in)::n                              !input
  real,dimension(n)::P,SPD,DIR                       !input
  real::s,d                                          !output
  real::u,v,u1,u2,v1,v2,a1,a2,x1,x2,x,tp,a,cnp
  integer::i,manca,k

  CNP(A)=AMOD((450.-A),360.) 
  
  if(N <= 1 .or. TP < P(N) .or. TP > P(1) )then
     S=rmiss 
     D=rmiss
     return
  end if
 
  do k = 1,n
     if(spd(k) == rmiss .or. dir(k) == rmiss)manca=manca+1
  end do

  if(manca > n/2 )then
     S=rmiss 
     D=rmiss
     return
  end if

  do  I=1,N-1

     if(TP >  P(I))then 
        S=rmiss 
        D=rmiss 
        return
     end if

     if(TP <  P(I+1))cycle
 
     if(DIR(I) >  400.0 .OR. DIR(I+1) > 400.0)then
        S=rmiss 
        D=rmiss 
        return
     end if

     X1=alog(P(I)) 
     X2=alog(P(I+1)) 
     X=alog(TP) 
     A1=CNP(DIR(I))*DEGRAD 
     A2=CNP(DIR(I+1))*DEGRAD 
     U1=SPD(I)*cos(A1) 
     U2=SPD(I+1)*cos(A2) 
     V1=SPD(I)*sin(A1) 
     V2=SPD(I+1)*sin(A2) 
     U=(U1*(X-X2)-U2*(X-X1))/(X1-X2) 
     V=(V1*(X-X2)-V2*(X-X1))/(X1-X2) 
     S=SQRT(U*U+V*V) 
     D=360. 
     if(S  <  0.01)return
              
     D=CNP( RADDEG*ATAN2(V,U)) 
     if(D <=  0.1)D=360. 

     return

   end do

   return 

 end subroutine TRPW

!----------------------------------------------------------------------------

real function PKAVR(PRES,THR,N,P1,P2)   
 
!  Calcola la media pesata col logaritmo naturale della pressione 
!  di un parametro termodinamico nello strato compreso fra le 
!  pressioni P2 e P1 con   P1 maggiore di P2. 
! 
!  Uso :    x =  PKAVR (P,A,NT,P1,P2) 
! 
!  Input : 
!  P           real   Vettore delle pressioni                       (hPa) 
!  A           real   Vettore del parametro 
!  NT       integer   Numero di elementi. 
!  P1          real   Valore della pressione al livello inferiore   (hPa) 
!  P2          real   Valore della pressione al livello superiore   (hPa) 
! 
!  Output : 
!  PKAVR       real   Valore della media pesata 
!  PKAVR =   rmiss   Se non e' possibile calcolarla. 
!------------------------------------------------------------------------------

  integer,intent(in)::n
  real,intent(in),dimension(n)::pres,THR
  real,dimension(200)::p,a
  real,intent(in)::p1,p2
  real::wbar
  integer::k,i,j,l

  if(p1 > pres(1) .or. p2 < pres(n) )then
     PKAVR=rmiss 
     return
  end if

  k=0 
  PKAVR=0.0 
  wbar=0.0 
  k=k+1 
  p(k)=p1 
  a(k)=TRPT(pres,thr,n,p1) 

  if (p1 == p2) then
     PKAVR=a(k)
     return
  endif

  do i=1,N 
     if(pres(i) <= p2)exit
     k=k+1 
     p(k)=pres(i) 
     a(k)=thr(i) 
  end do

  k=k+1 
  p(k)=P2 
  a(k)=TRPT(pres,thr,n,p2) 
  j=k-1 
  do i=1,j 
     l=i+1 
     wbar=(a(i)+a(l))*alog(p(i)/p(l))+wbar 
  end do

  wbar=wbar/(2.*alog(p(1)/p(k))) 
  PKAVR=wbar 

  return 
end function PKAVR

!-----------------------------------------------------------------------
!
!                       INDICI TEMPORALESCHI
!
!-----------------------------------------------------------------------

real function SI_K  (PT,T,TD,NT) 
!
! Calcola l'indice temporalesco K-Index 
!
!-----------------------------------------------------------------------

  integer,intent(in)::nt
  real,intent(in),dimension(nt)::pt,t,td 
  real::t850,t700,t500,td850,td700
  real::tmentd

  t850=  TRPT(pt,t,nt,850.) 
  t700=  TRPT(pt,t,nt,700.) 
  t500=  TRPT(pt,t,nt,500.) 
  td850= TRPT(pt,td,nt,850.) 
  td700= TRPT(pt,td,nt,700.) 
 
  if(T850 < 0 .or. T500 <0  .or. TD850 < 0 .or. TD700 < 0)then 
     SI_K=rmiss 
     return 
  end if
 
  t850  = t850  - t0c
  t500  = t500  - t0c 
  t700  = t700  - t0c 
  td850 = td850 - t0c 
  td700 = td700 - t0c 
  tmentd = abs(t700-td700) 
  SI_K  = t850 - t500 + td850 - tmentd 

  return 
end function SI_K

!-------------------------------------------------------------------------

real function SI_LI (PT,T,TD,NT) 

 
!  Calcola l'indice di stabilita' LIFTED-INDEX definito dalla 
!  differenza fra 
!  la temperatura dell'aria a 500 hPa. e la temperatura a 500 hPa. 
!  sull'adiabatica satura passante per il livello di condensazione 
!  convettiva secondo la formula:  
!  si_LI = T500 - TAS500 
! 
!  Dove :  T500   e` la Temperatura dell'aria a 500 hPa.            (C.) 
!          TAS500 e` la Temperatura a 500 hPa. sull'adiabatica 
!                    satura che parte dal Livello di Condesazione 
!                    Forzata                                       (C.) 
! 
!  Uso :   x = SI_LI (PT,T,TD,NT) 
! 
!  Input : 
!  PT           real  Vettore delle Pressioni                       (hPa) 
!  T            real  Vettore delle Temperature                     (K.) 
!  TD           real  Vettore delle Temperature di rugiada          (K.) 
!  NT        integer  Numero di dati 
! 
!  Output : 
!  SI_LI       real  Indice di stabilita` LIFTED-INDEX 
!  SI_LI =   rmiss  se non e' possibile calcolarlo. 
!
!-------------------------------------------------------------------------

  integer,intent(in)::nt
  REAL,intent(in),dimension(nt)::pt,t,td
  real,dimension(nt)::aw
  real::pm,plift,tdew,wbar,t500,tsa500,tlcl,tas
  integer::k

  pm=pt(1)-50. 
  if(pm <  pt(nt))then
     SI_LI=rmiss
     return
  end if
  
  do k=1,nt
     aw(k)= W(td(k),pt(k))   !rapporto di mescolanza
  end do

  wbar=   PKAVR (pt,aw,nt,pt(1),pt(1)-50.) !wmed primi 50 hPa
  tdew=     TMR (wbar,pt(1))
  plift=   ALCL (tdew,t(1),pt(1)) 
  tlcl=     TMR (wbar,plift)                !Temperatura al LCL
  tas=       OS (tlcl,plift) 
  tsa500 =  TSA (tas,500.) 
  t500   = TRPT (pt,t,nt,500.) 

  if(t500 <  0 .or. tsa500 < 0 )then
     SI_LI=rmiss
     return
  end if

  SI_LI= t500 - tsa500 


  return  
end function SI_LI
 
!-----------------------------------------------------------------------------

real function SI_SW(PT,T,TD,NT,PW,FF,DD,NW) 

!  Calcola l'indice di stabilita' di S.W.E.A.T. 
!  definito dalla formula: 
! 
! SI_SW = 12.*TD850+20.*(T850+TD850-(2*T500)-49.)+2.* 
!                FF850+FF500+125.*((sin(D500-D850)*0.0174533)+0.2) 
! 
! Dove : TD850  e` la Temperatura di rugiada a 850 hPa.    (C.) 
!        T850   e` la Temperatura dell'aria a 850 hPa.     (C.) 
!        T500   e` la Temperatura dell'aria a 500 hPa.     (C.) 
!        FF850  e` la Velocita` del vento a 850 hPa.       (m/sec) 
!        FF500  e` la Velocita` del vento a 500 hPa.       (m/sec) 
!        D850   e` la Direzione del vento a 850 hPa.       (Gr.) 
!        D500   e` la Direzione del vento a 500 hPa.       (Gr.) 
! 
!  Uso :  x = SI_SW (PT,T,TD,NT,PW,FF,DD,NW) 
! 
!  Input : 
!  PT   real  Vettore delle Pressioni dati Termodinamici  (hPa) 
!  T    real  Vettore delle Temperature                   (K.) 
!  TD   real  Vettore delle Temperature di rugiada        (K.) 
!  NT   integer  Numero di dati Termodinamici 
!  PW   real  Vettore delle Pressioni dati vento          (hPa) 
!  DD   real  Vettore delle Direzioni                     (hPa) 
!  FF   real  Vettore delle Velocita`                     (m/sec) 
!  NW   integer  Numero di dati Vento 
! 
!  Output : 
!  SI_SW  real       Indice di stabilita` di S.W.E.A.T. 
!  SI_SW =  rmiss  Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  integer,intent(in)::nt,nw
  real,intent(in),dimension(nt)::pt,t,td
  real,intent(in),dimension(nw)::pw,dd,ff
  real::t850,t500,td850,rtot,rs
  real::dd850,dd500,ff500,ff850,rd85

  T850=  TRPT(pt,t,nt,850.)  - t0c 
  T500=  TRPT(pt,t,nt,500.)  - t0c 
  td850= TRPT(pt,td,nt,850.) - t0c 

  call TRPW(pw,ff,dd,nw,850.,ff850,dd850) 
  call TRPW(pw,ff,dd,nw,500.,ff500,dd500) 

  if(t850 < -900 .or. t500 < -900 .or. td850 < -900.or. &
       &ff850 < 0 .or. ff500 < 0 .or. dd850 < 0 .or. dd500 < 0)then
    SI_SW=rmiss
    return
  end if

  ff850=ff850*convff 
  ff500=ff850*convff 
 
  rtot=(t850+td850-2.*T500)-49. 
  if(rtot < 0)rtot=0. 
 
  rs=((sin(dd500-dd850))+0.2) 
 
  if(dd850 >= 130 .and. dd850 <= 250)rs=0. 
  if(dd500 >= 210 .and. dd500 <= 310)rs=0. 
  if((dd500 - dd850) > 0)rs=0 
  if(dd850 < 15 .or. dd500 < 15)rs=0 
 
  if(td850 < 0)then 
     rd85=0. 
  else 
     rd85=12.*td850 
  end if
 
  SI_SW=rd85+20.*rtot+2*ff850+ff500+125.*rs 
 
  return 

end function si_SW

!----------------------------------------------------------------------------

real function TEP_IDX (PT,T,TD,NT)
!  Calcola l'indice di stabilita' definito dalla differenza fra 
!  le temperature equivalenti potenziali a 500 e 850 hPa. 
!  secondo la formula: 
! 
!        TEP_IDX = TEP500 - TEP850 
! 
!  Dove : TEP500 e` la Temp. eq. pot. a 500 hPa.               (K.) 
!         TEP850 e` la Temp. eq. pot. a 850 hPa.               (K.) 
! 
!  Uso : x = TEP_IDX (PT,T,TD,NT) 
! 
!  Input : 
!  PT   real  Vettore delle Pressioni dati Termodinamici      (hPa) 
!  T    real  Vettore delle Temperature                       (K.) 
!  TD   real  Vettore delle Temperature di rugiada            (K.) 
!  NT   integer  Numero di dati Termo 
! 
!  Output : 
!  TEP_IDX       real  Indice di stabilita` 
!  TEP_IDX =  rmiss  Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  integer,intent(in)::nt
  real,intent(in),dimension(nt)::pt,t,td 
  real::t850,t500,td850,td500,tep850,tep500

  t850   = TRPT (pt,t,nt,850.) 
  t500   = TRPT (pt,t,nt,500.) 
  td850  = TRPT (pt,td,nt,850.) 
  td500  = TRPT (pt,td,nt,500.) 
  tep850 =   OE (td850,T850,850.) 
  tep500 =   OE (td500,T500,500.) 
  
  if(tep500 <  0 .or. tep850 < 0)then
     TEP_IDX=rmiss 
     return
  end if

  TEP_IDX=tep500-tep850
 
  return  
end function TEP_IDX

!----------------------------------------------------------------------------


real function SI_SH(PT, T, TD, NT) 

!  Calcola l'indice di stabilita` di Showalter 
!  definito dalla formula:
! 
!  si_SH  = T500 - TSA500 
! 
!  Dove : T500   e` la Temperatura dell'aria a 500 hPa.          (K.) 
!         TSA500 e` la Temperatura a 500 hPa. dell'adiabatica 
!                   satura passante per il livello di CCL da 850 hPa  (K.) 
! 
!  Uso : x = si_SH  (PT,T,TD,NT) 
! 
!  Input : 
!  PT           real     Vettore delle Pressioni dati Termo     (hPa) 
!  T            real     Vettore delle Temperature              (K.) 
!  TD           real     Vettore delle Temperature di rugiada   (K.) 
!  NT        integer     Numero di dati Termo 
! 
!  Output : 
!  si_SH      real      Indice di stabilita` di Showalter
!  si_SH  =  rmiss     Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  integer,intent(in)::nt
  real,intent(in),dimension(nt)::pt,t,td 
  real::td850,t850,w850,tccl,tas,tsa500,t500,pccl

  if(pt(1) < pt(nt))then
     SI_SH=rmiss 
     return
  end if

  td850  = TRPT (pt,td,nt,850.)     !td a 850 hPa
  t850   = TRPT (pt,t,nt,850.)      !t  a 850 hPa
  w850   =    W (td850,850.)        !rapporto di mescolanza a 850 hPa
  pccl   = ALCL (td850,t850,850.)
  tccl   =  TMR (w850,pccl) 
  tas    =   OS (tccl,pccl)
  tsa500 =  TSA (tas,500.) 
  t500=    TRPT (pt,t,nt,500.) 

  if(t500 < 0 .or. tsa500 < 0)then 
     SI_SH=rmiss 
     return
  end if

  SI_SH= t500 - tsa500 

  return 
end function SI_SH

!----------------------------------------------------------------------------

real function SI_U(PT,T,TD,NT) 

!  Calcola l'indice di stabilita' definito dalla media delle umidita` 
!  relative dei livelli 500-700-850 secondo la formula: 
! 
!  SI_U =   (U850 + U700 + U500 ) / 3. 
! 
!  Dove : U850 e` l'Umidita` relativa a 850 hPa                (%) 
!         U700 e` l'Umidita` relativa a 700 hPa                (%) 
!         U500 e` l'Umidita` relativa a 500 hPa                (%) 
! 
!  Uso :  x = SI_U (PT,T,TD,NT) 
! 
!  Input : 
!  PT         real     Vettore delle Pressioni dati Termo      (hPa) 
!  T          real     Vettore delle Temperature               (K.) 
!  TD         real     Vettore delle Temperature di rugiada    (K.) 
!  NT      integer     Numero di dati Termo 
! 
!  Output : 
!  SI_U      real     Indice di stabilita` 
!  SI_U =  rmiss     Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  integer,intent(in)::nt
  real,intent(in),dimension(nt)::pt,t,td 
  real::t850,t700,t500,td850,td700,td500,u850,u700,u500
  
  t850  = TRPT(pt,t,nt,850.) 
  t700  = TRPT(pt,t,nt,700.) 
  t500  = TRPT(pt,t,nt,500.) 
  td850 = TRPT(pt,td,nt,850.) 
  td700 = TRPT(pt,td,nt,700.) 
  td500 = TRPT(pt,td,nt,500.) 
  u850  = FR  (t850,td850) 
  u700  = FR  (t700,td700) 
  u500  = FR  (t500,td500) 
 
  if(u500 < 0 .or. u700 <  0 .or. u850 < 0)then
     SI_U=rmiss 
     return
  end if

  SI_U=(u850+u700+u500)/3. 

  return 
end function SI_U

!----------------------------------------------------------

real function SI_TT (pt,t,td,nt )

!   Calcola l'indice di stabilità total-total
!
!    PT = vettore pressioni              [hPa]
!    T  = Vettore temperature            [°K]
!    TD = Vettore Temperature di rugiada [°K]
!
!    VT = Vertical Total
!    CTT = Cross Total
!    TT = indice Total-Total
!_____________________________________________________________

  integer,intent(in)::nt
  real,intent(in),dimension(nt)::pt,t,td
  real::t850,t500,td850,ctt,vt

  t850  = TRPT(pt,t,nt,850.) 
  t500  = TRPT(pt,t,nt,500.) 
  td850 = TRPT(pt,td,nt,850.) 

  if ( t850 == rmiss .or. t500 == rmiss&
       & .or. td850 == rmiss )then
     SI_TT=rmiss
     return
  end if

  vt =  t850  - t500
  ctt =  td850 - t500

  SI_TT=vt+ctt

  return
end function SI_TT

!_________________________________________________________________


!> stima la classe di stabilita' a partire da velocita' del vento, 
!! radiazione solare incidente e radiazione infrarossa netta.
!! Per le ore diurne, usa Bowen 1983 (vento e radiazione solare)
!! Per le ore notturne, usa Reuter 1970 modificato (vento e radiazione IR)
elemental real function pgt (ff,swd,lwb)
real,intent(in) ::  ff,swd,lwb

integer :: cl_ff,cl_rad,kc

! Tabelle per calcolo PGT diurna (secondo Bowen 1983)
INTEGER, PARAMETER :: ncrd = 4
INTEGER, PARAMETER :: ncfd = 5
REAL, PARAMETER :: class_rad_day(0:ncrd) = &
  (/HUGE(0.),925.,675.,175.,-HUGE(0.)/)
REAL, PARAMETER :: class_ff_day(0:ncfd) = &
  (/-HUGE(0.),2.,3.,5.,6.,HUGE(0.)/)
INTEGER, PARAMETER :: ipgt_day(ncrd,ncfd) = RESHAPE((/ &
  1,1,2,4, &
  1,2,3,4, &
  2,2,3,4, &
  3,3,4,4, &
  3,4,4,4  &
  /),(/ncrd,ncfd/))  

! Tabelle per calcolo PGT notturna (secondo Reuter 1970)
INTEGER, PARAMETER :: ncrn = 3
INTEGER, PARAMETER :: ncfn = 6
REAL, PARAMETER :: class_rad_nig(0:ncrn) = &
  (/-HUGE(0.),-70.,-6.5,HUGE(0.)/)
REAL, PARAMETER :: class_ff_nig(0:ncfn) = &
  (/0.,2.,3.,4.,5.,7.,HUGE(0.)/)
INTEGER, PARAMETER :: ipgt_nig(ncrn,ncfn) = RESHAPE((/ &
  6,6,4, &
  6,5,4, &
  5,4,4, &
  5,4,4, &
  5,4,4, &
  4,4,4  &
  /),(/ncrn,ncfn/))  

! Se ci sono dati mancanti in input, il calcolo e' impossibile
if(.not. c_e(ff) .or. .not. c_e(swd) .or. .not. c_e(lwb) )then 
  pgt=rmiss 
  return
end if

! Calcolo la classe di stabilita'
cl_ff = 0
cl_rad = 0

if (swd > 5.) then                                      ! condizioni diurne
  do kc = 1,ncfd
    if (ff > class_ff_day(kc-1) .AND. ff <= class_ff_day(kc)) cl_ff = kc
  enddo
  do kc = 1,ncrd
    if (swd >= class_rad_day(kc) .AND. swd < class_rad_day(kc-1)) cl_rad = kc
  enddo
  pgt = REAL(ipgt_day(cl_rad,cl_ff))

else                                                    ! condizioni notturne
  do kc = 1,ncfn
    if (ff > class_ff_nig(kc-1) .AND. ff <= class_ff_nig(kc)) cl_ff = kc
  enddo
  do kc = 1,ncrn
    if (lwb > class_rad_nig(kc-1) .AND. lwb <= class_rad_nig(kc)) cl_rad = kc
  enddo
  pgt = REAL(ipgt_nig(cl_rad,cl_ff))

endif

return
end function PGT



!> short wave radiation budget from incoming short wave radiation and albedo
elemental real function swbudget (swd,alb)
real,intent(in) :: swd !< incoming short wave radiation (W/m**2)
real,intent(in) :: alb !< albedo (%)

if (c_e(alb) .and. c_e(swd)) then
  swbudget = swd*(1. - max(min(alb,100.),0.) /100.)
else
  swbudget=rmiss
end if


end function swbudget


!> incoming short wave radiation from short wave radiation budget and albedo
elemental real function swdown (swb,alb)
real,intent(in) :: swb !< incoming short wave radiation (W/m**2)
real,intent(in) :: alb !< albedo (%)

if (c_e(alb) .and. c_e(swb) .and. alb < 100.) then
  swdown = swb/(1.- max(min(alb,100.),0.)/100.)
else
  swdown=rmiss
end if

end function swdown


!> Omega from temperature, pressure and vertical velocity; pressure
!! tendency term and humidity in density are neglected.
ELEMENTAL REAL FUNCTION omega_simple(t, p, w)
REAL,intent(in) :: t !< temperature (K)
REAL,intent(in) :: p !< pressure (Pa)
REAL,INTENT(in) :: w !< geometrical vertical velocity (m/s)

IF (c_e(t) .AND. c_e(p) .AND. c_e(w)) THEN
  omega_simple = -w*rd*t*gearth/p ! check the units
ELSE
  omega_simple = rmiss
ENDIF

END FUNCTION omega_simple


end module termolib

