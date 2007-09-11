

module termolib

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


!------------------------------------------------------------------------------
!
!                 TEMPERATURE CONVENZIONALI
!
!------------------------------------------------------------------------------

real function OE(TDS,TS,PS)

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
!  OE =   -999.9   Se non e' possibile calcolarla 
!
!------------------------------------------------------------------------------

  implicit none
  real::tds,ts,ps
  real::tw,os      !funzioni di termolib
  real::atw        !variabile di lavoro

  if(TDS < 0 .or. TS < 0 .or. PS < 0)then 
     OE=-999.9 
     return 
  end if

  ATW=TW(TDS,TS,PS) 
  OE=OS(ATW,PS) 

  return 
end function OE

!------------------------------------------------------------------------------

real function O(T,P)

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
!  P      real    Pressione aria                       (hPa) 
! 
!  Output : 
!  O      real    Temperatura potenziale               (K.) 
!  O =  -999.9    Se non e' possibile calcolarla 
!
!------------------------------------------------------------------------------

  implicit none
  real::t,p

  if(T < 0 .or. P < 0)then 
     O=-999.9 
     return 
  end if

  O=T*((1000./P)**.286) 

  return 
end function O

!------------------------------------------------------------------------------

real function OW(TDS,TS,PS)
 
!  Calcola la Temperatura Potenziale di Bulbo Bagnato in (K) 
!
!  Uso :  x = OW (TD,T,P) 
! 
!  Input : 
!  TD      real   Temperatura di rugiada                 (K.) 
!  T       real   Temperatura dell'aria                  (K.) 
!  P       real   Pressione aria                         (hPa) 
! 
!  Output : 
!  OW      real   Temperatura di Bulbo Bagnato           (K.) 
!  OW =  -999.9   Se non e' possibile calcolarla 	
!
!------------------------------------------------------------------------------

  implicit none
  real::tds,ts,ps      !input
  real::atw,aos        !variabili di lavoro
  real::tw,os,tsa      !funzioni di termolib

  if(TDS < 0 .or. TS < 0 .or. PS < 0)then 
     OW=-999.9 
     return 
  end if

  ATW=TW(TDS,TS,PS) 
  AOS = OS(ATW,PS) 
  OW  = TSA(AOS,1000.) 

  return 
end function OW

!------------------------------------------------------------------------------

real function  OS(T,P)

!  Calcola la Temperatura Adiabatica Satura 
!  caratterizzata dalla temperatura T e dalla pressione P 
! 
!  Uso :  x = OS (T,P) 
! 
!  Input : 
!  T       real   Temperatura dell'aria                   (K.) 
!  P       real   Pressione aria                          (hPa) 
! 
!  Output : 
!  OS       real  Temperatura Adiabatica Satura           (K.) 
!  OS =   -999.9  se non e' possibile calcolarla  
!------------------------------------------------------------------------------

  implicit none
  real::t,p
  real::w         !functio di termolib - calcola il rapporto di mescolanza

  if(T <  0 .or. P < 0)then 
     OS=-999.9 
     return 
  end if

  OS=T*((1000./P)**.286)/(exp(-2.6518986*W(T,P)/T)) 

  return 
end function OS
 
!------------------------------------------------------------------------------

real function  TE(TDS,TS,PS)

!  Calcola la Temperatura Equivalente Isobarica espressa in gradi Kelvin 
! 
!  Uso :  x = TE (TD,T,P) 
! 
!  Input : 
!  TD   real   Temperatura di rugiada                      (K.) 
!  T    real   Temperatura dell'aria                       (K.) 
!  P    real   Pressione aria                              (hPa) 
! 
!  Output : 
!  TE     real   Temperatura Eqivalente  Isobarica         (K.) 
!  TE =  -999.9 Se non e' possibile calcolarla 
!------------------------------------------------------------------------------

  implicit none
  real::tds,ts,ps       !input
  real::oe,tda          !funzioni di termolib
  real::aoe             !variabile di lavoro

  if(TDS < 0 .or. TS <  0 .or. PS < 0)then 
     TE=-999.9 
     return 
  end if

  AOE=OE(TDS,TS,PS) 
  TE=TDA(AOE,PS) 

  return 
end function TE

!------------------------------------------------------------------------------

real function TRUG(UMID,T)

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
!  TRUG =   -999.9  Se non e' possibile calcolarla 
!-----------------------------------------------------------------------------

  real::abz,d,c,b
  parameter(ABZ=273.16,D=237.3,C=17.2693882,B=6.1078)
  real::umid,t                                          !input
  real::esat                                            !fuction termolib
  real::es,e                                            !variabili di lavoro

  if(UMID <  0 .or. t < 0)then
     TRUG=-999.9 
     return 
  end if

  ES=ESAT(T)                        ! >>> Calcolo la P.v.s 
  E=UMID/100.*ES                    ! >>> Calcolo la P.v. 
  TRUG=(D*log(E/B))/(C-log(E/B))    ! >>> Calcolo la T.d. 
  TRUG=TRUG+ABZ                     ! >>> Calcolo la T.d. in Kelvin 

  return 
end function TRUG
 
!----------------------------------------------------------------------------


real function  TW( TDS,TS,PS )

!  Calcola la Temperatura di Bulbo Bagnato isobarica, differisce sul
!  secondo decimale dalla temepratura di bulbo bagnato adiabatica, poichè
!  si satura con acqua immessa alla temperatura TS (temperatura di
!  saturazione, anzichè alla T. E' la stessa temperatura di bulbo bagnato
!  che si può calcolare graficamente su di un nomogramma termodinamico.
!  
!  Uso : x = TW (TD,T,P) 
! 
!  Input : 
!  TD   real  Temperatura di rugiada                        (K.) 
!  T    real  Temperatura dell'aria                         (K.) 
!  P    real  Pressione dell'aria                           (K.) 
! 
!  Output : 
!  TW       real  Temperatura di Bulbo Bagnato isobaria     (K.) 
!  TW =  -999.9  Se non e' possibile calcolarla  
!
!----------------------------------------------------------------------------

  implicit none
  real::tds,ts,ps            !input
  real::w,o,tda,os,tsa,tmr   !funzioni termolib
  real::aw,ao,pi,x,aos,ti    !variabili locali di lavoro
  integer::i

  if(TDS <  0 .or. TS < 0 .or. PS < 0)then 
     TW=-999.9 
     return 
  end if

  AW = W(TDS,PS) 
  AO = O(TS,PS) 
  PI =  PS 
!con 10 cicli l'umidità specifica media tra td e tw dovrebbe tendere a
!essere quella alla TW, se converge prima si esce dal ciclo 
  do    I =1,10 
     X  =  .02*( TMR(AW,PI) - TDA(AO,PI) ) 
     if ( abs(X) < 0.01  )then
        TI=TDA(AO,PI)
!trovato il punto di intersezione,calcoliamo l'adiab. satura che ci passa 
        AOS  =   OS(TI,PI) 
        TW   =  TSA( AOS,PS) 
        return
     end if
     PI = PI* ( 2.**(X)  ) 
  end do
     
  TI=TDA(AO,PI) 
 
!trovato il punto di intersezione,calcoliamo l'adiab. satura che ci passa
  AOS  =   OS(TI,PI) 
  TW   =  TSA( AOS,PS) 

  return 
end function TW

!--------------------------------------------------------------------------

real function TVIR(TDS,TS,PS)

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
!  P         real  Pressione dell' aria                     (hPa) 
! 
!  Output : 
!  TVIR      real  Temperatura virtuale                     (K.) 
!  TVIR =  -999.9  Se non e' possibile calcolarla 
!----------------------------------------------------------------------------
  implicit none
  real::tds,ts,ps
  real::uspec             !function termolib

  if(TDS <  0.or. TS <0 .or. PS < 0)then 
     TVIR=-999.9 
     return 
  end if

  TVIR=(1+0.00061*USPEC(TDS,PS))*TS 

  return 
end function TVIR

!--------------------------------------------------------------------------
!
!       UMIDITA'  E PRESSIONE DI VAPORE
!
!--------------------------------------------------------------------------


real function USPEC(TDS,PS)

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
!  USPEC =  -999.9  Se non e' possibile calcolarla.
!
!---------------------------------------------------------------------------

  implicit none
  real::eps
  parameter(eps=0.622)
  real::tds,ps
  real::esat              !function termolib

  if(TDS <  0 .or. PS < 0)then
     USPEC=-999.9
     return
  end if

  USPEC=EPS*(ESAT(TDS)/(PS-(1-EPS)*ESAT(TDS)))
  USPEC=USPEC*1000.

  return

end function USPEC

!--------------------------------------------------------------------------
 
real function FR (T,TD )

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
!    FR = -999.9   Se non e' possibile calcolarla. 
!---------------------------------------------------------------------------
  implicit none
  real::t,td
  real::esat

  if(T <= 0 .or. TD <=0 )then 
     FR=-999.9 
     return 
  end if
 
  FR  =  100.*( ESAT(TD)/ESAT(T) ) 
  return 
end function FR
 
!-----------------------------------------------------------------------------

real function W(TDS,PS) 

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
!  P       real  Pressione aria                              (hPa) 
! 
!  Output : 
!  W       real  Rapporto di mescolanza                      (gr/kg) 
!  W =   -999.9  Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------
  implicit none
  real::tds,ps
  real::esat
  real::x

  if(tds  < 0 .or. ps < 0)then 
     w=-999.9 
     return 
  end if

  x  = esat(tds) 
  w  =  622.*x/(ps-x) 

  return 
end function W

!-----------------------------------------------------------------

real function QTORELHUM(Q,P,T) 
 
! CALCOLA L'UMIDITA RELATIVA A PARTIRE DALLA UMIDITA' SPECIFICA 
! REF.: BAKER, MON.WEA.REV.,1983,111,PAG.328 E SEG. 
!----------------------------------------------------------------------------
  
  QTORELHUM=Q*(P-0.378*ESAT(T))/(0.622*ESAT(T)) 
  if (QTORELHUM  < 0.)QTORELHUM=0. 
  return 
end function QTORELHUM
 
!-----------------------------------------------------------------

 
!----------------------------------------------------------------- 

real function RELHUMTOQ(RH,P,T)
 
! CALCOLA L'UMIDITA SPECIFICA A PARTIRE DALLA UMIDITA RELATIVA 

  implicit none
  real::rh,p,t
  real::esat

   if ( t < 8 )then         !al disotto ritorna NaN
      relhumtoq =-999.9
     return
  end if

  relhumtoq=RH*(0.622*ESAT(T))/(P-0.378*ESAT(T)) 
  if (relhumtoq <  0.) relhumtoq =0. 

  return 
end function RELHUMTOQ
 
!-----------------------------------------------------------------------
 
real function ESAT(T)  
 
! CALCOLA LA PRESSIONE DI VAPORE SATURO A PARTIRE DALLA TEMPERATURA 
! REF.: BAKER, MON.WEA.REV.,1983,111,PAG.328 E SEG. 
! T in Kelvin, SAT(T) in hPa ()  Sostituisce la vecchia ESAT di NCAR
!-----------------------------------------------------------------------
  implicit none
  real::t
  real::a,b


!escludo le temperature minori di 8 gradi Kelvin, poichè la function
!restituirebbe un numero infinito.
  if ( t < 8 )then
     esat=-999.9
     return
  end if

  if(t > 273.16)then 
     a=17.269 
     b=35.86 
  else 
     a=21.874 
     b=7.66 
  end if
 
  esat=6.11*exp(a*(t-273.16)/(t-b)) 

  return 
end function ESAT

!-----------------------------------------------------------------------------
!
!   PARAMETRI TERMODINAMICI PER COSTRUIRE I PROFILI VERTICALI RELATIVI
!   AI NOMOGRAMMI
!    
!------------------------------------------------------------------------------

real function AEQL(P,T,N,PLOW,THS) 

!
!     AEQL- EQUILIBRIUM LEVEL OF A SOUNDING (HPA)
!
!     P- PRESSURE ARRAY (HPA)
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
  implicit none
  integer::n
  real,dimension(n)::P,T
  real::plow,ths
  real::x,pc,del,tccl
  integer::j,ih,il,i
  real::tsa,trpt,os       !function termolib

  pc=plow

  if(THS <  0.0)then
     TCCL=TRPT(P,T,N,PLOW)
     THS=OS(TCCL,PLOW)
  end if
     
  do  J=1,N-1

     I=N-J

     if(P(I+1) > PLOW)then
        AEQL=PC
        return
     end if

     IL=I
     X=TSA(THS,P(I))-T(I)
     if(X >= 0.0)exit 

  end do

  ih=il+1
  DEL=P(il)-P(ih)
  PC=P(ih)+0.5*DEL

  do  j=1,15
     DEL=DEL*0.5
     X=TRPT(P,T,N,PC)-TSA(THS,PC)
     PC=PC+sign(DEL,X)
  end do

  AEQL=PC

  return
end function AEQL



real function eql (pp,tt,np,plow,tlow)
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
 
  implicit none
  integer::np
  real,dimension(np)::pp,tt
  real::plow,tlow,temp,tamb
  real::tsa,trpt,pres

  if ( pp(1) <= pp (np) .or. pp(1) < 0 .or. tt(1) < 0 )then
     eql=-999.9
     return
  end if

  eql=0.
  pres=plow



  do while (pres > 100. )

     pres=pres-1
     temp=tsa(tlow,pres)
     tamb=trpt(pp,tt,np,pres)
     
     if( temp+1 < tamb)then
        eql=pres
        return
     end if
        
  end do

  return
end function eql


!----------------------------------------------------------------


function alfc (pp,tt,aw,np,strato)

! calcola il livello di libera condensazione convettiva 
! Function completamente riscritta, sostitisce la precedente
! N.C.A.R.
!
! ALFC- LEVEL OF FREE CONVECTION PRESSURE (HPA)
! RETURNS 0.0 IF NONE EXISTS
!
! Pp-     PRESSURE ARRAY (HPA)
! Tt-     TEMP ARRAY (KELVIN)
! AW -    mixing ratio Array [g/Kg]
! NP-     NUMBER OF CONTACT POINTS
! strato- spessore dello strato [hPa]
!
!----------------------------------------------------------------
  real,dimension(np)::pp,tt,aw

!trovo il rapporto di mescolanza medio dello strato
  samix=PKAVR(Pp,aw,np,Pp(1),Pp(1)-strato)
!trovo la temperatura media dello strato
  t_bar=PKAVR(Pp,tt,np,Pp(1),Pp(1)-strato)
!trovo la pressione media dello strato
  p_bar=(pp(1) + ( pp(1) -strato ))*0.5

!ora trovo l'intercetto tra l'iso-igrometria media dello strato
!e la tempratura lungo l'adiabatica seccca che parte dalla temperatura
!media dello strato
  t_ad=o(t_bar,p_bar)
  t_sat=t_ad
  t_iso=tmr(samix,p_bar)

  if (pp(1) < 0. .or. t_ad < 0 .or. t_sat < 0. )then
     alfc=-999.9
     return
  end if
  
  do while ( t_sat > t_iso )

     p_bar=p_bar-1
!mi fermo a 100 hPa se non trovo il cfcl
     if(p_bar < 100. )then
        alfc=0.                   !non esiste lcf
        return
     end if

     t_adi=tda(t_ad,p_bar)
     t_iso=tmr(samix,p_bar)
         
     if ( t_iso >= t_adi )then

!se si trova l'lcl si calcola il free convective condensation level           
!partendo dall'adiabatica che sale dal lcl, e questo è anche il livello di 
!equilibrio

        t_sat=os(t_iso,p_bar)

        do while (p_bar > 100. )
           p_bar=p_bar-5
           temp=tsa(t_sat,p_bar)
           tamb=trpt(pp,tt,np,p_bar)
           
           if( temp >= tamb)then
              alfc=p_bar
              return
           end if
        
        end do

     end if
  end do
  
  return
end function alfc


real function cape (pp,tt,np,lfc,eql)

!
! Calcola l'area di istabilità per sollevamento forzato, secondo la
! definizione standard di CAPE (Convective Available Potential Energy
!
! input :
! 
! pp   :    vettore pressioni                     [hPa]
! tt   :    vettore temperature ambiente          [°K]
! np   :    numero elementi vettori pp e tt
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
! Ritorna 0 se non esiste cape, ritorna -999.9 se non è possibile calcolarlo
!----------------------------------------------------------------------------

  implicit  none

  real::ra
  parameter (ra=287.05)
  real,dimension(np)::tt,pp
  real::lfc,eql,tamb,tlcl,pres1,pres2
  real::os,tsa,trpt
  real::tamb_upp,tamb_low,temp_upp,temp_low
  integer::np

  if ( pp(1) <= pp (np) .or. pp(1) < 0 .or. tt(1) < 0 )then
     cape=-999.9
     return
  end if

  cape=0.

  tlcl=trpt(pp,tt,np,lfc)   !temperatura del lcl
  tlcl=os(tlcl,lfc)

  pres1=lfc+1

  do while (pres1 > eql ) 

     pres1=pres1-1
     pres2=pres1-1
     temp_low=tsa(tlcl,pres1)
     tamb_low=trpt(pp,tt,np,pres1)
     temp_upp=tsa(tlcl,pres2)
     tamb_upp=trpt(pp,tt,np,pres2)
     
!se l'area è positiva incremento la sommatoria del cape
!sommando l'area del trapezoide
     if(temp_low > tamb_low .and. temp_upp > tamb_upp)then

        cape=cape+ (alog (pres1/pres2))* ra * &
             &( temp_low - tamb_low + temp_upp -tamb_upp )/2  

     end if
         
  end do

  
  return
end function cape


!_______________________________________________________________________



real function cin (pp,tt,td,np)


! Calcola l'inibizione convettiva, ossia l'energia da fornire ad una
! particella che stazione al suolo affinchè possa raggiungere il livello
! di libera convezione - se questo esiste -
!
! input:
! pp   :    vettore pressioni                     [hPa]
! tt   :    vettore temperature ambiente          [°K]
! td   :    vettore temperature di rugiada        [°K]
! np   :    numero elementi vettori pp e tt
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
! Ritorna 0 se non esiste cin, ritorna -999.9 se non è possibile calcolarlo
!
!----------------------------------------------------------------------------
  
  implicit  none

  real::ra,strato
  parameter (ra=287.05,strato=50.)
  real,dimension(np)::tt,pp,td,aw
  real::lcl,eql,tamb,tlcl,pres1,pres2,samix,tmed
  real::os,tsa,trpt,w,tmr,pkavr,o,tda
  real::tamb_upp,tamb_low,tadi_upp,tadi_low,t_iso
  integer::np,flag,k

  if ( pp(1) <= pp (np) .or. pp(1) < 0 .or. tt(1) < 0 &
       &.or. td(1) < 0 )then
     cin=-999.9
     return
  end if

  cin=0.

!vettore dei rapporti di mescolanza
  do k=1,np
     Aw(k)=W(td(k),Pp(k)) 
  end do

!trovo il rapporto di mescolanza medio dello strato
  samix=PKAVR(Pp,aw,np,Pp(1),Pp(1)-strato)
!trovo la temperatura media dello strato
  tmed=PKAVR(Pp,tt,np,Pp(1),Pp(1)-strato)
  
  flag=-1
  pres1=pp(1)-strato+1

  tadi_low=99999.9  
  do while ( pres1 > 100 )

!trovo il punto di saturazione ed incremento l'integrale del cin         
     pres1=pres1-1
     pres2=pres1-1

     t_iso=tmr(samix,pres1)     !calcolo la temperatura lungo l'iso-igrometrica

     tamb_low=trpt(pp,tt,np,pres1)  !interpolo la temperatura ambiente a pres1
     tamb_upp=trpt(pp,tt,np,pres2)  !interpolo la temperatura ambiente a pres2
         
     if ( tadi_low < t_iso .and. flag == -1)then  !se saturi calcoliamo
        tmed=os(tadi_low,pres1)                   !il valore della linea
        flag=0                                    !di processo ad. saturo
     end if

     if(flag == -1)then              !non siamo ancora alla saturazione
        tadi_low=tda(tmed,pres1)
        tadi_upp=tda(tmed,pres2)
     else                            !siamo saturi
        tadi_low=tsa(tmed,pres1)
        tadi_upp=tsa(tmed,pres2)
     end if

     cin=cin+ (alog (pres1/pres2))* ra * &
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

real function ALCL(TDS,TS,PS)

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
!  ALCL = -999.9 Se non e' possibile calcolarla.  
!--------------------------------------------------------------------------
  implicit none
  real::tds,ts,ps
  real::tda,tmr,o,w
  real::x,pi,ao,aw
  integer::i

  if(TDS < 0 .or. TS <  0 .or. PS < 0)then 
     ALCL=-999.9 
     return 
  end if
 
  AW = W(TDS,PS) 
  AO = O(TS,PS) 

  PI =  PS 
  do  I =1,10 
     X  =  .02*( TMR(AW,PI) - TDA(AO,PI) ) 
     if ( abs(X) < 0.01  )then
        PI = PI* ( 2.**(X)  ) 
        ALCL =  PI 
        return
     end if
     PI = PI* ( 2.**(X)  ) 
  end do

  return 
end function ALCL
 
!--------------------------------------------------------------------------

real function ALCLM(aw,ao,PS)  

! calcola il lifted condensation level a partire dal rapporto di mescolanza 
! medio dello strato e dalla temperatura potenziale media dello strato
!
! uso x = ALCLM (aw,ao,ps )
!
! input  aw    reale  mixing ratio medio dello strato             [g/Kg]
! inpt   ao    reale  temperatura potenziale media dello strato   [°K]
! input  ps    reale  livello di pressione inferiore dello strato [hPa]
! output alclm reale  livello di condensazione forzata            [hPa]
! ALCL = -999.9 Se non e' possibile calcolarlo  
!----------------------------------------------------------------------------

  implicit none
  real::aw,ao,ps
  real::tda,tmr
  real::x,pi
  integer::i


  PI =  PS 
  do    I =1,10 
     X  =  0.02*( TMR(AW,PI) - TDA(AO,PI) ) 
     if ( abs(X) < 0.01  )then
        ALCLM =  PI 
     end if
     PI = PI* ( 2.**(X)  ) 
  end do

  ALCLM =  PI 
  return 
end function ALCLM
 

!--------------------------------------------------------------------------

real function  CT(WBAR,P_CCL,P0 )     
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
! CT =  -999.9    Se non e' possibile calcolarli
!--------------------------------------------------------------------------
  implicit none
  real::wbar,p_ccl,p0
  real::tmr,o,tda           !function termolib
  real::tc,ao

  if(WBAR <= 0 .or. P_CCL <= 0 .or. P0 <= 0)then 
     CT=-999.9 
     return 
  endif
 
  TC  = TMR(WBAR,P_CCL) 
  AO  =    O(TC,P_CCL ) 
  CT  =  TDA(AO,P0) 

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
!  CCL =  -999.9 Se non e' possibile calcolarlo.  
!-------------------------------------------------------------------------

  implicit none
  integer::n
  real,dimension(n)::T,P,TD 
  real::wbar,pm
  real::w,tmr                 !function termolib
  integer::k,j,i,l,m
  real::pc,del,a,x,tq

    if(PM /=  P(1))then      !parte con uno strato (entraiment)
       WBAR =0 
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
             WBAR=(W(TD(I),P(I))+W(TD(L),P(L)))*alog(P(I)/P(L))+WBAR 
          end do
       end if

       L=K+1 
       TQ=TD(K)+(TD(L)-TD(K)  )*(alog(PM/P(K)))/(alog(P(L)/P(K))) 
       WBAR = (WBAR+(W(TD(K),P(K))+W(TQ   ,PM  ))*alog(P(K)/PM)) 
       WBAR=  WBAR/( 2.*alog(P(1)/PM) ) 

!     find the  level at which  tmr -ts  changes  sign.ts -sounding 
    else                      !parte con il primo livello
         
       WBAR=W(TD(1),P(1)) 
       PC=PM 
         
       if(abs(T(1)-TD(1)).LT.0.05)then
          CCL=PC 
          return
       end if
    end if
 
!     find the  level at which  tmr -ts  changes  sign.ts -sounding 
    do  J=1,N 

       I=N-J+1 
       if(P(I) < 300.0)cycle
       X=TMR(WBAR,P(I))-T(I)
 
       if(X <=  0.0)then
!         set up bisection routine 
          L=I 
          I=I+1 
          DEL = P(L)- P(I) 
          PC  =  P(I) + .5*DEL 
          A=   ( T(I)-T(L))/alog( P(L)/P(I)) 
          do  m=1,10 
             DEL = DEL/2. 
             X= TMR(WBAR,PC)-T(L)-A*(alog(P(L)/PC)) 
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
! TRPT =  -999.9 Se non e' possibile calcolarlo.
!------------------------------------------------------------------------
  implicit none
  integer::n,i
  real,dimension(n)::P(n),A(n) 
  real::tp
  real::x,x1,x2

  TRPT = -999.9 

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

real function TMR(W,P)  

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
! TMR =  -999.9 Se non e' possibile calcolarla 
!--------------------------------------------------------------------------

  X =  alog10(   W*P/(622.+ W)  ) 
  TMR=10.**(.0498646455*X+2.4082965)-7.07475+38.9114*&
       &((10.**( .0915*X ) - 1.2035 )**2 ) 



  return 
end function TMR

!---------------------------------------------------------------------------

real function TDA(OO,P)    

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
! TDA =  -999.9  Se non e' possibile calcolarla 
!---------------------------------------------------------------------------

  implicit none
  real::oo,p

  if(oo < 0 .or. P < 0)then 
     TDA=-999.9 
     return 
  end if

  TDA=oo*((P*0.001)**0.286) 

  return 
end function TDA
  
!-----------------------------------------------------------------------------

real function TSA(OS,P) 

! Calcola la temperatura lungo una adiabatica satura 
! caratterizzata dalla temperatura T_AS alla pressione P 
! 
! Uso :   x = TSA (T_AS,P) 
! 
! Input : 
! T_AS     real  Temperatura adiabatica satura                       (K.) 
! P        real  Pressione di intersecazione sull' isoigrometrica    (hPa) 
! 
! Output : 
! TSA      real  Temperatura                                         (K.) 
! TSA =  -999.9  Se non e' possibile calcolarla 
!-----------------------------------------------------------------------------
  implicit none
  real::os,p
  real::a,tq,d,x
  integer::i
  real::w

  if(OS <  0 .or. P < 0)then 
     TSA=-999.9 
     return 
  end if

  A  =  OS 
  TQ  =  253.16 
  
  D =  120 
  do   I = 1,12 
     D = D/2. 
 
!se la differenza x di temperatura è piccola esce dal ciclo e dalla function  
     X=A*exp(-2.6518986*W(TQ,P)/TQ)-TQ*((1000./P)**.286) 
     if(abs(X) < 0.01)then
        TSA=TQ 
        return
     end if
     TQ = TQ + sign(D,X)
  end do
  
  TSA=TQ 

  return 
end function TSA

!-----------------------------------------------------------------------------

subroutine TROPO(P,T,AZ,N,RLDC,ZLAY,PLOW,PHI,PTROP,ZTROP)  
 

! calcola l'altezza ed il livello di pressione della tropopausa 
! esce a -999.9 se non trova o se non esiste la tropopausa secondo 
! i criteri inseriti, i criteri inseriti se sono posti = -999.9
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
  implicit none
  integer::n                    !input
  real,dimension(n)::P,T,AZ     !input
  real::rldc,zlay,plow,phi      !input
  real::ptrop,ztrop             !output
  integer::i,k                  !variabili locali
  real::z2,rlap,zmax,p2,t2      !variabili locali
  real::ptrp,trpt               !function termolib

  PTROP=-999. !inizializzazione 
  ZTROP=-999. !inizializzazione
! setto i default di ricerca se non sono stati inizializzati dall'utente
! secondo lo standard operativo N.C.A.R.
  if(RLDC == -999.9)rldc=2.    ![K]
  if(ZLAY == -999.9)zlay=500.  ![m]
  if(plow == -999.9)plow=300.  ![hpa]
  if(phi  == -999.9)phi=10.    ![hpa]

  if(P(N) > PLOW)return
  RLAP=RLDC*0.001 
	
  do  I=1,N-1

     if(P(I) < PHI)return
     if(P(I) > PLOW)cycle
     if(T(I)-T(I+1) > (AZ(I+1)-AZ(I))*RLAP)cycle 
     ZMAX=AZ(I)+ZLAY 
     Z2=AZ(I)+ZLAY 
     if(Z2 > AZ(N))ZMAX=AZ(N) 
     if(Z2 > AZ(N))Z2=AZ(N) 
     K=I+1 
     P2=PTRP(P,AZ,N,Z2) 
     T2=TRPT(P,T,N,P2) 

     do while (ZMAX >=  AZ(K)) 
        if(T(I)-T2 > (Z2-AZ(I))*RLAP)exit 
        K=K+1 
        Z2=AZ(K) 
        T2=T(K) 
     end do

     PTROP=P(I) 
     ZTROP=AZ(I) 

     return 

  end do

end subroutine TROPO


!----------------------------------------------------------------------


subroutine zero_termico (pp,tt,td,np,rhstaz,p_zero,h_zero)

! Calcola l'altezza ed il livello di pressione dello zero termico.
! Restituisce il dato dell'altezza dello zero termico scandendo dall'alto,
! Quindi in caso di inversioni invernali al suolo nulla vieta che lo zero
! sia presente anche a quote inferiori a quella riportata dalla subroutine
!
! uso  :  call zero_termico (pp,tt,td,np,rhstaz,p_zero,h_zero)
!
!  input :
!  PP     real       Vettore delle pressioni                        (hPa) 
!  TT     real       Vettore delle Temperature dell'aria            (K.) 
!  TD     real       Vettore delle Temperature di rugiada           (K.) 
!  NP     integer    Numero di dati 
!  rhstaz real       Altezza della stazione S.L.M.                  (m)
!
!  output:
!  p_zero real       Livello di pressione dello zero termico        (hPa)
!  h_zero real       Altezza dello zero termico                     (m)
!---------------------------------------------------------------------------
  implicit none
  real::abz
  parameter (abz=273.15)
  integer::np
  real,dimension(np)::pp,tt,td
  real,rhstaz,p_zero,h_zero,t_zero,pres
  real::trpt,z

  if(np < 2 .or. pp(1) <  pp(np) )then !filtra input invalidi
     h_zero=-999.9
     p_zero=-999.9
     return
  end if

  t_zero=-999.9
  pres=400.
  do while( t_zero <= abz )
     pres=pres+1    
     t_zero=trpt(pp,tt,np,pres)
     if( pres > pp(1) )then
        p_zero=-999.9
        h_zero=-999.9
        return
     end if
  end do

  p_zero=pres-1                        !livello pressione zero termico
  h_zero=z(p_zero,pp,tt,td,np)+rhstaz  !altezza zero termico

return

end subroutine zero_termico


!----------------------------------------------------------------------
 
real function Z(PT,P,T,TD,N)  

! Calcola l' altezza geopotenziale al livello di pressione "PT" 
! 
! Uso : x = Z (PT,P,T,TD,NT) 
! 
!  Input :
!  PT   real   livello per il quale si vuole calcolare l'altezza (hPa) 
!  P    real   Vettore delle pressioni                           (hPa) 
!  T    real   Vettore delle Temperature dell'aria               (K.) 
!  TD   real   Vettore delle Temperature di rugiada              (K.) 
!  NT   I*4    Numero di dati 
! 
!  Output : 
!  Z      real   Altezza del Geopotenziale                       (mgp) 
!  Z =  -999.9  Se non e' possibile calcolarla 
!----------------------------------------------------------------------
 
  implicit none
  integer::n
  real,dimension(n)::t,p,td
  real::pt,a1,a2
  real::w
  integer::i,j

  z = 0.0                   !inizializzo l'altezza 

  if(N == 0 .or. PT <  P(N) .or. PT > p(1) )then !filtra input invalidi
     z=-999.9
     return
  end if
      
  if(pt == p(1) )then
     z=0
     return
  end if

  I =  0
  j =  i+1
  do while (pt < p(i+1) )
     I = I+1 
     J  = I+1 
     if(PT  >= P(J))then
        A1=T(J)*(1.+.0006078*W(TD(J),P(J))) 
        A2=T(I)*(1.+.0006078*W(TD(I),P(I))) 
        z = z+14.64285*(A1+A2)*(alog(P(I)/PT )) 
        return
     end if

     A1=T(J)*(1.+.0006078*W(TD(J),P(J))) 
     A2=T(I)*(1.+.0006078*W(TD(I),P(I))) 
     z = z+14.64285*(A1+A2)*(alog(P(I)/P(J))) 
  end do

  A1=T(J)*(1.+.0006078*W(TD(J),P(J))) 
  A2=T(I)*(1.+.0006078*W(TD(I),P(I))) 
  z =z+14.64285*(A1+A2)*(alog(P(I)/PT )) 
	
  return 
end function Z
 

!-----------------------------------------------------------------------------
!
!                                  VENTO 
!
!-----------------------------------------------------------------------------

subroutine UV(DD,FF,U,V)   

!  Calcola le componenti U e V  del vento espresse in m/sec 
! 
!      Uso : 
!                  CALL UV (DD,FF,U,V) 
! 
!      Input : 
!  DD   real   Direzione del vento                  (Gradi sessag, 0 = nord) 
!  FF   real   Velocita` del vento                  (m/sec) 
! 
!      Output : 
!  U   real   Componente lungo i paralleli del vento orientata da 
!            West a Est                                              (m/sec) 
!  V   real   Componente lungo i meridiani del vento orientata da 
!            Sud a Nord                                              (m/sec) 
! 
!  U e V =  -999.9 Se non e' possibile calcolarle 	
!
!------------------------------------------------------------------------------
  implicit none
  real,atr
  parameter(ATR=0.0174533) 	
  real::dd,ff,u,v
  real::ar

  if(DD < 0 .or. DD > 360 .or. FF < 0 .or. FF < 300)then
     U=-999.9 
     V=-999.9
  end if
  
! Calcolo le componenti del vento U e V 
  AR=DD*ATR 
  U=-FF*sin(AR) 
  V=-FF*cos(AR) 
  
  return 
end subroutine UV

!--------------------------------------------------------------------------


subroutine UVWIND(UBAR,VBAR,SPEED,DIREC)     
! Ricostruisce direzione e velocita` del vento dalle componenti cartesiane 
! 
! Uso :  CALL UVWIND (U,V,DD,FF) 
! 
! Input : 
! U   real   Componente lungo i paralleli del vento orientata da West a Est 
!            espressa in (m/sec) 
! V   real   Componente lungo i meridiani del vento orientata da Sud a Nord 
!            espressa in (m/sec) 
! 
! Output : 
! DD   real   Direzione del vento                  (Gradi sessag, 0 = nord) 
! FF   real   Velocita` del vento                  (m/sec) 
! 
! DD e FF =  -999.9 se non e' possibile calcolarle.  
!------------------------------------------------------------------------------
  implicit none
  real::ubar,vbar,speed,direc
  real,rta
  parameter(RTA=57.2957795) 

  if(abs(UBAR) > 300.or. abs(VBAR) > 300)then
     speed=-999.9 
     direc=-999.9
     return
  end if

  speed=SQRT(UBAR**2+VBAR**2) 
  direc=360.0 
  if(speed <=  0.01)return

  direc=ATAN2(UBAR,VBAR)*RTA+180.0 
  if(direc <= 0.1)direc=360.0 
 
  return 
 
end subroutine UVWIND

!------------------------------------------------------------------------

real function AVVEZ (P1,DD1,FF1,P2,DD2,FF2,ALAT)   
!
! costanti :
! Aomega : Velocita' angolare terra
! Atr    : conversione da gradi a radianti
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

  implicit none
  real::p1,dd1,ff1,p2,dd2,ff2,alat  !input
  real::u1,u2,v1,v2,uvm,uvt,vvt,vvm
  real::grad,alamda,a,b
  real,atr,ra,aomega  ! da gradi in radianti  e Costante dei gas per aria secca
  parameter(atr=0.0174533,ra=287.05,aomega=7.292*10.**-5)
   
  ALAMDA=2*AOMEGA*sin(ALAT*ATR)   !Coriolis 
  A=ALAMDA/RA 
  B=P1/P2 
  B=log(B) 
 
! Calcolo le componenti del vento U e V 
  call UV(DD1,FF1,U1,V1) 
  call UV(DD2,FF2,U2,V2) 
  
  if(u2 > 300 .or. u1 > 300)then
     AVVEZ=999.9 
     return
  end if

! Calcolo le componenti del vento termico 
  UVT=U2-U1 
  VVT=V2-V1 
 
! Calcolo le componenti del vento medio 
  UVM=(U2+U1)/2.	
  VVM=(V2+V1)/2. 
 
! Calcolo il valore del termine avvettivo 
  GRAD=UVT*VVM-VVT*UVM 
  AVVEZ=A/B*GRAD 
  AVVEZ=AVVEZ*3600.			! in Kelvin/ore 

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
!  F e D = -999.9 Se non e' possibile calcolarle. 
!------------------------------------------------------------------------------
  implicit none
  integer::nw                          !input
  real,dimension(nw)::P,SPD,DIR,U,V    !input
  real::speed,direc                    !output
  real::spds,dirs,p1,p2,ubar,vbar
  integer::i
  real::pkavr                          !funzione termolib

  if(P1 >  P(1) .or. P2 < P(NW))then 
     SPDS=-999.9 
     DIRS=-999.9 
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
! PTRP =     -999.9   se non e' possibile calcolarla.  
!------------------------------------------------------------------------------

  implicit none
  integer::n
  real,dimension(n)::P,A
  real::ax
  real::rdir,y1,y2
  integer::i

  PTRP=-999.9 
  RDIR=sign(1.0,A(N)-A(1)) 
  if(AX*RDIR > A(N)*RDIR)return 
      
  do  I=1,N-1 
     if(AX*RDIR <  A(I)*RDIR)then
        PTRP=-999.9 
        return
     end if

     if(AX*RDIR > A(I+1)*RDIR)cycle 
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
!  F e D =  -999.9 Se non e' possibile calcolarle.  
!------------------------------------------------------------------------------

  implicit none
  integer::n                      !input
  real,dimension(n)::P,SPD,DIR    !input
  real::s,d                       !output
  real,atr,rta
  parameter(atr=0.0174533,rta=57.2957795)
  real::u,v,u1,u2,v1,v2,a1,a2,x1,x2,x,tp,a,cnp
  integer::i,manca,k

  CNP(A)=AMOD((450.-A),360.) 
  
  if(N <= 1 .or. TP < P(N) .or. TP > P(1) )then
     S=-999.9 
     D=-999.9
     return
  end if
 
  do k = 1,n
     if(spd(k) == -999.9 .or. dir(k) == -999.9)manca=manca+1
  end do

  if(manca > n/2 )then
     S=-999.9 
     D=-999.9
     return
  end if

  do  I=1,N-1

     if(TP >  P(I))then 
        S=-999.9 
        D=-999.9 
        return
     end if

     if(TP <  P(I+1))cycle
 
     if(DIR(I) >  400.0 .OR. DIR(I+1) > 400.0)then
        S=-999.9 
        D=-999.9 
        return
     end if

     X1=alog(P(I)) 
     X2=alog(P(I+1)) 
     X=alog(TP) 
     A1=CNP(DIR(I))*ATR 
     A2=CNP(DIR(I+1))*ATR 
     U1=SPD(I)*cos(A1) 
     U2=SPD(I+1)*cos(A2) 
     V1=SPD(I)*sin(A1) 
     V2=SPD(I+1)*sin(A2) 
     U=(U1*(X-X2)-U2*(X-X1))/(X1-X2) 
     V=(V1*(X-X2)-V2*(X-X1))/(X1-X2) 
     S=SQRT(U*U+V*V) 
     D=360. 
     if(S  <  0.01)return
              
     D=CNP( RTA*ATAN2(V,U)) 
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
!  PKAVR =   -999.9   Se non e' possibile calcolarla. 
!------------------------------------------------------------------------------

  implicit none
  integer::n
  real,dimension(n)::PRES,THR
  real,dimension(200)::P,A
  real::p1,p2,wbar
  integer::k,i,j,l
  real::trpt

  if(P1 > PRES(1) .or. P2 < PRES(N) )then
     PKAVR=-999.9 
     return
  end if

  K=0 
  PKAVR=0.0 
  WBAR=0.0 
  K=K+1 
  P(K)=P1 
  A(K)=TRPT(PRES,THR,N,P1) 

  if (p1 == p2) then
     pkavr=a(k)
     return
  endif

  do  I=1,N 
     if(PRES(I) <= P2)exit
     K=K+1 
     P(K)=PRES(I) 
     A(K)=THR(I) 
  end do

  K=K+1 
  P(K)=P2 
  A(K)=TRPT(PRES,THR,N,P2) 
  J=K-1 
  do I=1,J 
     L=I+1 
     WBAR=(A(I)+A(L))*alog(P(I)/P(L))+WBAR 
  end do

  WBAR=WBAR/(2.*alog(P(1)/P(K))) 
  PKAVR=WBAR 

  return 
end function PKAVR

!-----------------------------------------------------------------------
!
!                       INDICI TEMPORALESCHI
!
!-----------------------------------------------------------------------

real function si_K  (PT,T,TD,NT) 
!
! Calcola l'indice temporalesco K-Index 
!
! 
  REAL PT(NT),T(NT),TD(NT) 
  real::abz
  parameter(abz=273.14) 

  T850=TRPT(PT,T,NT,850.) 
  T700=TRPT(PT,T,NT,700.) 
  T500=TRPT(PT,T,NT,500.) 
  TD850=TRPT(PT,TD,NT,850.) 
  TD700=TRPT(PT,TD,NT,700.) 
 
  if(T850 < 0 .or. T500 <0  .or. TD850 < 0 .or. TD700 < 0)then 
     si_K=-999.9 
     return 
  end if
 
  T850=T850-ABZ 
  T500=T500-ABZ 
  T700=T700-ABZ 
  TD850=TD850-ABZ 
  TD700=TD700-ABZ 
  TMENTD = abs(T700-TD700) 
  si_K  = T850 - T500 + TD850 - TMENTD 

  return 
end function si_K

!-------------------------------------------------------------------------

real function si_LI (PT,T,TD,NT) 

 
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
!  SI_LI =   -999.9  se non e' possibile calcolarlo. 
!
!-------------------------------------------------------------------------

  REAL PT(NT),T(NT),TD(NT),aw(nt)
 
  real::abz

  PM=PT(1)-50. 
  if(PM <  PT(NT))then
     si_li=-999.9
     return
  end if
  
  do k=1,nt
     Aw(k)=W(td(k),Pt(k))   !rapporto di mescolanza
  end do

  wbar=PKAVR(Pt,AW,nt,Pt(1),Pt(1)-50.) !wmed primi 50 hPa

  tdew=tmr(wbar,pt(1))
  plift=alcl(tdew,t(1),pt(1)) 

  TLCL=tmr(wbar,plift)                !Temperatura al LCL
  TAS=OS(tlcl,plift) 

  TSA500=TSA(TAS,500.) 
  T500=TRPT(PT,T,NT,500.) 

  if(T500 <  0 .OR. TSA500 < 0 )then
     si_li=-999.9
     return
  end if

  si_LI=T500-TSA500 


  return  
end function SI_LI
 
!-----------------------------------------------------------------------------

real function si_SW(PT,T,TD,NT,PW,FF,DD,NW) 

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
!  SI_SW =  -999.9  Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  real,dimension(nt)::PT,T,TD
  real,dimension(nw)::PW,DD,FF
  real::abz,convff,atr
  parameter(ABZ=273.15,convff=1.94,ATR=0.0174533) 
	
  T850=  TRPT(PT,T,NT,850.)  -ABZ 
  T500=  TRPT(PT,T,NT,500.)  -ABZ 
  TD850= TRPT(PT,TD,NT,850.) -ABZ 

  call TRPW(PW,FF,DD,NW,850.,FF850,DD850) 
  call TRPW(PW,FF,DD,NW,500.,FF500,DD500) 

  if(T850 < -900 .or. T500 < -900 .or. TD850 < -900.or. &
       &FF850 < 0 .or. FF500 < 0 .or. DD850 < 0 .or. DD500 < 0)then
	si_SW=-999.9
        return
  end if

  FF850=FF850*CONVFF 
  FF500=FF850*CONVFF 
 
  RTOT=(T850+TD850-2.*T500)-49. 
  if(RTOT < 0)RTOT=0. 
 
  RS=((sin(DD500-DD850))+0.2) 
 
  if(DD850 >= 130 .and. DD850 <= 250)RS=0. 
  if(DD500 >= 210 .and. DD500 <= 310)RS=0. 
  if((DD500-DD850) > 0)RS=0 
  if(DD850 < 15 .or. DD500 < 15)RS=0 
 
  if(TD850 < 0)then 
     RD85=0. 
  else 
     RD85=12.*TD850 
  end if
 
  si_sw=RD85+20.*RTOT+2*FF850+FF500+125.*RS 
 
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
!  TEP_IDX =  -999.9  Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  REAL,dimension(nt)::PT,T,TD 
 
  T850=TRPT(PT,T,NT,850.) 
  T500=TRPT(PT,T,NT,500.) 
  TD850=TRPT(PT,TD,NT,850.) 
  TD500=TRPT(PT,TD,NT,500.) 
  TEP850=OE(TD850,T850,850.) 
  TEP500=OE(TD500,T500,500.) 
  
  if(TEP500 <  0 .or. TEP850 < 0)then
     TEP_IDX=-999.9 
     return
  end if

  TEP_IDX=TEP500-TEP850
 
  return  
end function TEP_IDX

!----------------------------------------------------------------------------


real function si_SH(PT, T, TD, NT) 

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
!  si_SH  =  -999.9     Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  real,dimension(nt)::PT,T,TD 
 
  if(PT(1) < PT(NT))then
     si_sh=-999.9 
     return
  end if

  td850=TRPT(PT,Td,NT,850.)     !td a 850 hPa
  t850= TRPT(PT,T,NT,850.)      !t  a 850 hPa
  w850 = w (td850, 850.)        !rapporto di mescolanza a 850 hPa

  pccl = ALCL(td850 ,t850 ,850.)

  TCCL=tmr(w850,pccl) 
  TAS=OS(TCCL,pccl)
  TSA500=TSA(TAS,500.) 

  T500=TRPT(PT,T,NT,500.) 

  if(T500 < 0 .or. TSA500 < 0)then 
     si_SH=-999.9 
     return
  end if

  si_SH= T500 - TSA500 

  return 
end function SI_SH

!----------------------------------------------------------------------------

function SI_U(PT,T,TD,NT) 

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
!  SI_U =  -999.9     Se non e' possibile calcolarlo. 
!----------------------------------------------------------------------------

  real,dimension(nt)::PT,T,TD 
  
  T850=TRPT(PT,T,NT,850.) 
  T700=TRPT(PT,T,NT,700.) 
  T500=TRPT(PT,T,NT,500.) 
  TD850=TRPT(PT,TD,NT,850.) 
  TD700=TRPT(PT,TD,NT,700.) 
  TD500=TRPT(PT,TD,NT,500.) 
  U850=FR(T850,TD850) 
  U700=FR(T700,TD700) 
  U500=FR(T500,TD500) 
 
  if(U500 < 0 .or. U700 <  0 .or. U850 < 0)then
     si_U=-999.9 
     return
  end if

  si_U=(U850+U700+U500)/3. 

  return 
end function SI_U

!----------------------------------------------------------

real function si_tt (pt,t,td,nt )

!   Calcola l'indice di stabilità total-total
!
!    PT = vettore pressioni              [hPa]
!    T  = Vettore temperature            [°K]
!    TD = Vettore Temperature di rugiada [°K]
!
!    VT = Vertical Total
!    CT = Cross Total
!    TT = indice Total-Total
!_____________________________________________________________

  real,dimension(nt)::PT,T,TD

  T850=TRPT(PT,T,NT,850.) 
  T500=TRPT(PT,T,NT,500.) 
  TD850=TRPT(PT,TD,NT,850.) 

  if ( t850 == -999.9 .or. t500 == -999.9&
       & .or. td850 == -999.9 )then
     si_tt=-999.9
     return
  end if

  vt =  t850  - t500
  ct =  td850 - t500

  si_tt=vt+ct

  return
end function si_tt

!_________________________________________________________________

end module termolib

