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
module astrolib

  use phis_const

  implicit none

CONTAINS


  subroutine sundur(igg,imm,iaa,latit,longit,tzone,&
       &daylen,twam,twpm,riset,settm)

!------------------------------------------------------------------------
!
! Programma astronomico per il calcolo dell'alba e del tramonto
!
! Richiede in input giorno mese ed anno, 
! latitudine (centesimale) - longitudine (centesimale)
! time zone ( Italia = 1 )
!
! Restituisce :
! Durata del giorno                : daylen
! Inizio della luce                : twam
! Fine della luce                  : twpm
! Ora e minuti (sess) dell'alba    : riset
! Ora e minuti (sess) del tramonto : settm
!
!-------------------------------------------------------------------------

    integer,intent(in)::igg,imm,iaa,tzone
    real,intent(in)::latit,longit
    real,intent(out)::daylen,twam,twpm,riset,settm
    real::lambda,l,ll,mant
    real::obliq,alpha,delta,equation,ha,hb,twx,noont,d
    integer::inter,h,day,y,m,altmax,g

    day= igg
    y=iaa
    m=imm

    h = 12

    d = FNDAY(y, m, day, h)

!Uso FNsun per trovare la longitudine dell'eclittica solare
    lambda = FNSUN(d)

!Obliquita' dell' eclittica  
    obliq = 23.439 * rads - .0000004 * rads * d

!  Find the RA and DEC of the Sun

    alpha = atan2(cos(obliq) * sin(lambda), cos(lambda))
    delta = asin(sin(obliq) * sin(lambda))

! Find the Equation of Time in minutes
    l = FNRANGE(280.461 * rads + .9856474 * rads * d)
    g = FNRANGE(357.528 * rads + .9856003 * rads * d)

    ll = l - alpha

    if (l < pi) ll =ll+ 2.0*pi

    equation = 1440.0 * (1.0 - ll / pi/2.0)

    ha = F0(latit,delta) 
    hb = F1(latit,delta)  

    twx = hb - ha	     !  length of twilight in radians
    twx = 12.0*twx/pi	     !	length of twilight in hours 

!Conversion of angle to hours and minutes
    daylen = degs*ha/7.5
      
    if (daylen<0.0001)daylen = 0.0

    riset = 12.0 - 12.0 * ha/pi - tzone - longit/15.0 + equation/60.0
    settm = 12.0 + 12.0 * ha/pi - tzone - longit/15.0 + equation/60.0
    
    noont = riset + 12.0 * ha/pi
    altmax = 90.0 + delta * degs - latit
 
! to express altitude as degrees from the N horizon
    if (latit < delta * degs) altmax = 180.0 - altmax

    twam = riset - twx	! morning twilight begin
    twpm = settm + twx	! evening twilight end

    if (riset > 24.0) riset=riset- 24.0
    if (settm > 24.0) settm=settm- 24.0

!conversione tra minuti in centesimale e minuti in sessagesimale
    inter=int(riset)
    mant=riset-inter
    mant=(mant*60.)/100.
    riset=inter+mant

    inter=int(settm)
    mant=settm-inter
    mant=(mant*60.)/100.
    settm=inter+mant

    return
  end subroutine SUNDUR

!----------------------------------------------------------------------

real  function FNDAY (y,m,d,h)

!  Get the days to J2000
!  h is UT in decimal hours
!  FNday only works between 1901 to 2099 - 
!
!-----------------------------------------------------------------------

  integer,intent(in)::y,m,d,h
  real::luku

    luku = - 7 * (y + (m + 9)/12)/4 + 275*m/9 + d
    luku=luku+y*367
    fnday=luku - 730531.5 + h/24.0

    return
  end function FNDAY

!------------------------------------------------------------------

  real  function F0 (lat,declin)

! Calculating the hourangle

    real,intent(in)::lat,declin
    real::dfo,fo

    dfo = rads*(0.5*SunDia + AirRefr)
    if (lat < 0.0) dfo = -dfo
    fo = tan(declin + dfo) * tan(lat*rads)
    if (fo>0.99999) fo=1.0                  ! to avoid overflow //
    fo = asin(fo) + pi/2.0
    f0=fo

    return
  end function F0

!------------------------------------------------------------------

  real function F1(lat,declin)

! Calculating the hourangle for twilight times

    real,intent(in)::lat,declin
    real::df1,fi

    df1 = rads * 6.0
    if (lat < 0.0) df1 = -df1
    fi = tan(declin + df1) * tan(lat*rads)
    if (fi>0.99999) fi=1.0                  ! to avoid overflow //
    fi = asin(fi) + pi/2.0
    f1=fi
  
    return
  end function F1

!________________________________________________________________________

  real function FNSUN (d)
    
!
!  Calcola la longitudine dell'eclittica del sole 
!
!-------------------------------------------------------------------------

    real,intent(in)::d
    real::l,lambda,g
  
    l = FNRANGE(280.461 * rads + .9856474 * rads * d)

!   mean anomaly of the Sun
    g= FNRANGE(357.528 * rads + .9856003 * rads * d)

!   Ecliptic longitude of the Sun
    FNSUN= FNRANGE(l + 1.915 * rads * sin(g) + .02 * rads * sin(2 * g))

    return
  end function FNSUN

!________________________________________________________________________

  real function FNRANGE (x)

!  Calcola  un angolo nel range tra 0 e 2 pi greco
    real,intent(in)::x
    real::a,b
    
    b = 0.5*x / pi
    a = 2.0*pi * (b - int(b))
    if (a < 0) a = 2.0*pi + a
    FNRANGE=a

    return
  end function FNRANGE

!________________________________________________________________________

  real function insol (rad,rlat,rlon,tzone,ig,im,ia)

!
! Calcola l'insolazione (o soleggiamento) utilizzando la formula di Anstromg
!
!input :
! radi  reale      radiazione globale giornaliera   [Watt/metri2]
! ig    intero     giorno
! im    intero     mese
! ia    intero     anno                             (espresso con 4 digit)
! rlat  reale      latitudine
! rlon  reale      longitudine
! tzone integer    time zone                        [ore]
!output: 
!insol  reale      insolazione                      [ore,minuti]
!_____________________________________________________________________________

    integer,intent(in)::ig,im,ia,tzone
    real,intent(in)::rlat,rlon,rad
    real::radi
    integer,dimension(12)::mese
    real::rad_max
    integer::igio,l
    real::daylen,twam,twpm,riset,settm,rlat_rad,real_alba,real_tram
    real::rappsol
    integer::mant_alba,mant_tram
    
    data mese/31,28,31,30,31,30,31,31,30,31,30,31/

!la data e' composta da igio - m - l , rispettivamente giorno giuliano
! mese anno

    radi=rad

    igio=0
    do l=1,im-1
       igio=igio+mese(l)
    end do

    igio=igio+ig

!converto la latitudine in radianti
    rlat_rad= rlat / 180 * 4 * atan (1.)
    radi=radi*0.23892                       ! da watt/m2  a cal/cm2  

!radiazione massima astronomica in MegaJoule/m2
    rad_max= DAILY_ET_RADIATION (igio,rlat_rad)
                  
!converto in cal cm-2
    rad_max=((rad_max *1000000. ) / 10000. )/4.1855

!lunghezza del giorno - o insolazione massima astronomica giornaliera
    call SUNDUR(ig,im,ia,rlat,rlon,tzone,daylen,twam,twpm,riset,settm)    

!insolazione massima astronomica in minuti
    mant_alba=int(riset)
    mant_tram=int(settm)
    real_alba=(riset-mant_alba)*100
    real_tram=(settm-mant_tram)*100
    
    if(real_tram >= real_alba)then
       daylen=(mant_tram-mant_alba)*60 + (real_tram-real_alba)
    else
       daylen=(mant_tram-mant_alba-1)*60 +(real_tram-real_alba+60)
    end if
         
!calcolo il rapporto tra radiazione e radiazione massima
    rappsol=((radi/rad_max)-0.23)/0.37

!elimino i dati errati , ossia radiazione maggiore della massima teorica
!astronomica o formula rappsol > 1.3
    if(radi > (rad_max + (rad_max/10)) .or. rappsol > 1.5 )then
       insol=-999.9
    else
       if(radi > rad_max )then
          radi = rad_max
          rappsol=((radi/rad_max)-0.23)/0.37
       end if
    end if

    if(rappsol > 1)rappsol=1

!calcolo l'insolazione reale
    insol=daylen*rappsol

!tappo i possibili valori negativi per radiazioni bassissime
    if(insol < 0 .and. insol /= -999.9 )insol=0
                        
    return
  end function INSOL

!-----------------------------------------------------------------------------


real  function DAILY_ET_RADIATION (giul,rlat)

!input : giorno giuliano - Intero
!input : latitudine in Radianti della localita' (negativa emisfero sud)- Reale
!
!output Radiazione massima astronomica giornaliera in Mj m-2 - Reale
!
!____________________________________________________________________________

    integer,intent(in)::giul
    real,intent(in)::rlat
    real::soldec,omega
    
    soldec=SOLAR_DECLINATION(giul)
    omega=SUNSET_HOUR_ANGLE (rlat,soldec)

    daily_et_radiation= 37.5 *  EARTH_SUN_DISTANCE (giul) * &
         &( omega * sin (rlat) * sin (soldec) + cos (rlat) * &
         &cos(soldec) * sin (omega))

    return
  end function DAILY_ET_RADIATION


!________________________________________________________________________

  real function SUNSET_HOUR_ANGLE (rlat, soldec )

!input  : latitudine in radianti            -reale
!input  : declinazione solare in radianti   -reale
!output : angolo del tramonto in radianti

    real,intent(in)::rlat,soldec
    real::x

    x = -tan (rlat) * tan (soldec)
    SUNSET_HOUR_ANGLE= atan ( -x / sqrt ( -x * x +1 ) ) +2 * atan (1.)

    return
  end function SUNSET_HOUR_ANGLE

!________________________________________________________________________


  real function EARTH_SUN_DISTANCE (giul)
!input  : giorno giuliano - intero
!output : distanza relativa della terra dal sole
    
    integer,intent(in)::giul

    EARTH_SUN_DISTANCE = 1 + 0.033 * cos (0.0172 * giul)

    return
  end function EARTH_SUN_DISTANCE

!-----------------------------------------------------------------------

  real function SOLAR_DECLINATION (giul)
!
!     Calcola la Declinazione Solare
!
!input  : giorno giuliano - intero
!output : declinazione solare in radianti
!
!__________________________________________________________________________

    integer,intent(in)::giul

    SOLAR_DECLINATION=0.409 * sin( 0.0172 * giul - 1.39)
    
    return
  end function SOLAR_DECLINATION


!---------------------------------------------------------------

  integer function GIULIANO (igg,imm,iaa)

!     ritorna la data giuliana
!
!---------------------------------------------------------------------
       
    integer,intent(in)::igg,imm,iaa
    integer,dimension(12)::mese
    integer,l

    data mese/31,28,31,30,31,30,31,31,30,31,30,31/
      
    GIULIANO=0

    if(mod(iaa,4)==0)mese(2)=mese(2)+1
    
    do l=1,imm-1
       GIULIANO=GIULIANO+mese(l)
    end do

    GIULIANO=GIULIANO+igg
  
    return
  end function GIULIANO


end module astrolib
