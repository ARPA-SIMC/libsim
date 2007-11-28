module phisical_constant

real::eps
parameter(eps=0.622)              !rapporto ra/rv
real::ra
parameter(ra=287.05)              !costante dei gas per aria secca J/kg*°K
real::rv
parameter(rv=461.495)             !costante dei gas per vapore acqueo J/kg*°K
real::abz
parameter(abz=273.15)             !0 gradi celsius in kelvin
real::rta
parameter(RTA=57.2957795)         !
real::aomega
parameter(aomega=7.292*10.**-5)   !velocita' angolare della terra
real::atr                          
parameter(atr=0.0174533)          !fattore di conversione da gradi a radianti  
real::convff                      
parameter(convff=1.94)            !fattore di conversione da KT a m/s
real::psva                        !Hpa
parameter(psva=6.1078)            !pressione di saturazione del vapore acqueo
real::g                           !m/sec2
parameter(g=9.80665)              !costante gravitazionale
real::c1                          !c1= 1-(Mv/Md) dove Mv=peso molecolare acqua
parameter(c1=0.378)               !Md=peso molecolare aria secca (28.9644 e 18.016
end module phisical_constant
