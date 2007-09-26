module phisical_constant

real::eps
parameter(eps=0.622)
real::ra
parameter(ra=287.05)              !costante dei gas per aria secca
real::abz
parameter(abz=273.15)             !0 gradi celsius in kelvin
real::rta
parameter(RTA=57.2957795)         !raggio terrestre in metri
real::aomega
parameter(aomega=7.292*10.**-5)   !velocita' angolare della terra

end module phisical_constant
