MODULE phys_const
IMPLICIT NONE

!omstart phys_const
!idx Costanti fisiche di pubblica utilit&agrave;
!Questo modulo definisce le costanti fisiche di uso
!pi&ugrave; comune in meteorologia:
!
!pi      => pi greco
!rearth  => raggio medio della Terra in m
!gearth  => accelerazione gravitazionale media alla superficie
!           terrestre (m/s**2)
!omearth => velocit&agrave; angolare terrestre (s**-1)
!degrad  => fattore di conversione da gradi a radianti
!raddeg  => fattore di conversione da radianti a gradi
!rd      => Costante dei gas per l'aria secca
!rv      => Costante dei gas per il vapore acqueo
!eps0    => rd/rv
!epsy    => rv/rd -1.
!rcp     => Rapporto tra calore specifico...?
!cpd     => Calore specifico dell'aria secca a pressione costante
!cvd     => Calore specifico dell'aria secca a volume costante
!lvw     => Calore latente...?
!
!omend


REAL, PARAMETER :: &
! pi = 4.*ATAN(1.), &
 pi = 3.14159, &
 rearth = 6370997., &
 gearth = 9.81, &
 omearth = 2*pi/86164., &
 degrad = pi/180., &
 raddeg = 180./pi, &
 rd = 287.05, &
 rv = 461.51, &
 eps0 = rd/rv, &
 epsy = rv/rd-1., &
 rcp = 2./7., &
 cpd = rd/rcp, &
 cvd = cpd-rd, &
 lvw = 2.5E+6

END MODULE phys_const
