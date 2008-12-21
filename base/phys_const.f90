!> \brief Costanti fisiche.
!!
!! Questo modulo definisce delle costanti fisiche di uso comune
!! in meteorologia e discipline affini. Attualmente esse sono definite
!! solo in singola precisione, sarebbe opportuno trovare un metodo
!! elegante per definirle semiautomaticamente anche in doppia precisione
!! affinché possano essere usate, ad esempio, dalle routine di
!! conversione geo-UTM della classe geo_coord_class.
!! \ingroup base
MODULE phys_const
IMPLICIT NONE

! pi = 4.*ATAN(1.)
REAL, PARAMETER :: pi = 3.14159 !< pi greco (\f$\pi\f$)
REAL, PARAMETER :: rearth = 6370997. !< raggio medio della Terra (\f$m\f$)
REAL, PARAMETER :: gearth = 9.81 !< accelerazione gravitazionale media alla superficie terrestre (\f$m/s^2\f$)
REAL, PARAMETER :: omearth = 2*pi/86164. !< velocità angolare terrestre (\f$s^{-1}\f$)
REAL, PARAMETER :: degrad = pi/180. !< fattore di conversione da gradi a radianti
REAL, PARAMETER :: raddeg = 180./pi !< fattore di conversione da radianti a gradi
REAL, PARAMETER :: rd = 287.05 !< costante dei gas per l'aria secca
REAL, PARAMETER :: rv = 461.51 !< costante dei gas per il vapore acqueo
REAL, PARAMETER :: eps0 = rd/rv !< \f$rd/rv\f$
REAL, PARAMETER :: epsy = rv/rd-1. !< \f$rv/rd -1\f$
REAL, PARAMETER :: rcp = 2./7. !< rapporto tra calore specifico...?
REAL, PARAMETER :: cpd = rd/rcp !< calore specifico dell'aria secca a pressione costante
REAL, PARAMETER :: cvd = cpd-rd !< calore specifico dell'aria secca a volume costante
REAL, PARAMETER :: lvw = 2.5E+6 !< calore latente...?

END MODULE phys_const

MODULE doubleprecision_phys_const
IMPLICIT NONE

! pi = 4.*ATAN(1.)
DOUBLEPRECISION, PARAMETER :: pi = 3.14159 !< pi greco (\f$\pi\f$)
DOUBLEPRECISION, PARAMETER :: rearth = 6370997. !< raggio medio della Terra (\f$m\f$)
DOUBLEPRECISION, PARAMETER :: gearth = 9.81 !< accelerazione gravitazionale media alla superficie terrestre (\f$m/s^2\f$)
DOUBLEPRECISION, PARAMETER :: omearth = 2*pi/86164. !< velocità angolare terrestre (\f$s^{-1}\f$)
DOUBLEPRECISION, PARAMETER :: degrad = pi/180. !< fattore di conversione da gradi a radianti
DOUBLEPRECISION, PARAMETER :: raddeg = 180./pi !< fattore di conversione da radianti a gradi
DOUBLEPRECISION, PARAMETER :: rd = 287.05 !< costante dei gas per l'aria secca
DOUBLEPRECISION, PARAMETER :: rv = 461.51 !< costante dei gas per il vapore acqueo
DOUBLEPRECISION, PARAMETER :: eps0 = rd/rv !< \f$rd/rv\f$
DOUBLEPRECISION, PARAMETER :: epsy = rv/rd-1. !< \f$rv/rd -1\f$
DOUBLEPRECISION, PARAMETER :: rcp = 2./7. !< rapporto tra calore specifico...?
DOUBLEPRECISION, PARAMETER :: cpd = rd/rcp !< calore specifico dell'aria secca a pressione costante
DOUBLEPRECISION, PARAMETER :: cvd = cpd-rd !< calore specifico dell'aria secca a volume costante
DOUBLEPRECISION, PARAMETER :: lvw = 2.5E+6 !< calore latente...?

END MODULE doubleprecision_phys_const
