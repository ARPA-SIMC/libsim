program demo

use grid_class

implicit none

type (grid) :: grigliato
type(grid_type) :: gtype

!leggo da qualche parte i dati su grigliato

gtype%type="regular_ll"

call init (grigliato,gtype)

grigliato%regular_ll%lon_min = -2
grigliato%regular_ll%lon_max = 24.
grigliato%regular_ll%lat_min = 35.
grigliato%regular_ll%lat_max = 51.
grigliato%regular_ll%component_flag=1
grigliato%dim_ll%nx = 10
grigliato%dim_ll%ny = 15


call grids_unproj(grigliato)

print*,grigliato%dim_ll%lat,grigliato%dim_ll%lat

call delete(grigliato)

end program demo
