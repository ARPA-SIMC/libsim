!>\brief  classe per la gestione di volumi di dati regolari (gridded)
!!
!!Questo modulo definisce gli oggetti e i metodi per gestire
!!l'importazione e l'esportazione di volumi regolari (gridded) 
!!e della loro gestione nei sistemi di coordinate geografiche e proiezioni


module grid_class

use regular_ll_class
use rotated_ll_class

implicit none


type grid_type

  character(len=80) :: type

end type grid_type


!>\brief definizione del grigliato in genere
type grid

  type(grid_type)   :: type
  type(grid_dim_ll) :: dim_ll
  type(grid_regular_ll) :: regular_ll
  type(grid_rotated_ll) :: rotated_ll

end type grid


INTERFACE init
  MODULE PROCEDURE init_grid
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_grid
END INTERFACE


private

public grids_proj,grids_unproj, grid,grid_type,init,delete

contains



subroutine init_grid(this,gtype)

type(grid) :: this
type(grid_type) :: gtype

this%type=gtype


select case ( this%type%type)

case ( "regular_ll")
  call init(this%regular_ll,this%dim_ll)

case ( "rotated_ll")
  call init(this%rotated_ll,this%dim_ll)
  
case default
  print *,this%type%type," non gestita"
  call exit (1)

end select

end subroutine init_grid




subroutine delete_grid(this)
type(grid) :: this

select case ( this%type%type)

case ( "regular_ll")
  call delete(this%regular_ll,this%dim_ll)

case ( "rotated_ll")
  call delete(this%rotated_ll,this%dim_ll)
  
case default
  print *,this%type%type," non gestita"
  call exit (1)

end select

end subroutine delete_grid




subroutine grids_proj (this)

type(grid) :: this


select case ( this%type%type)

case ( "regular_ll")
  call grid_proj(this%regular_ll,this%dim_ll)

case ( "rotated_ll")
  call grid_proj(this%rotated_ll,this%dim_ll)
  
case default
  print *,this%type%type," non gestita"
  call exit (1)

end select

end subroutine grids_proj


subroutine grids_unproj (this)

type(grid) ::this

select case ( this%type%type)

case ( "regular_ll")
  call grid_unproj(this%regular_ll, this%dim_ll)

case ( "rotated_ll")
  call grid_unproj(this%rotated_ll, this%dim_ll)

case default
  print *,this%type%type," non gestita"
  call exit (1)

end select

end subroutine grids_unproj


end module grid_class



