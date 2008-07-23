

!ciclo sul tipo dato

!if (associated(this%vol7d%voldati/**/VOL7D_POLY_TYPES_V)) then

do iiiii=1,ndativar/**/VOL7D_POLY_TYPES_V

  if (.not.lvar/**/VOL7D_POLY_TYPES_V/**/(iiiii)) cycle
  if (.not.c_e(this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii))) cycle
    
  !print*,"scrivo",this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable,&
  ! this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii)

  
  if (lattr_only) then
                                !print*,i,ii,iii,iiii,iiiiii
                                !print*,"context_id -->",this%data_id(i,ii,iii,iiii,iiiiii)
    if (.not. c_e(this%data_id(i,ii,iii,iiii,iiiiii))) cycle
    if (.not. c_e(this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable )) cycle

    !print*,"*context_id",this%data_id(i,ii,iii,iiii,iiiiii)
    !print*,"*var_related",this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable

    call idba_set (this%handle,"*context_id",this%data_id(i,ii,iii,iiii,iiiiii))
    call idba_set (this%handle,"*var_related",this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable )

  else


    call l4f_category_log(this%category,L4F_DEBUG,"setto: "//to_char(this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable)// &
     to_char(this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii)))

    call idba_set (this%handle,this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable , &
     this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii))
    write =.true.

  end if

  if (any(lattrr).or.any(lattri).or.any(lattrb).or.any(lattrd).or.any(lattrc))then

    !print*,"eseguo prendilo per attributi"
    if (write) call idba_prendilo (this%handle)

    write=.false.
    writeattr=.false.
    
    
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES r
#include "vol7d_dballe_class_datiattr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES i
#include "vol7d_dballe_class_datiattr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES b
#include "vol7d_dballe_class_datiattr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES d
#include "vol7d_dballe_class_datiattr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES c
#include "vol7d_dballe_class_datiattr.F90"
#undef VOL7D_POLY_TYPES

    if (writeattr) then
      !print *,"critica"
      call idba_critica (this%handle)
    end if
      
    !print*,"unset",this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable 
    call idba_unset (this%handle,this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable )
    
  end if
  
end do

!end if

!#fine ciclo sul tipo dato


