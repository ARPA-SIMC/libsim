!ciclo sul tipo dato


do ii=1,nanavar/**/VOL7D_POLY_TYPES_V

  if (.not.lanavar/**/VOL7D_POLY_TYPES_V/**/(ii)) cycle
  if (.not.c_e(this%vol7d%volana/**/VOL7D_POLY_TYPES_V(i,ii,iii))) cycle
    
  !print*,"scrivo",this%vol7d%datiana%/**/VOL7D_POLY_TYPES_V(iiiii)%btable,&
   !this%vol7d%volana/**/VOL7D_POLY_TYPES_V(i,ii,iii)

  call idba_set (this%handle,this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(ii)%btable , &
   this%vol7d%volana/**/VOL7D_POLY_TYPES_V(i,ii,iii))

  write =.true.

  if (any(lanaattrr).or.any(lanaattri).or.any(lanaattrb).or.any(lanaattrd).or.any(lanaattrc))then

    !print*,"eseguo prendilo per attributi"
    call idba_prendilo (this%handle)
    call idba_enq (this%handle,"ana_id",ana_id(i,iii))

    write=.false.
    writeattr=.false.
    
    
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES r
#include "vol7d_dballe_class_anaattr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES i
#include "vol7d_dballe_class_anaattr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES b
#include "vol7d_dballe_class_anaattr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES d
#include "vol7d_dballe_class_anaattr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES c
#include "vol7d_dballe_class_anaattr.F90"
#undef VOL7D_POLY_TYPES

    if (writeattr) then
      !print *,"critica"
      call idba_critica (this%handle)
    end if
      
    !print*,"unset",this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(ii)%btable 
    call idba_unset (this%handle,this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(ii)%btable )
    
  end if
  
end do


!#fine ciclo sul tipo dato

