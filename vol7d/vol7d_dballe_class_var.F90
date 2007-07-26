!ciclo sul tipo dato


               do iiiii=1,ndativar/**/VOL7D_POLY_TYPES_V
                  if (.not.lvar/**/VOL7D_POLY_TYPES_V(iiiii))cycle
                  if (c_e(this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii)))then
                     call idba_set (this%handle,this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(iiiii)%btable , &
                          this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii))
                     write =.true.

#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES r
#include "vol7d_dballe_class_attr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES i
#include "vol7d_dballe_class_attr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES b
#include "vol7d_dballe_class_attr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES d
#include "vol7d_dballe_class_attr.F90"
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPES c
#include "vol7d_dballe_class_attr.F90"
#undef VOL7D_POLY_TYPES

                  end if

               end do

               if (write) call idba_prendilo (this%handle)

!#fine ciclo sul tipo dato
