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


!ciclo sul tipo dato

!if (associated(this%vol7d%voldati/**/VOL7D_POLY_TYPES_V)) then

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"ndativar*: "//to_char(ndativar/**/VOL7D_POLY_TYPES_V))
#endif

do iiiii=1,ndativar/**/VOL7D_POLY_TYPES_V

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,"ndativar*: "//&
   to_char(ndativar/**/VOL7D_POLY_TYPES_V ))
  call l4f_category_log(this%category,L4F_DEBUG,"indice iiiii: "//to_char(iiiii))
#endif
  
  if (.not.lvar/**/VOL7D_POLY_TYPES_V/**/(iiiii)) cycle
  if (.not.c_e(this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii))) cycle
    
  !print*,"scrivo",this%vol7d%dativar% VOL7D_POLY_TYPES_V(iiiii)%btable,&
  ! this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii)

  if (lattr_only) then
                                !print*,i,ii,iii,iiii,iiiiii
                                !print*,"context_id -->",this%data_id(i,ii,iii,iiii,iiiiii)
    if (.not. c_e(this%data_id(i,ii,iii,iiii,iiiiii))) cycle
    if (.not. c_e(this%vol7d%dativar% VOL7D_POLY_TYPES_V(iiiii)%btable )) cycle

    !print*,"*context_id",this%data_id(i,ii,iii,iiii,iiiiii)
    !print*,"*var_related",this%vol7d%dativar% VOL7D_POLY_TYPES_V(iiiii)%btable

    call idba_set (this%handle,"*context_id",this%data_id(i,ii,iii,iiii,iiiiii))
    call idba_set (this%handle,"*var_related",this%vol7d%dativar% VOL7D_POLY_TYPES_V(iiiii)%btable )

  else


#ifdef DEBUG
     CALL l4f_category_log(this%category,L4F_DEBUG,"setto: "&
          //to_char(this%vol7d%dativar% VOL7D_POLY_TYPES_V(iiiii)%btable)//' '// &
          to_char(this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii)))
#endif

    call idba_set (this%handle,this%vol7d%dativar% VOL7D_POLY_TYPES_V(iiiii)%btable , &
     this%vol7d%voldati/**/VOL7D_POLY_TYPES_V(i,ii,iii,iiii,iiiii,iiiiii))
    write =.true.

  end if

  if (any(lattrr).or.any(lattri).or.any(lattrb).or.any(lattrd).or.any(lattrc))then

#ifdef DEBUG
     CALL l4f_category_log(this%category,L4F_DEBUG,&
          "I have attributes to write")
#endif


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
      
    !print*,"unset",this%vol7d%dativar% VOL7D_POLY_TYPES_V(iiiii)%btable 
    call idba_unset (this%handle,this%vol7d%dativar% VOL7D_POLY_TYPES_V(iiiii)%btable )
    
  end if
  
end do

!end if

!#fine ciclo sul tipo dato


