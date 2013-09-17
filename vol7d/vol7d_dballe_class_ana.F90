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

if (nanavar/**/VOL7D_POLY_TYPES_V > 0 ) then

  do a=1,nanavar/**/VOL7D_POLY_TYPES_V



    if (.not.lanavar/**/VOL7D_POLY_TYPES_V/**/(a)) cycle
    if (.not.c_e(this%vol7d%volana/**/VOL7D_POLY_TYPES_V(i,a,iiiiii))) cycle
    

#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,"scrivo: "//this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(a)%btable//&
                                t2c(this%vol7d%volana/**/VOL7D_POLY_TYPES_V(i,a,iiiiii)))
#endif        

    ier=idba_set (this%handle,this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(a)%btable , &
     this%vol7d%volana/**/VOL7D_POLY_TYPES_V(i,a,iiiiii))

                                ! ci sono dei dati da scrivere, richiedo la scrittura
    write=.true.
    
    if (any(lanaattrr).or.any(lanaattri).or.any(lanaattrb).or.any(lanaattrd).or.any(lanaattrc))then
      
                                !print*,"eseguo prendilo per attributi"
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,"prendilo")
#endif        
      ier=idba_prendilo (this%handle)
      
                                !ho appena scritto quindi la scrittura non è più richiesta
      write=.false.

                                ! imposto a default nullo la scrittura degli attributi
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
#ifdef DEBUG
        CALL l4f_category_log(this%category,L4F_DEBUG,"critica")
#endif        
        ier=idba_critica (this%handle)
      end if
      
#ifdef DEBUG
      CALL l4f_category_log(this%category,L4F_DEBUG,"unset"//this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(a)%btable )
#endif        
      ier=idba_unset (this%handle,this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(a)%btable )
      
    end if
    
  end do
  
end if

                                !#fine ciclo sul tipo dato
