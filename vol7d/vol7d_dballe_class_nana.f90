!type /**/VOL7D_POLY_TYPES_V

if (associated(this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V))then
   nanavar/**/VOL7D_POLY_TYPES_V=size(this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(:))
   allocate (lanavar/**/VOL7D_POLY_TYPES_V(nanavar/**/VOL7D_POLY_TYPES_V))
   lanavar/**/VOL7D_POLY_TYPES_V(nanavar/**/VOL7D_POLY_TYPES_V)=.false.
   if (present(anavar))then
      lanavar/**/VOL7D_POLY_TYPES_V(:)=.false.
      do  i=1,size(anavar)
         where (anavar(i) == this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(:)%btable)
            lanavar/**/VOL7D_POLY_TYPES_V(:)=.true.
         end where
      end do
   else
      lanavar/**/VOL7D_POLY_TYPES_V(:)=c_e(this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(:)%btable)
   end if
else
   allocate (lanavar/**/VOL7D_POLY_TYPES_V(0))
end if

if (associated(this%vol7d%anavarattr%/**/VOL7D_POLY_TYPES_V))then
   nanaattr/**/VOL7D_POLY_TYPES_V=size(this%vol7d%anaattr%/**/VOL7D_POLY_TYPES_V(:))
   allocate (lanaattr/**/VOL7D_POLY_TYPES_V(nanaattr/**/VOL7D_POLY_TYPES_V))
   lanaattr/**/VOL7D_POLY_TYPES_V(nanaattr/**/VOL7D_POLY_TYPES_V)=.false.
   if (present(anaattr))then
      lanaattr/**/VOL7D_POLY_TYPES_V(:)=.false.
      do  i=1,size(anaattr)
         where (anaattr(i) == this%vol7d%anaattr%/**/VOL7D_POLY_TYPES_V(:)%btable)
            lanaattr/**/VOL7D_POLY_TYPES_V(:)=.true.
         end where
      end do
   else
      lanaattr/**/VOL7D_POLY_TYPES_V(:)=c_e(this%vol7d%anaattr%/**/VOL7D_POLY_TYPES_V(:)%btable)
   end if
else
   allocate (lanaattr/**/VOL7D_POLY_TYPES_V(0))
end if

!# end type /**/VOL7D_POLY_TYPES_V

