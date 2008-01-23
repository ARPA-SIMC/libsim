!type VOL7D_POLY_TYPES_V

if (associated(this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V))then
   ndativar/**/VOL7D_POLY_TYPES_V=size(this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(:))
   allocate (lvar/**/VOL7D_POLY_TYPES_V(ndativar/**/VOL7D_POLY_TYPES_V))
   lvar/**/VOL7D_POLY_TYPES_V(ndativar/**/VOL7D_POLY_TYPES_V)=.false.
   if (present(var))then
      lvar/**/VOL7D_POLY_TYPES_V(:)=.false.
      do  i=1,size(var)
         where (var(i) == this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(:)%btable)
            lvar/**/VOL7D_POLY_TYPES_V(:)=.true.
         end where
      end do
   else
     lvar/**/VOL7D_POLY_TYPES_V(:)=.true.
   end if
else
   allocate (lvar/**/VOL7D_POLY_TYPES_V(0))
end if

if (associated(this%vol7d%dativarattr%/**/VOL7D_POLY_TYPES_V))then
   ndatiattr/**/VOL7D_POLY_TYPES_V=size(this%vol7d%datiattr%/**/VOL7D_POLY_TYPES_V(:))
   allocate (lattr/**/VOL7D_POLY_TYPES_V(ndatiattr/**/VOL7D_POLY_TYPES_V))
   lattr/**/VOL7D_POLY_TYPES_V(ndatiattr/**/VOL7D_POLY_TYPES_V)=.false.
   if (present(attr))then
      lattr/**/VOL7D_POLY_TYPES_V(:)=.false.
      do  i=1,size(attr)
         where (attr(i) == this%vol7d%datiattr%/**/VOL7D_POLY_TYPES_V(:)%btable)
            lattr/**/VOL7D_POLY_TYPES_V(:)=.true.
         end where
      end do
   else
      lattr/**/VOL7D_POLY_TYPES_V(:)=.true.
   end if
else
   allocate (lattr/**/VOL7D_POLY_TYPES_V(0))
end if

!# end type  /**/VOL7D_POLY_TYPES_V

