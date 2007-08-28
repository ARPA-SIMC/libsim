
if ( associated(this%/**/VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V/**/ ) .and. associated(that%/**/VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V/**/ ))then
  where ( this%/**/VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V/**/ == that%/**/VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V/**/ )
    that%/**/VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V/**/ = /**/VOL7D_POLY_TYPES_V/**/miss
  end where
end if

