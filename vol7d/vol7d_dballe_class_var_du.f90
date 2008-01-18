if (associated(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V)) then
do i =1,size(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V)

if (this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%btable(:1) == "*") then
  j=firsttrue(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%btable(2:) == dballevar(:)%btable)
else
  j=firsttrue(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%btable == dballevar(:)%btable)
end if

  if ( j > 0 )then
    if(.not.c_e(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%description))this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%description =  dballevar(j)%description
    if(.not.c_e(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%unit))this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%unit =  dballevar(j)%unit
  end if
end do
endif

