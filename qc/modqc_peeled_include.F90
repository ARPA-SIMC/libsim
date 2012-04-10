
function peeled/**/VOL7D_POLY_TYPES/**/VOL7D_POLY_SUBTYPES(data,flag0,flag1,flag2,flag3)

/**/VOL7D_POLY_TYPE, intent(in) :: data(:,:,:,:,:)
/**/VOL7D_POLY_SUBTYPE, intent(in),pointer :: flag0(:,:,:,:,:)
/**/VOL7D_POLY_SUBTYPE, intent(in),pointer :: flag1(:,:,:,:,:)
/**/VOL7D_POLY_SUBTYPE, intent(in),pointer :: flag2(:,:,:,:,:)
/**/VOL7D_POLY_SUBTYPE, intent(in),pointer :: flag3(:,:,:,:,:)
/**/VOL7D_POLY_TYPE :: peeled/**/VOL7D_POLY_TYPES/**/VOL7D_POLY_SUBTYPES(size(data,1),size(data,2),size(data,3),size(data,4),size(data,5))

/**/VOL7D_POLY_SUBTYPE :: flag0l
/**/VOL7D_POLY_SUBTYPE :: flag1l
/**/VOL7D_POLY_SUBTYPE :: flag2l
/**/VOL7D_POLY_SUBTYPE :: flag3l

integer :: i,j,k,l,m


do m=1,size(data,5)
  do l=1,size(data,4)
    do k=1,size(data,3)
      do j=1,size(data,2)
        do i=1,size(data,1)
          
          if (associated(flag0))then
            flag0l=flag0(i,j,k,l,m)
          else
            flag0l=/**/VOL7D_POLY_SUBTYPES/**/miss
          end if

          if (associated(flag1))then
            flag1l=flag1(i,j,k,l,m)
          else
            flag1l=/**/VOL7D_POLY_SUBTYPES/**/miss
          end if

          if (associated(flag2))then
            flag2l=flag2(i,j,k,l,m)
          else
            flag2l=/**/VOL7D_POLY_SUBTYPES/**/miss
          end if

          if (associated(flag3))then
            flag3l=flag3(i,j,k,l,m)
          else
            flag3l=/**/VOL7D_POLY_SUBTYPES/**/miss
          end if

          if (qcsummaryflag/**/VOL7D_POLY_SUBTYPES(flag0l,flag1l,flag2l,flag3l)) then
            peeled/**/VOL7D_POLY_TYPES/**/VOL7D_POLY_SUBTYPES(i,j,k,l,m)=data(i,j,k,l,m)
          else
            peeled/**/VOL7D_POLY_TYPES/**/VOL7D_POLY_SUBTYPES(i,j,k,l,m)=/**/VOL7D_POLY_TYPES/**/miss
          end if

        end do
      end do
    end do
  end do
end do

end function peeled/**/VOL7D_POLY_TYPES/**/VOL7D_POLY_SUBTYPES



