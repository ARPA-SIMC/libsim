SUBROUTINE vol7d_merge_final/**/VOL7D_POLY_TYPES(this, that, v7dtmp, &
 remapa1, remapa2, remapt1, remapt2, remapl1, remapl2, &
 remaptr1, remaptr2, remapn1, remapn2)
TYPE(vol7d),INTENT(inout) :: this, that
TYPE(vol7d),INTENT(inout) :: v7dtmp
INTEGER,INTENT(in) :: remapa1(:), remapa2(:), remapt1(:), remapt2(:), &
 remapl1(:), remapl2(:), remaptr1(:), remaptr2(:), remapn1(:), remapn2(:)

INTEGER,POINTER :: remapv1(:), remapv2(:), remapva1(:), remapva2(:)

NULLIFY(remapv1, remapv2, remapva1, remapva2)

! 3d
CALL vol7d_pre_remap_var(this%anavar%/**/VOL7D_POLY_TYPES, &
 that%anavar%/**/VOL7D_POLY_TYPES, v7dtmp%anavar%/**/VOL7D_POLY_TYPES, &
 remapv1, remapv2)
IF (ASSOCIATED(v7dtmp%anavar%/**/VOL7D_POLY_TYPES)) THEN
  CALL vol7d_alloc_vol(v7dtmp)
  IF (ASSOCIATED(remapv1)) THEN
    v7dtmp%volana/**/VOL7D_POLY_TYPES(remapa1(:),remapv1(:),remapn1(:)) = &
     this%volana/**/VOL7D_POLY_TYPES(:,:,:)
    DEALLOCATE(remapv1)
  ENDIF
  IF (ASSOCIATED(remapv2)) THEN
    v7dtmp%volana/**/VOL7D_POLY_TYPES(remapa2(:),remapv2(:),remapn2(:)) = &
     that%volana/**/VOL7D_POLY_TYPES(:,:,:)
    DEALLOCATE(remapv2)
  ENDIF
ENDIF

! 4d
CALL vol7d_pre_remap_var(this%anaattr%/**/VOL7D_POLY_TYPES, &
 that%anaattr%/**/VOL7D_POLY_TYPES, v7dtmp%anaattr%/**/VOL7D_POLY_TYPES, &
 remapv1, remapv2)
CALL vol7d_pre_remap_var(this%anavarattr%/**/VOL7D_POLY_TYPES, &
 that%anavarattr%/**/VOL7D_POLY_TYPES, v7dtmp%anavarattr%/**/VOL7D_POLY_TYPES, &
 remapva1, remapva2)
IF (ASSOCIATED(v7dtmp%anaattr%/**/VOL7D_POLY_TYPES) .AND. &
 ASSOCIATED(v7dtmp%anavarattr%/**/VOL7D_POLY_TYPES)) THEN
  CALL vol7d_alloc_vol(v7dtmp)
  IF (ASSOCIATED(remapv1) .AND. ASSOCIATED(remapva1)) THEN
    v7dtmp%volanaattr/**/VOL7D_POLY_TYPES &
     (remapa1(:),remapva1(:),remapn1(:),remapv1(:)) = &
     this%volanaattr/**/VOL7D_POLY_TYPES(:,:,:,:)
    DEALLOCATE(remapv1, remapva1)
  ELSE IF (ASSOCIATED(remapv1)) THEN
    DEALLOCATE(remapv1)
  ELSE IF (ASSOCIATED(remapva1)) THEN
    DEALLOCATE(remapva1)
  ENDIF
  IF (ASSOCIATED(remapv2) .AND. ASSOCIATED(remapva2)) THEN
    v7dtmp%volanaattr/**/VOL7D_POLY_TYPES &
     (remapa2(:),remapva2(:),remapn2(:),remapv2(:)) = &
     that%volanaattr/**/VOL7D_POLY_TYPES(:,:,:,:)
    DEALLOCATE(remapv2)
  ELSE IF (ASSOCIATED(remapv2)) THEN
    DEALLOCATE(remapv2)
  ELSE IF (ASSOCIATED(remapva2)) THEN
    DEALLOCATE(remapva2)
  ENDIF
ENDIF

! 6d
CALL vol7d_pre_remap_var(this%dativar%/**/VOL7D_POLY_TYPES, &
 that%dativar%/**/VOL7D_POLY_TYPES, v7dtmp%dativar%/**/VOL7D_POLY_TYPES, &
 remapv1, remapv2)
IF (ASSOCIATED(v7dtmp%dativar%/**/VOL7D_POLY_TYPES)) THEN
  CALL vol7d_alloc_vol(v7dtmp)
  IF (ASSOCIATED(remapv1)) THEN
    v7dtmp%voldati/**/VOL7D_POLY_TYPES &
     (remapa1(:),remapt1(:),remapl1(:),remaptr1(:),remapv1(:),remapn1(:)) = &
     this%voldati/**/VOL7D_POLY_TYPES(:,:,:,:,:,:)
    DEALLOCATE(remapv1)
  ENDIF
  IF (ASSOCIATED(remapv2)) THEN
    v7dtmp%voldati/**/VOL7D_POLY_TYPES &
     (remapa2(:),remapt2(:),remapl2(:),remaptr2(:),remapv2(:),remapn2(:)) = &
     that%voldati/**/VOL7D_POLY_TYPES(:,:,:,:,:,:)
    DEALLOCATE(remapv2)
  ENDIF
ENDIF

! 7d
CALL vol7d_pre_remap_var(this%datiattr%/**/VOL7D_POLY_TYPES, &
 that%datiattr%/**/VOL7D_POLY_TYPES, v7dtmp%datiattr%/**/VOL7D_POLY_TYPES, &
 remapv1, remapv2)
CALL vol7d_pre_remap_var(this%dativarattr%/**/VOL7D_POLY_TYPES, &
 that%dativarattr%/**/VOL7D_POLY_TYPES, v7dtmp%dativarattr%/**/VOL7D_POLY_TYPES, &
 remapva1, remapva2)
IF (ASSOCIATED(v7dtmp%datiattr%/**/VOL7D_POLY_TYPES) .AND. &
 ASSOCIATED(v7dtmp%dativarattr%/**/VOL7D_POLY_TYPES)) THEN
  CALL vol7d_alloc_vol(v7dtmp)
  IF (ASSOCIATED(remapv1) .AND. ASSOCIATED(remapva1)) THEN
    v7dtmp%voldatiattr/**/VOL7D_POLY_TYPES &
     (remapa1(:),remapt1(:),remapl1(:),remaptr1(:),remapva1(:),remapn1(:),remapv1(:)) = &
     this%voldatiattr/**/VOL7D_POLY_TYPES(:,:,:,:,:,:,:)
    DEALLOCATE(remapv1, remapva1)
  ELSE IF (ASSOCIATED(remapv1)) THEN
    DEALLOCATE(remapv1)
  ELSE IF (ASSOCIATED(remapva1)) THEN
    DEALLOCATE(remapva1)
  ENDIF
  IF (ASSOCIATED(remapv2) .AND. ASSOCIATED(remapva2)) THEN
    v7dtmp%voldatiattr/**/VOL7D_POLY_TYPES &
     (remapa2(:),remapt1(:),remapl1(:),remaptr1(:),remapva2(:),remapn2(:),remapv2(:)) = &
     that%voldatiattr/**/VOL7D_POLY_TYPES(:,:,:,:,:,:,:)
    DEALLOCATE(remapv2)
  ELSE IF (ASSOCIATED(remapv2)) THEN
    DEALLOCATE(remapv2)
  ELSE IF (ASSOCIATED(remapva2)) THEN
    DEALLOCATE(remapva2)
  ENDIF
ENDIF


END SUBROUTINE vol7d_merge_final/**/VOL7D_POLY_TYPES


!! datiana(ana,anavar,network)
!! attrdatiana(ana,attranavar,network,attr)
!! dati(ana,time,level,timerange,dativar,network)
!! attrdati(ana,time,level,timerange,attrdativar,network,attr)
!!$  DO i3 = 1, SIZE(v7dtmp%network)
!!$    DO i2 = 1, SIZE(v7dtmp%var?)
!!$      DO i1 = 1, SIZE(v7dtmp%ana)
!!$        v7dtmp%voldatiana(remapa1(i1),remapv1?(i2),remapn1(i3)) = &
!!$         this%voldatiana(i1,i2,i3)
!!$        v7dtmp%voldatiana(remapa2(i1),remapv2?(i2),remapn2(i3)) = &
!!$         that%voldatiana(i1,i2,i3)
!!$      ENDDO
!!$    ENDDO
!!$  ENDDO

