SUBROUTINE vol7d_merge_final/**/VOL7D_POLY_TYPES(this, that, v7dtmp, &
 remapa1, remapa2, remapt1, remapt2, remapl1, remapl2, &
 remaptr1, remaptr2, remapn1, remapn2)
TYPE(vol7d),INTENT(inout) :: this, that
TYPE(vol7d),INTENT(inout) :: v7dtmp
INTEGER,INTENT(in) :: remapa1(:), remapa2(:), remapt1(:), remapt2(:), &
 remapl1(:), remapl2(:), remaptr1(:), remaptr2(:), remapn1(:), remapn2(:)

INTEGER,POINTER :: remapv1(:), remapv2(:), remapva1(:), remapva2(:)

! 3d
CALL vol7d_remap2_vol7d_var(this%anavar%/**/VOL7D_POLY_TYPES, &
 that%anavar%/**/VOL7D_POLY_TYPES, v7dtmp%anavar%/**/VOL7D_POLY_TYPES, &
 .FALSE., remapv1, remapv2)
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
CALL vol7d_remap2_vol7d_var(this%anaattr%/**/VOL7D_POLY_TYPES, &
 that%anaattr%/**/VOL7D_POLY_TYPES, v7dtmp%anaattr%/**/VOL7D_POLY_TYPES, &
 .FALSE., remapv1, remapv2)
CALL vol7d_remap2_vol7d_var(this%anavarattr%/**/VOL7D_POLY_TYPES, &
 that%anavarattr%/**/VOL7D_POLY_TYPES, v7dtmp%anavarattr%/**/VOL7D_POLY_TYPES, &
 .FALSE., remapva1, remapva2)
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
CALL vol7d_remap2_vol7d_var(this%dativar%/**/VOL7D_POLY_TYPES, &
 that%dativar%/**/VOL7D_POLY_TYPES, v7dtmp%dativar%/**/VOL7D_POLY_TYPES, &
 .FALSE., remapv1, remapv2)
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
CALL vol7d_remap2_vol7d_var(this%datiattr%/**/VOL7D_POLY_TYPES, &
 that%datiattr%/**/VOL7D_POLY_TYPES, v7dtmp%datiattr%/**/VOL7D_POLY_TYPES, &
 .FALSE., remapv1, remapv2)
CALL vol7d_remap2_vol7d_var(this%dativarattr%/**/VOL7D_POLY_TYPES, &
 that%dativarattr%/**/VOL7D_POLY_TYPES, v7dtmp%dativarattr%/**/VOL7D_POLY_TYPES, &
 .FALSE., remapva1, remapva2)
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


SUBROUTINE vol7d_reform_final/**/VOL7D_POLY_TYPES(this, v7dtmp, &
 remapa, remapt, remapl, remaptr, remapn, miss)
TYPE(vol7d),INTENT(inout) :: this, v7dtmp
INTEGER,INTENT(in) :: remapa(:), remapt(:), remapl(:), remaptr(:), remapn(:)
LOGICAL,INTENT(in) :: miss

INTEGER,POINTER :: remapv(:), remapva(:)

! 3d
CALL vol7d_remap1_vol7d_var(this%anavar%/**/VOL7D_POLY_TYPES, &
 v7dtmp%anavar%/**/VOL7D_POLY_TYPES, .FALSE., miss, remapv)
IF (ASSOCIATED(v7dtmp%anavar%/**/VOL7D_POLY_TYPES)) THEN
  CALL vol7d_alloc_vol(v7dtmp)
  v7dtmp%volana/**/VOL7D_POLY_TYPES(:,:,:) = &
   this%volana/**/VOL7D_POLY_TYPES(remapa(:),remapv(:),remapn(:))
  DEALLOCATE(remapv)
ENDIF

! 4d
CALL vol7d_remap1_vol7d_var(this%anaattr%/**/VOL7D_POLY_TYPES, &
 v7dtmp%anaattr%/**/VOL7D_POLY_TYPES, .FALSE., miss, remapv)
CALL vol7d_remap1_vol7d_var(this%anavarattr%/**/VOL7D_POLY_TYPES, &
 v7dtmp%anavarattr%/**/VOL7D_POLY_TYPES, .FALSE., miss, remapva)
IF (ASSOCIATED(v7dtmp%anaattr%/**/VOL7D_POLY_TYPES) .AND. &
 ASSOCIATED(v7dtmp%anavarattr%/**/VOL7D_POLY_TYPES)) THEN
  CALL vol7d_alloc_vol(v7dtmp)
  v7dtmp%volanaattr/**/VOL7D_POLY_TYPES(:,:,:,:) = &
   this%volanaattr/**/VOL7D_POLY_TYPES &
   (remapa(:),remapva(:),remapn(:),remapv(:))
  DEALLOCATE(remapv, remapva)
ELSE IF (ASSOCIATED(remapv)) THEN
  DEALLOCATE(remapv)
ELSE IF (ASSOCIATED(remapva)) THEN
  DEALLOCATE(remapva)
ENDIF

! 6d
CALL vol7d_remap1_vol7d_var(this%dativar%/**/VOL7D_POLY_TYPES, &
 v7dtmp%dativar%/**/VOL7D_POLY_TYPES, .FALSE., miss, remapv)
IF (ASSOCIATED(v7dtmp%dativar%/**/VOL7D_POLY_TYPES)) THEN
  CALL vol7d_alloc_vol(v7dtmp)
  v7dtmp%voldati/**/VOL7D_POLY_TYPES(:,:,:,:,:,:) =  &
   this%voldati/**/VOL7D_POLY_TYPES &
   (remapa(:),remapt(:),remapl(:),remaptr(:),remapv(:),remapn(:))
  DEALLOCATE(remapv)
ENDIF

! 7d
CALL vol7d_remap1_vol7d_var(this%datiattr%/**/VOL7D_POLY_TYPES, &
 v7dtmp%datiattr%/**/VOL7D_POLY_TYPES, .FALSE., miss, remapv)
CALL vol7d_remap1_vol7d_var(this%dativarattr%/**/VOL7D_POLY_TYPES, &
 v7dtmp%dativarattr%/**/VOL7D_POLY_TYPES, .FALSE., miss, remapva)
IF (ASSOCIATED(v7dtmp%datiattr%/**/VOL7D_POLY_TYPES) .AND. &
 ASSOCIATED(v7dtmp%dativarattr%/**/VOL7D_POLY_TYPES)) THEN
  CALL vol7d_alloc_vol(v7dtmp)
  v7dtmp%voldatiattr/**/VOL7D_POLY_TYPES(:,:,:,:,:,:,:) = &
     this%voldatiattr/**/VOL7D_POLY_TYPES &
     (remapa(:),remapt(:),remapl(:),remaptr(:),remapva(:),remapn(:),remapv(:))
  DEALLOCATE(remapv, remapva)
ELSE IF (ASSOCIATED(remapv)) THEN
  DEALLOCATE(remapv)
ELSE IF (ASSOCIATED(remapva)) THEN
  DEALLOCATE(remapva)
ENDIF

END SUBROUTINE vol7d_reform_final/**/VOL7D_POLY_TYPES


SUBROUTINE vol7d_get_volana/**/VOL7D_POLY_TYPES(this, dimlist, vol1dp, &
 vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
TYPE(vol7d),INTENT(in) :: this
INTEGER,INTENT(in) :: dimlist(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol1dp(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol2dp(:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol3dp(:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol4dp(:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol5dp(:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol6dp(:,:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol7dp(:,:,:,:,:,:,:)

INTEGER :: voldim(vol7d_maxdim_ad)

IF (ASSOCIATED(this%volana/**/VOL7D_POLY_TYPES)) THEN
  voldim(1:SIZE(SHAPE((this%volana/**/VOL7D_POLY_TYPES)))) = SHAPE(this%volana/**/VOL7D_POLY_TYPES)
  voldim(SIZE(SHAPE(this%volana/**/VOL7D_POLY_TYPES))+1:) = 1

  CALL vol7d_get_vol/**/VOL7D_POLY_TYPES(this%volana/**/VOL7D_POLY_TYPES, voldim, dimlist, &
   vol1dp, vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
ELSE
  CALL vol7d_nullify/**/VOL7D_POLY_TYPES(vol1dp, vol2dp, vol3dp, vol4dp, &
   vol5dp, vol6dp, vol7dp)
ENDIF

END SUBROUTINE vol7d_get_volana/**/VOL7D_POLY_TYPES


SUBROUTINE vol7d_get_volanaattr/**/VOL7D_POLY_TYPES(this, dimlist, vol1dp, &
 vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
TYPE(vol7d),INTENT(in) :: this
INTEGER,INTENT(in) :: dimlist(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol1dp(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol2dp(:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol3dp(:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol4dp(:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol5dp(:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol6dp(:,:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol7dp(:,:,:,:,:,:,:)

INTEGER :: voldim(vol7d_maxdim_ad)

IF (ASSOCIATED(this%volanaattr/**/VOL7D_POLY_TYPES)) THEN
  voldim(1:SIZE(SHAPE((this%volanaattr/**/VOL7D_POLY_TYPES)))) = SHAPE(this%volanaattr/**/VOL7D_POLY_TYPES)
  voldim(SIZE(SHAPE(this%volanaattr/**/VOL7D_POLY_TYPES))+1:) = 1

  CALL vol7d_get_vol/**/VOL7D_POLY_TYPES(this%volanaattr/**/VOL7D_POLY_TYPES, voldim, dimlist, &
   vol1dp, vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
ELSE
  CALL vol7d_nullify/**/VOL7D_POLY_TYPES(vol1dp, vol2dp, vol3dp, vol4dp, &
   vol5dp, vol6dp, vol7dp)
ENDIF

END SUBROUTINE vol7d_get_volanaattr/**/VOL7D_POLY_TYPES


SUBROUTINE vol7d_get_voldati/**/VOL7D_POLY_TYPES(this, dimlist, vol1dp, &
 vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
TYPE(vol7d),INTENT(in) :: this
INTEGER,INTENT(in) :: dimlist(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol1dp(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol2dp(:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol3dp(:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol4dp(:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol5dp(:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol6dp(:,:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol7dp(:,:,:,:,:,:,:)

INTEGER :: voldim(vol7d_maxdim_ad)

IF (ASSOCIATED(this%voldati/**/VOL7D_POLY_TYPES)) THEN
  voldim(1:SIZE(SHAPE((this%voldati/**/VOL7D_POLY_TYPES)))) = SHAPE(this%voldati/**/VOL7D_POLY_TYPES)
  voldim(SIZE(SHAPE(this%voldati/**/VOL7D_POLY_TYPES))+1:) = 1

  CALL vol7d_get_vol/**/VOL7D_POLY_TYPES(this%voldati/**/VOL7D_POLY_TYPES, voldim, dimlist, &
   vol1dp, vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
ELSE
  CALL vol7d_nullify/**/VOL7D_POLY_TYPES(vol1dp, vol2dp, vol3dp, vol4dp, &
   vol5dp, vol6dp, vol7dp)
ENDIF

END SUBROUTINE vol7d_get_voldati/**/VOL7D_POLY_TYPES


SUBROUTINE vol7d_get_voldatiattr/**/VOL7D_POLY_TYPES(this, dimlist, vol1dp, &
 vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
TYPE(vol7d),INTENT(in) :: this
INTEGER,INTENT(in) :: dimlist(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol1dp(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol2dp(:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol3dp(:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol4dp(:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol5dp(:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol6dp(:,:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol7dp(:,:,:,:,:,:,:)

INTEGER :: voldim(vol7d_maxdim_ad)

IF (ASSOCIATED(this%voldatiattr/**/VOL7D_POLY_TYPES)) THEN
  voldim(1:SIZE(SHAPE((this%voldatiattr/**/VOL7D_POLY_TYPES)))) = SHAPE(this%voldatiattr/**/VOL7D_POLY_TYPES)
!  voldim(SIZE(SHAPE(this%voldatiattr/**/VOL7D_POLY_TYPES))+1:) = 1

  CALL vol7d_get_vol/**/VOL7D_POLY_TYPES(this%voldatiattr/**/VOL7D_POLY_TYPES, voldim, dimlist, &
   vol1dp, vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
ELSE
  CALL vol7d_nullify/**/VOL7D_POLY_TYPES(vol1dp, vol2dp, vol3dp, vol4dp, &
   vol5dp, vol6dp, vol7dp)
ENDIF

END SUBROUTINE vol7d_get_voldatiattr/**/VOL7D_POLY_TYPES


SUBROUTINE vol7d_get_vol/**/VOL7D_POLY_TYPES(vol, volshp, dimlist, vol1dp, &
 vol2dp, vol3dp, vol4dp, vol5dp, vol6dp, vol7dp)
INTEGER :: volshp(:), dimlist(:)
VOL7D_POLY_TYPE :: vol(volshp(1),volshp(2),volshp(3),volshp(4),volshp(5), &
 volshp(6),volshp(7))
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol1dp(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol2dp(:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol3dp(:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol4dp(:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol5dp(:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol6dp(:,:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol7dp(:,:,:,:,:,:,:)

INTEGER :: shpr(vol7d_maxdim_ad), ndimr
LOGICAL :: qualidim(vol7d_maxdim_ad)

IF (MAXVAL(dimlist) > vol7d_maxdim_ad .OR. MINVAL(dimlist) < 1 &
 .OR. SIZE(dimlist) > vol7d_maxdim_ad) THEN
  CALL raise_error('dimensioni non valide '//to_char(dimlist))
  RETURN
ENDIF
qualidim = .FALSE.
qualidim(dimlist) = .TRUE.

ndimr = SIZE(dimlist)
!shp = SHAPE(this%voldatir)

IF (ANY(.NOT.qualidim .AND. volshp /= 1) ) THEN
  CALL raise_error('dimensioni non degeneri o nulle non richieste: '// &
   to_char(PACK(volshp, mask=(.NOT.qualidim .AND. volshp /= 1))))
  RETURN
ENDIF

shpr(1:ndimr) = PACK(volshp, MASK = qualidim)

SELECT CASE(ndimr)
CASE(1)
  IF (PRESENT(vol1dp)) THEN
    CALL volptr1d/**/VOL7D_POLY_TYPES(shpr, vol1dp, vol)
  ELSE
    CALL raise_error('puntatore 1d mancante')
    RETURN
  ENDIF
CASE(2)
  IF (PRESENT(vol2dp)) THEN
    CALL volptr2d/**/VOL7D_POLY_TYPES(shpr, vol2dp, vol)
  ELSE
    CALL raise_error('puntatore 2d mancante')
    RETURN
  ENDIF
CASE(3)
  IF (PRESENT(vol3dp)) THEN
    CALL volptr3d/**/VOL7D_POLY_TYPES(shpr, vol3dp, vol)
  ELSE
    CALL raise_error('puntatore 3d mancante')
    RETURN
  ENDIF
CASE(4)
  IF (PRESENT(vol4dp)) THEN
    CALL volptr4d/**/VOL7D_POLY_TYPES(shpr, vol4dp, vol)
  ELSE
    CALL raise_error('puntatore 4d mancante')
    RETURN
  ENDIF
CASE(5)
  IF (PRESENT(vol5dp)) THEN
    CALL volptr5d/**/VOL7D_POLY_TYPES(shpr, vol5dp, vol)
  ELSE
    CALL raise_error('puntatore 5d mancante')
    RETURN
  ENDIF
CASE(6)
  IF (PRESENT(vol6dp)) THEN
    CALL volptr6d/**/VOL7D_POLY_TYPES(shpr, vol6dp, vol)
  ELSE
    CALL raise_error('puntatore 6d mancante')
    RETURN
  ENDIF
CASE(7)
  IF (PRESENT(vol7dp)) THEN
    CALL volptr7d/**/VOL7D_POLY_TYPES(shpr, vol7dp, vol)
  ELSE
    CALL raise_error('puntatore 7d mancante')
    RETURN
  ENDIF
END SELECT

END SUBROUTINE vol7d_get_vol/**/VOL7D_POLY_TYPES


SUBROUTINE volptr1d/**/VOL7D_POLY_TYPES(dims, lvol, lvolnd)
INTEGER :: dims(1)
VOL7D_POLY_TYPE, POINTER :: lvol(:)
VOL7D_POLY_TYPE, TARGET :: lvolnd(dims(1))

lvol => lvolnd

END SUBROUTINE volptr1d/**/VOL7D_POLY_TYPES


SUBROUTINE volptr2d/**/VOL7D_POLY_TYPES(dims, lvol, lvolnd)
INTEGER :: dims(2)
VOL7D_POLY_TYPE, POINTER :: lvol(:,:)
VOL7D_POLY_TYPE, TARGET :: lvolnd(dims(1),dims(2))

lvol => lvolnd

END SUBROUTINE volptr2d/**/VOL7D_POLY_TYPES


SUBROUTINE volptr3d/**/VOL7D_POLY_TYPES(dims, lvol, lvolnd)
INTEGER :: dims(3)
VOL7D_POLY_TYPE, POINTER :: lvol(:,:,:)
VOL7D_POLY_TYPE, TARGET :: lvolnd(dims(1),dims(2),dims(3))

lvol => lvolnd

END SUBROUTINE volptr3d/**/VOL7D_POLY_TYPES


SUBROUTINE volptr4d/**/VOL7D_POLY_TYPES(dims, lvol, lvolnd)
INTEGER :: dims(4)
VOL7D_POLY_TYPE, POINTER :: lvol(:,:,:,:)
VOL7D_POLY_TYPE, TARGET :: lvolnd(dims(1),dims(2),dims(3),dims(4))

lvol => lvolnd

END SUBROUTINE volptr4d/**/VOL7D_POLY_TYPES


SUBROUTINE volptr5d/**/VOL7D_POLY_TYPES(dims, lvol, lvolnd)
INTEGER :: dims(5)
VOL7D_POLY_TYPE, POINTER :: lvol(:,:,:,:,:)
VOL7D_POLY_TYPE, TARGET :: lvolnd(dims(1),dims(2),dims(3),dims(4),dims(5))

lvol => lvolnd

END SUBROUTINE volptr5d/**/VOL7D_POLY_TYPES


SUBROUTINE volptr6d/**/VOL7D_POLY_TYPES(dims, lvol, lvolnd)
INTEGER :: dims(6)
VOL7D_POLY_TYPE, POINTER :: lvol(:,:,:,:,:,:)
VOL7D_POLY_TYPE, TARGET :: lvolnd(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6))

lvol => lvolnd

END SUBROUTINE volptr6d/**/VOL7D_POLY_TYPES


SUBROUTINE volptr7d/**/VOL7D_POLY_TYPES(dims, lvol, lvolnd)
INTEGER :: dims(7)
VOL7D_POLY_TYPE, POINTER :: lvol(:,:,:,:,:,:,:)
VOL7D_POLY_TYPE, TARGET :: lvolnd(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7))

lvol => lvolnd

END SUBROUTINE volptr7d/**/VOL7D_POLY_TYPES


SUBROUTINE vol7d_nullify/**/VOL7D_POLY_TYPES(vol1dp, vol2dp, vol3dp, vol4dp, &
 vol5dp, vol6dp, vol7dp)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol1dp(:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol2dp(:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol3dp(:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol4dp(:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol5dp(:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol6dp(:,:,:,:,:,:)
VOL7D_POLY_TYPE,POINTER,OPTIONAL :: vol7dp(:,:,:,:,:,:,:)

IF (PRESENT(vol1dp)) NULLIFY(vol1dp)
IF (PRESENT(vol2dp)) NULLIFY(vol2dp)
IF (PRESENT(vol3dp)) NULLIFY(vol3dp)
IF (PRESENT(vol4dp)) NULLIFY(vol4dp)
IF (PRESENT(vol5dp)) NULLIFY(vol5dp)
IF (PRESENT(vol6dp)) NULLIFY(vol6dp)
IF (PRESENT(vol7dp)) NULLIFY(vol7dp)

END SUBROUTINE vol7d_nullify/**/VOL7D_POLY_TYPES
