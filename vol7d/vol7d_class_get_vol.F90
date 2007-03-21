

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
  PRINT*,'Errore, dimensioni non valide:', dimlist
  STOP
ENDIF
qualidim = .FALSE.
qualidim(dimlist) = .TRUE.

ndimr = SIZE(dimlist)
!shp = SHAPE(this%voldatir)

IF (ANY(.NOT.qualidim .AND. volshp > 1) ) THEN
  PRINT*,'Errore, dimensioni non degeneri non richieste:', &
   PACK(volshp, mask=(.NOT.qualidim .AND. volshp > 1))
  STOP
ENDIF

shpr(1:ndimr) = PACK(volshp, MASK = qualidim)

SELECT CASE(ndimr)
CASE(1)
  IF (PRESENT(vol1dp)) THEN
    CALL volptr1d/**/VOL7D_POLY_TYPES(shpr, vol1dp, vol)
  ELSE
    PRINT*,'Errore, puntatore 1d, mancante'
    STOP
  ENDIF
CASE(2)
  IF (PRESENT(vol2dp)) THEN
    CALL volptr2d/**/VOL7D_POLY_TYPES(shpr, vol2dp, vol)
  ELSE
    PRINT*,'Errore, puntatore 2d, mancante'
    STOP
  ENDIF
CASE(3)
  IF (PRESENT(vol3dp)) THEN
    CALL volptr3d/**/VOL7D_POLY_TYPES(shpr, vol3dp, vol)
  ELSE
    PRINT*,'Errore, puntatore 3d, mancante'
    STOP
  ENDIF
CASE(4)
  IF (PRESENT(vol4dp)) THEN
    CALL volptr4d/**/VOL7D_POLY_TYPES(shpr, vol4dp, vol)
  ELSE
    PRINT*,'Errore, puntatore 4d, mancante'
    STOP
  ENDIF
CASE(5)
  IF (PRESENT(vol5dp)) THEN
    CALL volptr5d/**/VOL7D_POLY_TYPES(shpr, vol5dp, vol)
  ELSE
    PRINT*,'Errore, puntatore 5d, mancante'
    STOP
  ENDIF
CASE(6)
  IF (PRESENT(vol6dp)) THEN
    CALL volptr6d/**/VOL7D_POLY_TYPES(shpr, vol6dp, vol)
  ELSE
    PRINT*,'Errore, puntatore 6d, mancante'
    STOP
  ENDIF
CASE(7)
  IF (PRESENT(vol7dp)) THEN
    CALL volptr7d/**/VOL7D_POLY_TYPES(shpr, vol7dp, vol)
  ELSE
    PRINT*,'Errore, puntatore 7d, mancante'
    STOP
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
