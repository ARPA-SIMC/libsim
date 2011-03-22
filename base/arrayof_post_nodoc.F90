#ifndef ARRAYOF_TYPE
#define ARRAYOF_TYPE arrayof_/**/ARRAYOF_ORIGTYPE
#endif





FUNCTION ARRAYOF_TYPE/**/_new() RESULT(this)
TYPE(ARRAYOF_TYPE) :: this

! give empty/default values
NULLIFY(this%array)
this%arraysize = 0
this%overalloc = 2.0D0

END FUNCTION ARRAYOF_TYPE/**/_new




SUBROUTINE ARRAYOF_TYPE/**/_insert_array(this, content, nelem, pos)
TYPE(ARRAYOF_TYPE) :: this
ARRAYOF_ORIGTYPE, INTENT(in), OPTIONAL :: content(:)
INTEGER, INTENT(in), OPTIONAL :: nelem
INTEGER, INTENT(in), OPTIONAL :: pos

INTEGER :: i, n, p

IF (PRESENT(content)) THEN ! size of data
  n = SIZE(content)
ELSE IF (PRESENT(nelem)) THEN ! explicit size
  n = nelem
ELSE ! default add one element
  n = 1
ENDIF
IF (n <= 0) RETURN ! nothing to do

IF (PRESENT(pos)) THEN ! clip p
  p = MAX(1, MIN(pos, this%arraysize+1))
ELSE ! pos not provided, append
  p = this%arraysize + 1
ENDIF
this%arraysize = this%arraysize + n
#ifdef DEBUG
!PRINT*,'ARRAYOF: inserting ',n,' elements at position ',p
#endif

CALL array_of_alloc(this) ! ensure to have space
DO i = this%arraysize, p+n, -1 ! push the elements forward starting from p
  this%array(i) = this%array(i-n)
ENDDO
IF (PRESENT(content)) THEN
  this%array(p:p+n-1) = content(:)
ENDIF

END SUBROUTINE ARRAYOF_TYPE/**/_insert_array




SUBROUTINE ARRAYOF_TYPE/**/_insert(this, content, pos)
TYPE(ARRAYOF_TYPE) :: this
ARRAYOF_ORIGTYPE, INTENT(in) :: content
INTEGER, INTENT(in), OPTIONAL :: pos

CALL insert(this, (/content/), pos=pos)

END SUBROUTINE ARRAYOF_TYPE/**/_insert





FUNCTION ARRAYOF_TYPE/**/_append(this, content) RESULT(pos)
TYPE(ARRAYOF_TYPE) :: this
ARRAYOF_ORIGTYPE, INTENT(in) :: content
INTEGER :: pos

this%arraysize = this%arraysize + 1
pos = this%arraysize + 1
CALL array_of_alloc(this)
this%array(this%arraysize) = content

END FUNCTION ARRAYOF_TYPE/**/_append


#ifdef ARRAYOF_ORIGEQ



SUBROUTINE ARRAYOF_TYPE/**/_insert_unique(this, content, pos)
TYPE(ARRAYOF_TYPE) :: this
ARRAYOF_ORIGTYPE, INTENT(in) :: content
INTEGER, INTENT(in), OPTIONAL :: pos

INTEGER :: i

DO i = 1, this%arraysize
  IF (this%array(i) == content) RETURN
ENDDO

CALL insert(this, (/content/), pos=pos)

END SUBROUTINE ARRAYOF_TYPE/**/_insert_unique






FUNCTION ARRAYOF_TYPE/**/_append_unique(this, content) RESULT(pos)
TYPE(ARRAYOF_TYPE) :: this
ARRAYOF_ORIGTYPE, INTENT(in) :: content
INTEGER :: pos

DO pos = 1, this%arraysize
  IF (this%array(pos) == content) RETURN
ENDDO

this%arraysize = this%arraysize + 1
pos = this%arraysize
CALL array_of_alloc(this)
this%array(this%arraysize) = content

END FUNCTION ARRAYOF_TYPE/**/_append_unique
#endif




SUBROUTINE ARRAYOF_TYPE/**/_remove(this, nelem, pos, nodestroy)
TYPE(ARRAYOF_TYPE) :: this
INTEGER, INTENT(in), OPTIONAL :: nelem
INTEGER, INTENT(in), OPTIONAL :: pos
LOGICAL, INTENT(in), OPTIONAL :: nodestroy

INTEGER :: i, n, p
LOGICAL :: destroy

IF (this%arraysize <= 0) RETURN ! nothing to do
IF (PRESENT(nelem)) THEN ! explicit size
  n = nelem
  IF (n <= 0) RETURN ! nothing to do
ELSE ! default remove one element
  n = 1
ENDIF

IF (PRESENT(pos)) THEN ! clip p
  p = MAX(1, MIN(pos, this%arraysize-n+1))
ELSE ! pos not provided, cut at the end
  p = this%arraysize - n + 1
ENDIF
#ifdef DEBUG
!PRINT*,'ARRAYOF: removing ',n,' elements at position ',p
#endif

! destroy the elements if needed
#ifdef ARRAYOF_ORIGDESTRUCTOR
destroy = .TRUE.
IF (PRESENT(nodestroy)) THEN
  destroy = .NOT.nodestroy
ENDIF
IF (destroy) THEN
  DO i = p, p+n-1
    ARRAYOF_ORIGDESTRUCTOR(this%array(i))
  ENDDO
ENDIF
#endif

this%arraysize = this%arraysize - n
DO i = p, this%arraysize ! push the elements backward starting from p
  this%array(i) = this%array(i+n)
ENDDO
CALL array_of_alloc(this) ! release space if possible

END SUBROUTINE ARRAYOF_TYPE/**/_remove





SUBROUTINE ARRAYOF_TYPE/**/_delete(this, nodestroy)
TYPE(ARRAYOF_TYPE) :: this
LOGICAL, INTENT(in), OPTIONAL :: nodestroy

INTEGER :: i
LOGICAL :: destroy

#ifdef DEBUG
!PRINT*,'ARRAYOF: destroying ',this%arraysize
#endif
IF (ASSOCIATED(this%array)) THEN
! destroy the elements if needed
#ifdef ARRAYOF_ORIGDESTRUCTOR
  destroy = .TRUE.
  IF (PRESENT(nodestroy)) THEN
    destroy = .NOT.nodestroy
  ENDIF
  IF (destroy) THEN
    DO i = 1, this%arraysize
      ARRAYOF_ORIGDESTRUCTOR(this%array(i))
    ENDDO
  ENDIF
#endif
! free the space
  DEALLOCATE(this%array)
ENDIF
! give empty values
this=ARRAYOF_TYPE/**/_new()

END SUBROUTINE ARRAYOF_TYPE/**/_delete







SUBROUTINE ARRAYOF_TYPE/**/_packarray(this)
TYPE(ARRAYOF_TYPE) :: this

DOUBLE PRECISION :: tmpoveralloc

#ifdef DEBUG
!PRINT*,'ARRAYOF: packing ',this%arraysize
#endif
tmpoveralloc = this%overalloc ! save value
this%overalloc = 1.0D0
CALL array_of_alloc(this) ! reallocate exact size
this%overalloc = tmpoveralloc

END SUBROUTINE ARRAYOF_TYPE/**/_packarray


SUBROUTINE array_of_alloc(this)
TYPE(ARRAYOF_TYPE) :: this

ARRAYOF_ORIGTYPE, POINTER :: tmpptr(:)
INTEGER :: newsize, copysize

newsize = MAX(INT(this%arraysize*this%overalloc), this%arraysize)

IF (ASSOCIATED(this%array)) THEN ! array already allocated
! space is neither too small nor too big, nothing to do
  IF (SIZE(this%array) >= this%arraysize .AND. SIZE(this%array) <= newsize) RETURN
! if too big, reduce
  IF (SIZE(this%array) > newsize) newsize = this%arraysize
#ifdef DEBUG
!  PRINT*,'ARRAYOF: requested ',this%arraysize,' elements, allocating ',newsize
#endif
  tmpptr => this%array ! keep a pointer to the old data
  ALLOCATE(this%array(newsize))
  copysize = MIN(this%arraysize, SIZE(tmpptr)) ! restrict to valid intervals
  this%array(1:copysize) = tmpptr(1:copysize) ! copy the old data
  DEALLOCATE(tmpptr) ! and destroy them
ELSE ! need to allocate from scratch
#ifdef DEBUG
!  PRINT*,'ARRAYOF: first time requested ',this%arraysize,' elements, allocating ',newsize
#endif
  ALLOCATE(this%array(newsize))
ENDIF

END SUBROUTINE array_of_alloc
