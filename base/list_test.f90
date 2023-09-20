PROGRAM list_test
USE list_integer
IMPLICIT NONE

TYPE(integerlist) :: tlist1

INTEGER :: i, j, res(13)=(/50,100,40,300,30,500,20,10,1,2,3,4,5/)

DO i = 1, 5
  CALL tlist1%append(i)
  CALL tlist1%prepend(i*10)
ENDDO

DO i = 1, 5, 2
  IF (.NOT.tlist1%insert(i*100, i)) THEN
    PRINT*,'Unexpected error in insert at position ',i
    CALL EXIT(1)
  ENDIF
ENDDO

IF (tlist1%countelements() /= 13) THEN
  PRINT*,'Wrong list length: ',tlist1%countelements()
  CALL EXIT(1)
ENDIF

CALL tlist1%rewind()
DO WHILE(tlist1%element())
  j = tlist1%current()
  IF (j /= res(tlist1%currentindex())) THEN
    PRINT*,'Element ',tlist1%currentindex(),' has wrong value ',j
    CALL EXIT(1)
  ENDIF
  CALL tlist1%next()
ENDDO

END PROGRAM list_test
