      SUBROUTINE IFFY ( USE1, USE2, L )
* Tests parsing & type-checking of various IF forms.
*
      LOGICAL USE1, USE2
      INTEGER L
      INTEGER M
      REAL X
* this block-if statement is OK
      IF ( USE1 ) THEN
         M = 1
* else-if statement is OK
      ELSE IF ( USE2 ) THEN
         M = 2
      ELSE
         M = 3
      END IF
* this logical if is OK
      IF( M .eq. 3 ) L = L+1
* this arithmetic if is OK
      IF( L ) 100, 100, 100
 100  X = L
* this arithmetic if is OK
      IF( X ) 150, 150, 150
 150  CONTINUE
* violate a block if statement
      IF ( L ) THEN
         M = M+1
* violate an elseif statement
      ELSE IF ( M ) THEN
         M = M+2
      ELSE
         M = M+3
      END IF
* violate a logical if statement
      IF( M ) L = L+1
* violate an arithmetic if statement
      IF( L .EQ. 3 ) 200, 200, 200
 200  CONTINUE
      END

