! All the keywords recognized by ftnchek as of 21 July 2000
      PROGRAM allkeys
      IMPLICIT NONE
      BYTE b(100)
      CHARACTER *100 c
      DOUBLE COMPLEX a
      COMPLEX z
      DOUBLEPRECISION d
      DOUBLE PRECISION f
      EXTERNAL f, foo, iftn_calloc
      INTEGER i, l, u, iftn_calloc
      LOGICAL bool
      POINTER (ptr, ptee)
      INTEGER ptee(*)
      REAL pi
      EQUIVALENCE (b(1), i)
      NAMELIST /n1/ a,b,c
      PARAMETER (pi = 3.1415926)
      INCLUDE 'block2.i'
      ACCEPT *, c
      OPEN(unit=10, file=c)
      u = 10
      ASSIGN 100 TO l
 100  CONTINUE
      READ(10,*) a, x, i, z, d
      BACKSPACE 10
      BACKSPACE u
      BACKSPACE (unit=10)
      ptr = iftn_calloc (1000, 4)
      CALL foo
      SELECTCASE (i)
      CASE (1)
         ptee(i) = 2
      CASEDEFAULT
         ptee(i) = ptee(1)
      ENDSELECT
      SELECT CASE (i)
      CASE (2)
         i = 3
      CASE DEFAULT
         i = 4
      END SELECT
      DO 200 i=1,10
 200  PRINT *, i
      IF( x .eq. i ) GOTO l
      INQUIRE(UNIT=u, IOSTAT=i)
      ENDFILE 10
      END FILE u
      ENDFILE (unit=10,err=100)
      REWIND 10
      REWIND u
      REWIND (unit=10)
      CLOSE (UNIT=10)
      bool = .TRUE.
      IF( bool ) THEN
      WRITE(10,*) a, y, z         
      ELSEIF( .NOT. .FALSE. ) THEN
         WRITE(UNIT=10,FMT=900) 'Testing 1, 2, 3'
 900  FORMAT(1x,a20)
      ELSE
         CALL bar
         STOP
      ENDIF
      DOWHILE( i .lt. 100 )
      IF( f(d) .gt. 3.14) THEN
         CYCLE
      ELSE
         EXIT
      END IF
      ENDDO
      i = 1
      DO WHILE( i .lt. 100 )
         i = i*2
      END DO
      DO 300 WHILE( x .gt. 0.0 )
         TYPE *, pi, 'Over and over'
 300  CONTINUE
      DO 4321, WHILE( x .gt. 0.0 )
         TYPE *, pi, 'Over and over'
 4321 CONTINUE
      GO TO 100
      END
      DOUBLEPRECISION FUNCTION f(x)
      DOUBLE PRECISION x
      REAL y1, y2
      DOUBLECOMPLEX a
      SAVE a
      DIMENSION a(2)
      INTRINSIC sqrt
      COMMON /block1/ y1, y2
      a(1) = (1.0d1, 2.0d2)
      a(2) = a(1)
      f = x*dble(y1*y2)* sqrt(real(a(1)))
      RETURN
      END
      SUBROUTINE foo
      IMPLICIT INTEGER (A-Z)
      WRITE(10,*) 'foo'
      RETURN
      ENTRY bar
      WRITE(10,*) 'bar'
      PAUSE
      RETURN
      END
      BLOCKDATA bdat1
      COMMON /block1/ a, b
      DATA a, b /1.0, 2.0/
      END
      BLOCK DATA bdat2
      IMPLICITNONE
      INCLUDE 'block2.i'
      DATA x, y /1.0, 2.0/
      END
