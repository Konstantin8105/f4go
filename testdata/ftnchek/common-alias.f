C Test program for checking aliasing of arguments to common variables
C
      COMMON Q,F,X
      COMMON /BLK1/ DAT(100)
      W = 1.0
      X = 2.0
      Z = 4.0
      CALL FOO(W,X,Y,Z)
      CALL FOO(F,W,Y,Z)
      CALL FOO(W,Y,Q,Z)
      PRINT *, W,X,Y,Z
      CALL BAR(DAT,100)
      STOP
      END
      SUBROUTINE FOO(A,B,C,D)
      COMMON T,U,V
      B = A+C+D
      T = B + 1.0
      RETURN
      END
      SUBROUTINE BAR(D,N)
      COMMON /BLK1/ TAD(100)
      REAL D(N)
      DO 10 I=1,N-1
         TAD(I) = I
         D(I) = TAD(I)*2.0
 10   CONTINUE
      END
