      INTEGER LIST(100)
      EQUIVALENCE (E,B)
      EQUIVALENCE (E,C)
      EQUIVALENCE (A,C)
      A = 1.0
      CALL FOO(A,D,B,C)
      CALL FOO(A,B,C,A)
      CALL FOO(W,X,Y,Z)
      DO 10 I = 1,100
         LIST(I) = I
 10   CONTINUE
      DO 30 I = 1,100
         DO 20 J = 1,I
            CALL SWAP(LIST(I),LIST(J))
 20      CONTINUE
 30   CONTINUE
      END
      SUBROUTINE FOO(A,B,C,D)
      CALL SWAP(A,B)
      B = C
      C = D
      D = A
      END
      SUBROUTINE SWAP(X,Y)
      INTEGER X,Y
      INTEGER TEMP
      TEMP = X
      X = Y
      Y = TEMP
      END

