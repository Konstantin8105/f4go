**** for old version, "send otqlrat from eispack"
** From dana!moler Tue, 1 Sep 87 10:15:40 PDT
** New TQLRAT
      SUBROUTINE TQLRAT(N,D,E2,IERR)
C
      INTEGER I,J,L,M,N,II,L1,MML,IERR
      DOUBLE PRECISION D(N),E2(N)
      DOUBLE PRECISION B,C,F,G,H,P,R,S,T,EPSLON,PYTHAG
C
C     This subroutine is a translation of the Algol procedure tqlrat,
C     Algorithm 464, Comm. ACM 16, 689(1973) by Reinsch.
C
C     This subroutine finds the eigenvalues of a symmetric
C     tridiagonal matrix by the rational QL method.
C
C     On input
C
C        N is the order of the matrix.
C
C        D contains the diagonal elements of the input matrix.
C
C        E2 contains the squares of the subdiagonal elements of the
C          input matrix in its last N-1 positions.  E2(1) is arbitrary.
C
C      On output
C
C        D contains the eigenvalues in ascending order.  If an
C          error exit is made, the eigenvalues are correct and
C          ordered for indices 1,2,...IERR-1, but may not be
C          the smallest eigenvalues.
C
C        E2 has been destroyed.
C
C        IERR is set to
C          zero       for normal return,
C          J          if the J-th eigenvalue has not been
C                     determined after 30 iterations.
C
C     Calls PYTHAG for  DSQRT(A*A + B*B) .
C
C     Questions and comments should be directed to Burton S. Garbow,
C     Mathematics and Computer Science Div, Argonne National Laboratory
C
C     This version dated August 1987.
C     Modified by C. Moler to fix underflow/overflow difficulties,
C     especially on the VAX and other machines where epslon(1.0d0)**2
C     nearly underflows.  See the loop involving statement 102 and
C     the two statements just before statement 200.
C
C     ------------------------------------------------------------------
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E2(I-1) = E2(I)
C
      F = 0.0D0
      T = 0.0D0
      E2(N) = 0.0D0
C
      DO 290 L = 1, N
         J = 0
         H = DABS(D(L)) + DSQRT(E2(L))
         IF (T .GT. H) GO TO 105
         T = H
         B = EPSLON(T)
         C = B * B
         if (c .ne. 0.0d0) go to 105
C        Spliting tolerance underflowed.  Look for larger value.
         do 102 i = l, n
            h = dabs(d(i)) + dsqrt(e2(i))
            if (h .gt. t) t = h
  102    continue
         b = epslon(t)
         c = b * b
C     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (E2(M) .LE. C) GO TO 120
C     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP ..........
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 210
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         L1 = L + 1
         S = DSQRT(E2(L))
         G = D(L)
         P = (D(L1) - G) / (2.0D0 * S)
         R = PYTHAG(P,1.0D0)
         D(L) = S / (P + DSIGN(R,P))
         H = G - D(L)
C
         DO 140 I = L1, N
  140    D(I) = D(I) - H
C
         F = F + H
C     .......... RATIONAL QL TRANSFORMATION ..........
         G = D(M)
         IF (G .EQ. 0.0D0) G = B
         H = G
         S = 0.0D0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            P = G * H
            R = P + E2(I)
            E2(I+1) = S * R
            S = E2(I) / R
            D(I+1) = H + S * (H + D(I))
            G = D(I) - E2(I) / G
C           Avoid division by zero on next pass
            if (g .eq. 0.0d0) g = epslon(d(i))
            h = g * (p / r)
  200    CONTINUE
C
         E2(L) = S * G
         D(L) = H
C     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........
         IF (H .EQ. 0.0D0) GO TO 210
         IF (DABS(E2(L)) .LE. DABS(C/H)) GO TO 210
         E2(L) = H * E2(L)
         IF (E2(L) .NE. 0.0D0) GO TO 130
  210    P = D(L) + F
C     .......... ORDER EIGENVALUES ..........
         IF (L .EQ. 1) GO TO 250
C     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
C
  250    I = 1
  270    D(I) = P
  290 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END
