# f4go

Transpiling fortran code to golang code

Present result:
```fortran
*  =====================================================================
      SUBROUTINE CAXPY(N,CA,CX,INCX,CY,INCY)
*
*  -- Reference BLAS level1 routine (version 3.8.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      COMPLEX CA
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      COMPLEX CX(*),CY(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,IX,IY
*     ..
*     .. External Functions ..
      REAL SCABS1
      EXTERNAL SCABS1
*     ..
      IF (N.LE.0) RETURN
      IF (SCABS1(CA).EQ.0.0E+0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*        code for both increments equal to 1
*
         DO I = 1,N
            CY(I) = CY(I) + CA*CX(I)
         END DO
      ELSE
*
*        code for unequal increments or equal increments
*          not equal to 1
*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            CY(IY) = CY(IY) + CA*CX(IX)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
*
      RETURN
      END
```

Go code:

```golang
package main

func CAXPY(N int, CA complex128, CX []complex128, INCX int, CY []complex128, INCY int) {
	var I int
	var IX int
	var IY int
	if N <= 0 {
		return
	}
	if SCABS1(CA) == 0.0E+0 {
		return
	}
	if INCX == 1 && INCY == 1 {
		for I = 1; I < N; I++ {
			CY[I] = CY[I] + CA*CX[I]
		}
	} else {
		IX = 1
		IY = 1
		if INCX < 0 {
			IX = (-N+1)*INCX + 1
		}
		if INCY < 0 {
			IY = (-N+1)*INCY + 1
		}
		for I = 1; I < N; I++ {
			CY[IY] = CY[IY] + CA*CX[IX]
			IX = IX + INCX
			IY = IY + INCY
		}
	}
	return
}
```


### Fortran test sources

* [**ftnchek.tgz**](http://netlib.org/fortran/index.html)
* [**blas from lapack3.8.0**](http://netlib.org/lapack/index.html)
