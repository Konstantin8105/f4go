package main

import "github.com/Konstantin8105/f4go/intrinsic"

//*> \brief \b SSBMV
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE SSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
//*
//*       .. Scalar Arguments ..
//*       REAL ALPHA,BETA
//*       INTEGER INCX,INCY,K,LDA,N
//*       CHARACTER UPLO
//*       ..
//*       .. Array Arguments ..
//*       REAL A(LDA,*),X(*),Y(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> SSBMV  performs the matrix-vector  operation
//*>
//*>    y := alpha*A*x + beta*y,
//*>
//*> where alpha and beta are scalars, x and y are n element vectors and
//*> A is an n by n symmetric band matrix, with k super-diagonals.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] UPLO
//*> \verbatim
//*>          UPLO is CHARACTER*1
//*>           On entry, UPLO specifies whether the upper or lower
//*>           triangular part of the band matrix A is being supplied as
//*>           follows:
//*>
//*>              UPLO = 'U' or 'u'   The upper triangular part of A is
//*>                                  being supplied.
//*>
//*>              UPLO = 'L' or 'l'   The lower triangular part of A is
//*>                                  being supplied.
//*> \endverbatim
//*>
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry, N specifies the order of the matrix A.
//*>           N must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] K
//*> \verbatim
//*>          K is INTEGER
//*>           On entry, K specifies the number of super-diagonals of the
//*>           matrix A. K must satisfy  0 .le. K.
//*> \endverbatim
//*>
//*> \param[in] ALPHA
//*> \verbatim
//*>          ALPHA is REAL
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] A
//*> \verbatim
//*>          A is REAL array, dimension ( LDA, N )
//*>           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
//*>           by n part of the array A must contain the upper triangular
//*>           band part of the symmetric matrix, supplied column by
//*>           column, with the leading diagonal of the matrix in row
//*>           ( k + 1 ) of the array, the first super-diagonal starting at
//*>           position 2 in row k, and so on. The top left k by k triangle
//*>           of the array A is not referenced.
//*>           The following program segment will transfer the upper
//*>           triangular part of a symmetric band matrix from conventional
//*>           full matrix storage to band storage:
//*>
//*>                 DO 20, J = 1, N
//*>                    M = K + 1 - J
//*>                    DO 10, I = MAX( 1, J - K ), J
//*>                       A( M + I, J ) = matrix( I, J )
//*>              10    CONTINUE
//*>              20 CONTINUE
//*>
//*>           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
//*>           by n part of the array A must contain the lower triangular
//*>           band part of the symmetric matrix, supplied column by
//*>           column, with the leading diagonal of the matrix in row 1 of
//*>           the array, the first sub-diagonal starting at position 1 in
//*>           row 2, and so on. The bottom right k by k triangle of the
//*>           array A is not referenced.
//*>           The following program segment will transfer the lower
//*>           triangular part of a symmetric band matrix from conventional
//*>           full matrix storage to band storage:
//*>
//*>                 DO 20, J = 1, N
//*>                    M = 1 - J
//*>                    DO 10, I = J, MIN( N, J + K )
//*>                       A( M + I, J ) = matrix( I, J )
//*>              10    CONTINUE
//*>              20 CONTINUE
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in the calling (sub) program. LDA must be at least
//*>           ( k + 1 ).
//*> \endverbatim
//*>
//*> \param[in] X
//*> \verbatim
//*>          X is REAL array, dimension at least
//*>           ( 1 + ( n - 1 )*abs( INCX ) ).
//*>           Before entry, the incremented array X must contain the
//*>           vector x.
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>           On entry, INCX specifies the increment for the elements of
//*>           X. INCX must not be zero.
//*> \endverbatim
//*>
//*> \param[in] BETA
//*> \verbatim
//*>          BETA is REAL
//*>           On entry, BETA specifies the scalar beta.
//*> \endverbatim
//*>
//*> \param[in,out] Y
//*> \verbatim
//*>          Y is REAL array, dimension at least
//*>           ( 1 + ( n - 1 )*abs( INCY ) ).
//*>           Before entry, the incremented array Y must contain the
//*>           vector y. On exit, Y is overwritten by the updated vector y.
//*> \endverbatim
//*>
//*> \param[in] INCY
//*> \verbatim
//*>          INCY is INTEGER
//*>           On entry, INCY specifies the increment for the elements of
//*>           Y. INCY must not be zero.
//*> \endverbatim
//*
//*  Authors:
//*  ========
//*
//*> \author Univ. of Tennessee
//*> \author Univ. of California Berkeley
//*> \author Univ. of Colorado Denver
//*> \author NAG Ltd.
//*
//*> \date December 2016
//*
//*> \ingroup single_blas_level2
//*
//*> \par Further Details:
//*  =====================
//*>
//*> \verbatim
//*>
//*>  Level 2 Blas routine.
//*>  The vector and matrix arguments are not referenced when N = 0, or M = 0
//*>
//*>  -- Written on 22-October-1986.
//*>     Jack Dongarra, Argonne National Lab.
//*>     Jeremy Du Croz, Nag Central Office.
//*>     Sven Hammarling, Nag Central Office.
//*>     Richard Hanson, Sandia National Labs.
//*> \endverbatim
//*>
//*  =====================================================================
func SSBMV(UPLO *byte, N *int, K *int, ALPHA *float64, A *[][]float64, LDA *int, X *[]float64, INCX *int, BETA *float64, Y *[]float64, INCY *int) {
	var ONE float64 = 1.0e+0
	var ZERO float64 = 0.0e+0
	var TEMP1 float64
	var TEMP2 float64
	var I int
	var INFO int
	var IX int
	var IY int
	var J int
	var JX int
	var JY int
	var KPLUS1 int
	var KX int
	var KY int
	var L int
	//*
	//*  -- Reference BLAS level2 routine (version 3.7.0) --
	//*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
	//*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	//*     December 2016
	//*
	//*     .. Scalar Arguments ..
	//*     ..
	//*     .. Array Arguments ..
	//*     ..
	//*
	//*  =====================================================================
	//*
	//*     .. Parameters ..
	//*     ..
	//*     .. Local Scalars ..
	//*     ..
	//*     .. External Functions ..
	//*     ..
	//*     .. External Subroutines ..
	//*     ..
	//*     .. Intrinsic Functions ..
	//*     ..
	//*
	//*     Test the input parameters.
	//*
	INFO = 0
	if !LSAME(UPLO, func() *byte { y := byte('U'); return &y }()) && !LSAME(UPLO, func() *byte { y := byte('L'); return &y }()) {
		INFO = 1
	} else if (*N) < 0 {
		INFO = 2
	} else if (*K) < 0 {
		INFO = 3
	} else if (*LDA) < ((*K) + 1) {
		INFO = 6
	} else if (*INCX) == 0 {
		INFO = 8
	} else if (*INCY) == 0 {
		INFO = 11
	}
	if INFO != 0 {
		XERBLA(func() *[]byte { y := []byte("SSBMV "); return &y }(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if ((*N) == 0) || (((*ALPHA) == ZERO) && ((*BETA) == ONE)) {
		return
	}
	//*
	//*     Set up the start points in  X  and  Y.
	//*
	if (*INCX) > 0 {
		KX = 1
	} else {
		KX = 1 - ((*N)-1)*(*INCX)
	}
	if (*INCY) > 0 {
		KY = 1
	} else {
		KY = 1 - ((*N)-1)*(*INCY)
	}
	//*
	//*     Start the operations. In this version the elements of the array A
	//*     are accessed sequentially with one pass through A.
	//*
	//*     First form  y := beta*y.
	//*
	if (*BETA) != ONE {
		if (*INCY) == 1 {
			if (*BETA) == ZERO {
				for I = 1; I <= (*N); I++ {
					(*Y)[I-(1)] = ZERO
				}
			} else {
				for I = 1; I <= (*N); I++ {
					(*Y)[I-(1)] = (*BETA) * (*Y)[I-(1)]
				}
			}
		} else {
			IY = KY
			if (*BETA) == ZERO {
				for I = 1; I <= (*N); I++ {
					(*Y)[IY-(1)] = ZERO
					IY = IY + (*INCY)
				}
			} else {
				for I = 1; I <= (*N); I++ {
					(*Y)[IY-(1)] = (*BETA) * (*Y)[IY-(1)]
					IY = IY + (*INCY)
				}
			}
		}
	}
	if (*ALPHA) == ZERO {
		return
	}
	if LSAME(UPLO, func() *byte { y := byte('U'); return &y }()) {
		//*
		//*        Form  y  when upper triangle of A is stored.
		//*
		KPLUS1 = (*K) + 1
		if ((*INCX) == 1) && ((*INCY) == 1) {
			for J = 1; J <= (*N); J++ {
				TEMP1 = (*ALPHA) * (*X)[J-(1)]
				TEMP2 = ZERO
				L = KPLUS1 - J
				for I = intrinsic.MAX(int(1), J-(*K)); I <= J-1; I++ {
					(*Y)[I-(1)] = (*Y)[I-(1)] + TEMP1*(*A)[L+I-(1)][J-(1)]
					TEMP2 = TEMP2 + (*A)[L+I-(1)][J-(1)]*(*X)[I-(1)]
				}
				(*Y)[J-(1)] = (*Y)[J-(1)] + TEMP1*(*A)[KPLUS1-(1)][J-(1)] + (*ALPHA)*TEMP2
			}
		} else {
			JX = KX
			JY = KY
			for J = 1; J <= (*N); J++ {
				TEMP1 = (*ALPHA) * (*X)[JX-(1)]
				TEMP2 = ZERO
				IX = KX
				IY = KY
				L = KPLUS1 - J
				for I = intrinsic.MAX(int(1), J-(*K)); I <= J-1; I++ {
					(*Y)[IY-(1)] = (*Y)[IY-(1)] + TEMP1*(*A)[L+I-(1)][J-(1)]
					TEMP2 = TEMP2 + (*A)[L+I-(1)][J-(1)]*(*X)[IX-(1)]
					IX = IX + (*INCX)
					IY = IY + (*INCY)
				}
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + TEMP1*(*A)[KPLUS1-(1)][J-(1)] + (*ALPHA)*TEMP2
				JX = JX + (*INCX)
				JY = JY + (*INCY)
				if J > (*K) {
					KX = KX + (*INCX)
					KY = KY + (*INCY)
				}
			}
		}
	} else {
		//*
		//*        Form  y  when lower triangle of A is stored.
		//*
		if ((*INCX) == 1) && ((*INCY) == 1) {
			for J = 1; J <= (*N); J++ {
				TEMP1 = (*ALPHA) * (*X)[J-(1)]
				TEMP2 = ZERO
				(*Y)[J-(1)] = (*Y)[J-(1)] + TEMP1*(*A)[1-(1)][J-(1)]
				L = 1 - J
				for I = J + 1; I <= intrinsic.MIN((*N), J+(*K)); I++ {
					(*Y)[I-(1)] = (*Y)[I-(1)] + TEMP1*(*A)[L+I-(1)][J-(1)]
					TEMP2 = TEMP2 + (*A)[L+I-(1)][J-(1)]*(*X)[I-(1)]
				}
				(*Y)[J-(1)] = (*Y)[J-(1)] + (*ALPHA)*TEMP2
			}
		} else {
			JX = KX
			JY = KY
			for J = 1; J <= (*N); J++ {
				TEMP1 = (*ALPHA) * (*X)[JX-(1)]
				TEMP2 = ZERO
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + TEMP1*(*A)[1-(1)][J-(1)]
				L = 1 - J
				IX = JX
				IY = JY
				for I = J + 1; I <= intrinsic.MIN((*N), J+(*K)); I++ {
					IX = IX + (*INCX)
					IY = IY + (*INCY)
					(*Y)[IY-(1)] = (*Y)[IY-(1)] + TEMP1*(*A)[L+I-(1)][J-(1)]
					TEMP2 = TEMP2 + (*A)[L+I-(1)][J-(1)]*(*X)[IX-(1)]
				}
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + (*ALPHA)*TEMP2
				JX = JX + (*INCX)
				JY = JY + (*INCY)
			}
		}
	}
	//*
	return
	//*
	//*     End of SSBMV .
	//*
}
