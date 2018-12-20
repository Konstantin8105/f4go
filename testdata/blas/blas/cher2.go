package main

import "github.com/Konstantin8105/f4go/intrinsic"

//*> \brief \b CHER2
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX ALPHA
//*       INTEGER INCX,INCY,LDA,N
//*       CHARACTER UPLO
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX A(LDA,*),X(*),Y(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> CHER2  performs the hermitian rank 2 operation
//*>
//*>    A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
//*>
//*> where alpha is a scalar, x and y are n element vectors and A is an n
//*> by n hermitian matrix.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] UPLO
//*> \verbatim
//*>          UPLO is CHARACTER*1
//*>           On entry, UPLO specifies whether the upper or lower
//*>           triangular part of the array A is to be referenced as
//*>           follows:
//*>
//*>              UPLO = 'U' or 'u'   Only the upper triangular part of A
//*>                                  is to be referenced.
//*>
//*>              UPLO = 'L' or 'l'   Only the lower triangular part of A
//*>                                  is to be referenced.
//*> \endverbatim
//*>
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry, N specifies the order of the matrix A.
//*>           N must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] ALPHA
//*> \verbatim
//*>          ALPHA is COMPLEX
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] X
//*> \verbatim
//*>          X is COMPLEX array, dimension at least
//*>           ( 1 + ( n - 1 )*abs( INCX ) ).
//*>           Before entry, the incremented array X must contain the n
//*>           element vector x.
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>           On entry, INCX specifies the increment for the elements of
//*>           X. INCX must not be zero.
//*> \endverbatim
//*>
//*> \param[in] Y
//*> \verbatim
//*>          Y is COMPLEX array, dimension at least
//*>           ( 1 + ( n - 1 )*abs( INCY ) ).
//*>           Before entry, the incremented array Y must contain the n
//*>           element vector y.
//*> \endverbatim
//*>
//*> \param[in] INCY
//*> \verbatim
//*>          INCY is INTEGER
//*>           On entry, INCY specifies the increment for the elements of
//*>           Y. INCY must not be zero.
//*> \endverbatim
//*>
//*> \param[in,out] A
//*> \verbatim
//*>          A is COMPLEX array, dimension ( LDA, N )
//*>           Before entry with  UPLO = 'U' or 'u', the leading n by n
//*>           upper triangular part of the array A must contain the upper
//*>           triangular part of the hermitian matrix and the strictly
//*>           lower triangular part of A is not referenced. On exit, the
//*>           upper triangular part of the array A is overwritten by the
//*>           upper triangular part of the updated matrix.
//*>           Before entry with UPLO = 'L' or 'l', the leading n by n
//*>           lower triangular part of the array A must contain the lower
//*>           triangular part of the hermitian matrix and the strictly
//*>           upper triangular part of A is not referenced. On exit, the
//*>           lower triangular part of the array A is overwritten by the
//*>           lower triangular part of the updated matrix.
//*>           Note that the imaginary parts of the diagonal elements need
//*>           not be set, they are assumed to be zero, and on exit they
//*>           are set to zero.
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in the calling (sub) program. LDA must be at least
//*>           max( 1, n ).
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
//*> \ingroup complex_blas_level2
//*
//*> \par Further Details:
//*  =====================
//*>
//*> \verbatim
//*>
//*>  Level 2 Blas routine.
//*>
//*>  -- Written on 22-October-1986.
//*>     Jack Dongarra, Argonne National Lab.
//*>     Jeremy Du Croz, Nag Central Office.
//*>     Sven Hammarling, Nag Central Office.
//*>     Richard Hanson, Sandia National Labs.
//*> \endverbatim
//*>
//*  =====================================================================
func CHER2(UPLO *byte, N *int, ALPHA *complex64, X *[]complex64, INCX *int, Y *[]complex64, INCY *int, A *[][]complex64, LDA *int) {
	var ZERO complex64 = (0.0e+0 + (0.0e+0)*1i)
	var TEMP1 complex64
	var TEMP2 complex64
	var I int
	var INFO int
	var IX int
	var IY int
	var J int
	var JX int
	var JY int
	var KX int
	var KY int
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
	} else if (*INCX) == 0 {
		INFO = 5
	} else if (*INCY) == 0 {
		INFO = 7
	} else if (*LDA) < intrinsic.MAX(int(1), (*N)) {
		INFO = 9
	}
	if INFO != 0 {
		XERBLA(func() *[]byte { y := []byte("CHER2 "); return &y }(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if ((*N) == 0) || ((*ALPHA) == ZERO) {
		return
	}
	//*
	//*     Set up the start points in X and Y if the increments are not both
	//*     unity.
	//*
	if ((*INCX) != 1) || ((*INCY) != 1) {
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
		JX = KX
		JY = KY
	}
	//*
	//*     Start the operations. In this version the elements of A are
	//*     accessed sequentially with one pass through the triangular part
	//*     of A.
	//*
	if LSAME(UPLO, func() *byte { y := byte('U'); return &y }()) {
		//*
		//*        Form  A  when A is stored in the upper triangle.
		//*
		if ((*INCX) == 1) && ((*INCY) == 1) {
			for J = 1; J <= (*N); J++ {
				if ((*X)[J-(1)] != ZERO) || ((*Y)[J-(1)] != ZERO) {
					TEMP1 = (*ALPHA) * intrinsic.CONJG((*Y)[J-(1)])
					TEMP2 = intrinsic.CONJG((*ALPHA) * (*X)[J-(1)])
					for I = 1; I <= J-1; I++ {
						(*A)[I-(1)][J-(1)] = (*A)[I-(1)][J-(1)] + (*X)[I-(1)]*TEMP1 + (*Y)[I-(1)]*TEMP2
					}
					(*A)[J-(1)][J-(1)] = real((*A)[J-(1)][J-(1)]) + real((*X)[J-(1)]*TEMP1+(*Y)[J-(1)]*TEMP2)
				} else {
					(*A)[J-(1)][J-(1)] = real((*A)[J-(1)][J-(1)])
				}
			}
		} else {
			for J = 1; J <= (*N); J++ {
				if ((*X)[JX-(1)] != ZERO) || ((*Y)[JY-(1)] != ZERO) {
					TEMP1 = (*ALPHA) * intrinsic.CONJG((*Y)[JY-(1)])
					TEMP2 = intrinsic.CONJG((*ALPHA) * (*X)[JX-(1)])
					IX = KX
					IY = KY
					for I = 1; I <= J-1; I++ {
						(*A)[I-(1)][J-(1)] = (*A)[I-(1)][J-(1)] + (*X)[IX-(1)]*TEMP1 + (*Y)[IY-(1)]*TEMP2
						IX = IX + (*INCX)
						IY = IY + (*INCY)
					}
					(*A)[J-(1)][J-(1)] = real((*A)[J-(1)][J-(1)]) + real((*X)[JX-(1)]*TEMP1+(*Y)[JY-(1)]*TEMP2)
				} else {
					(*A)[J-(1)][J-(1)] = real((*A)[J-(1)][J-(1)])
				}
				JX = JX + (*INCX)
				JY = JY + (*INCY)
			}
		}
	} else {
		//*
		//*        Form  A  when A is stored in the lower triangle.
		//*
		if ((*INCX) == 1) && ((*INCY) == 1) {
			for J = 1; J <= (*N); J++ {
				if ((*X)[J-(1)] != ZERO) || ((*Y)[J-(1)] != ZERO) {
					TEMP1 = (*ALPHA) * intrinsic.CONJG((*Y)[J-(1)])
					TEMP2 = intrinsic.CONJG((*ALPHA) * (*X)[J-(1)])
					(*A)[J-(1)][J-(1)] = real((*A)[J-(1)][J-(1)]) + real((*X)[J-(1)]*TEMP1+(*Y)[J-(1)]*TEMP2)
					for I = J + 1; I <= (*N); I++ {
						(*A)[I-(1)][J-(1)] = (*A)[I-(1)][J-(1)] + (*X)[I-(1)]*TEMP1 + (*Y)[I-(1)]*TEMP2
					}
				} else {
					(*A)[J-(1)][J-(1)] = real((*A)[J-(1)][J-(1)])
				}
			}
		} else {
			for J = 1; J <= (*N); J++ {
				if ((*X)[JX-(1)] != ZERO) || ((*Y)[JY-(1)] != ZERO) {
					TEMP1 = (*ALPHA) * intrinsic.CONJG((*Y)[JY-(1)])
					TEMP2 = intrinsic.CONJG((*ALPHA) * (*X)[JX-(1)])
					(*A)[J-(1)][J-(1)] = real((*A)[J-(1)][J-(1)]) + real((*X)[JX-(1)]*TEMP1+(*Y)[JY-(1)]*TEMP2)
					IX = JX
					IY = JY
					for I = J + 1; I <= (*N); I++ {
						IX = IX + (*INCX)
						IY = IY + (*INCY)
						(*A)[I-(1)][J-(1)] = (*A)[I-(1)][J-(1)] + (*X)[IX-(1)]*TEMP1 + (*Y)[IY-(1)]*TEMP2
					}
				} else {
					(*A)[J-(1)][J-(1)] = real((*A)[J-(1)][J-(1)])
				}
				JX = JX + (*INCX)
				JY = JY + (*INCY)
			}
		}
	}
	//*
	return
	//*
	//*     End of CHER2 .
	//*
}
