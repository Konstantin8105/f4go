package main

import "github.com/Konstantin8105/f4go/intrinsic"

//*> \brief \b CHPR2
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CHPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX ALPHA
//*       INTEGER INCX,INCY,N
//*       CHARACTER UPLO
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX AP(*),X(*),Y(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> CHPR2  performs the hermitian rank 2 operation
//*>
//*>    A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
//*>
//*> where alpha is a scalar, x and y are n element vectors and A is an
//*> n by n hermitian matrix, supplied in packed form.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] UPLO
//*> \verbatim
//*>          UPLO is CHARACTER*1
//*>           On entry, UPLO specifies whether the upper or lower
//*>           triangular part of the matrix A is supplied in the packed
//*>           array AP as follows:
//*>
//*>              UPLO = 'U' or 'u'   The upper triangular part of A is
//*>                                  supplied in AP.
//*>
//*>              UPLO = 'L' or 'l'   The lower triangular part of A is
//*>                                  supplied in AP.
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
//*> \param[in,out] AP
//*> \verbatim
//*>          AP is COMPLEX array, dimension at least
//*>           ( ( n*( n + 1 ) )/2 ).
//*>           Before entry with  UPLO = 'U' or 'u', the array AP must
//*>           contain the upper triangular part of the hermitian matrix
//*>           packed sequentially, column by column, so that AP( 1 )
//*>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
//*>           and a( 2, 2 ) respectively, and so on. On exit, the array
//*>           AP is overwritten by the upper triangular part of the
//*>           updated matrix.
//*>           Before entry with UPLO = 'L' or 'l', the array AP must
//*>           contain the lower triangular part of the hermitian matrix
//*>           packed sequentially, column by column, so that AP( 1 )
//*>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
//*>           and a( 3, 1 ) respectively, and so on. On exit, the array
//*>           AP is overwritten by the lower triangular part of the
//*>           updated matrix.
//*>           Note that the imaginary parts of the diagonal elements need
//*>           not be set, they are assumed to be zero, and on exit they
//*>           are set to zero.
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
func CHPR2(UPLO *byte, N *int, ALPHA *complex64, X *[]complex64, INCX *int, Y *[]complex64, INCY *int, AP *[]complex64) {
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
	var K int
	var KK int
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
	}
	if INFO != 0 {
		XERBLA(func() *[]byte { y := []byte("CHPR2 "); return &y }(), &(INFO))
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
	//*     Start the operations. In this version the elements of the array AP
	//*     are accessed sequentially with one pass through AP.
	//*
	KK = 1
	if LSAME(UPLO, func() *byte { y := byte('U'); return &y }()) {
		//*
		//*        Form  A  when upper triangle is stored in AP.
		//*
		if ((*INCX) == 1) && ((*INCY) == 1) {
			for J = 1; J <= (*N); J++ {
				if ((*X)[J-(1)] != ZERO) || ((*Y)[J-(1)] != ZERO) {
					TEMP1 = (*ALPHA) * intrinsic.CONJG((*Y)[J-(1)])
					TEMP2 = intrinsic.CONJG((*ALPHA) * (*X)[J-(1)])
					K = KK
					for I = 1; I <= J-1; I++ {
						(*AP)[K-(1)] = (*AP)[K-(1)] + (*X)[I-(1)]*TEMP1 + (*Y)[I-(1)]*TEMP2
						K = K + 1
					}
					(*AP)[KK+J-1-(1)] = real((*AP)[KK+J-1-(1)]) + real((*X)[J-(1)]*TEMP1+(*Y)[J-(1)]*TEMP2)
				} else {
					(*AP)[KK+J-1-(1)] = real((*AP)[KK+J-1-(1)])
				}
				KK = KK + J
			}
		} else {
			for J = 1; J <= (*N); J++ {
				if ((*X)[JX-(1)] != ZERO) || ((*Y)[JY-(1)] != ZERO) {
					TEMP1 = (*ALPHA) * intrinsic.CONJG((*Y)[JY-(1)])
					TEMP2 = intrinsic.CONJG((*ALPHA) * (*X)[JX-(1)])
					IX = KX
					IY = KY
					for K = KK; K <= KK+J-2; K++ {
						(*AP)[K-(1)] = (*AP)[K-(1)] + (*X)[IX-(1)]*TEMP1 + (*Y)[IY-(1)]*TEMP2
						IX = IX + (*INCX)
						IY = IY + (*INCY)
					}
					(*AP)[KK+J-1-(1)] = real((*AP)[KK+J-1-(1)]) + real((*X)[JX-(1)]*TEMP1+(*Y)[JY-(1)]*TEMP2)
				} else {
					(*AP)[KK+J-1-(1)] = real((*AP)[KK+J-1-(1)])
				}
				JX = JX + (*INCX)
				JY = JY + (*INCY)
				KK = KK + J
			}
		}
	} else {
		//*
		//*        Form  A  when lower triangle is stored in AP.
		//*
		if ((*INCX) == 1) && ((*INCY) == 1) {
			for J = 1; J <= (*N); J++ {
				if ((*X)[J-(1)] != ZERO) || ((*Y)[J-(1)] != ZERO) {
					TEMP1 = (*ALPHA) * intrinsic.CONJG((*Y)[J-(1)])
					TEMP2 = intrinsic.CONJG((*ALPHA) * (*X)[J-(1)])
					(*AP)[KK-(1)] = real((*AP)[KK-(1)]) + real((*X)[J-(1)]*TEMP1+(*Y)[J-(1)]*TEMP2)
					K = KK + 1
					for I = J + 1; I <= (*N); I++ {
						(*AP)[K-(1)] = (*AP)[K-(1)] + (*X)[I-(1)]*TEMP1 + (*Y)[I-(1)]*TEMP2
						K = K + 1
					}
				} else {
					(*AP)[KK-(1)] = real((*AP)[KK-(1)])
				}
				KK = KK + (*N) - J + 1
			}
		} else {
			for J = 1; J <= (*N); J++ {
				if ((*X)[JX-(1)] != ZERO) || ((*Y)[JY-(1)] != ZERO) {
					TEMP1 = (*ALPHA) * intrinsic.CONJG((*Y)[JY-(1)])
					TEMP2 = intrinsic.CONJG((*ALPHA) * (*X)[JX-(1)])
					(*AP)[KK-(1)] = real((*AP)[KK-(1)]) + real((*X)[JX-(1)]*TEMP1+(*Y)[JY-(1)]*TEMP2)
					IX = JX
					IY = JY
					for K = KK + 1; K <= KK+(*N)-J; K++ {
						IX = IX + (*INCX)
						IY = IY + (*INCY)
						(*AP)[K-(1)] = (*AP)[K-(1)] + (*X)[IX-(1)]*TEMP1 + (*Y)[IY-(1)]*TEMP2
					}
				} else {
					(*AP)[KK-(1)] = real((*AP)[KK-(1)])
				}
				JX = JX + (*INCX)
				JY = JY + (*INCY)
				KK = KK + (*N) - J + 1
			}
		}
	}
	//*
	return
	//*
	//*     End of CHPR2 .
	//*
}
