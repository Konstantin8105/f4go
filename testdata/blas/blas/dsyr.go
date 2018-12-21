package main

import "github.com/Konstantin8105/f4go/intrinsic"

//*> \brief \b DSYR
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE DSYR(UPLO,N,ALPHA,X,INCX,A,LDA)
//*
//*       .. Scalar Arguments ..
//*       DOUBLE PRECISION ALPHA
//*       INTEGER INCX,LDA,N
//*       CHARACTER UPLO
//*       ..
//*       .. Array Arguments ..
//*       DOUBLE PRECISION A(LDA,*),X(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> DSYR   performs the symmetric rank 1 operation
//*>
//*>    A := alpha*x*x**T + A,
//*>
//*> where alpha is a real scalar, x is an n element vector and A is an
//*> n by n symmetric matrix.
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
//*>          ALPHA is DOUBLE PRECISION.
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] X
//*> \verbatim
//*>          X is DOUBLE PRECISION array, dimension at least
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
//*> \param[in,out] A
//*> \verbatim
//*>          A is DOUBLE PRECISION array, dimension ( LDA, N )
//*>           Before entry with  UPLO = 'U' or 'u', the leading n by n
//*>           upper triangular part of the array A must contain the upper
//*>           triangular part of the symmetric matrix and the strictly
//*>           lower triangular part of A is not referenced. On exit, the
//*>           upper triangular part of the array A is overwritten by the
//*>           upper triangular part of the updated matrix.
//*>           Before entry with UPLO = 'L' or 'l', the leading n by n
//*>           lower triangular part of the array A must contain the lower
//*>           triangular part of the symmetric matrix and the strictly
//*>           upper triangular part of A is not referenced. On exit, the
//*>           lower triangular part of the array A is overwritten by the
//*>           lower triangular part of the updated matrix.
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
//*> \ingroup double_blas_level2
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
func DSYR(UPLO *byte, N *int, ALPHA *float64, X *[]float64, INCX *int, A *[][]float64, LDA *int) {
	var ZERO float64 = 0.0e+0
	var TEMP float64
	var I int
	var INFO int
	var IX int
	var J int
	var JX int
	var KX int
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
	} else if (*LDA) < intrinsic.MAX(int(1), (*N)) {
		INFO = 7
	}
	if INFO != 0 {
		XERBLA(func() *[]byte { y := []byte("DSYR  "); return &y }(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if ((*N) == 0) || ((*ALPHA) == ZERO) {
		return
	}
	//*
	//*     Set the start point in X if the increment is not unity.
	//*
	if (*INCX) <= 0 {
		KX = 1 - ((*N)-1)*(*INCX)
	} else if (*INCX) != 1 {
		KX = 1
	}
	//*
	//*     Start the operations. In this version the elements of A are
	//*     accessed sequentially with one pass through the triangular part
	//*     of A.
	//*
	if LSAME(UPLO, func() *byte { y := byte('U'); return &y }()) {
		//*
		//*        Form  A  when A is stored in upper triangle.
		//*
		if (*INCX) == 1 {
			for J = 1; J <= (*N); J++ {
				if (*X)[J-(1)] != ZERO {
					TEMP = (*ALPHA) * (*X)[J-(1)]
					for I = 1; I <= J; I++ {
						(*A)[I-(1)][J-(1)] = (*A)[I-(1)][J-(1)] + (*X)[I-(1)]*TEMP
					}
				}
			}
		} else {
			JX = KX
			for J = 1; J <= (*N); J++ {
				if (*X)[JX-(1)] != ZERO {
					TEMP = (*ALPHA) * (*X)[JX-(1)]
					IX = KX
					for I = 1; I <= J; I++ {
						(*A)[I-(1)][J-(1)] = (*A)[I-(1)][J-(1)] + (*X)[IX-(1)]*TEMP
						IX = IX + (*INCX)
					}
				}
				JX = JX + (*INCX)
			}
		}
	} else {
		//*
		//*        Form  A  when A is stored in lower triangle.
		//*
		if (*INCX) == 1 {
			for J = 1; J <= (*N); J++ {
				if (*X)[J-(1)] != ZERO {
					TEMP = (*ALPHA) * (*X)[J-(1)]
					for I = J; I <= (*N); I++ {
						(*A)[I-(1)][J-(1)] = (*A)[I-(1)][J-(1)] + (*X)[I-(1)]*TEMP
					}
				}
			}
		} else {
			JX = KX
			for J = 1; J <= (*N); J++ {
				if (*X)[JX-(1)] != ZERO {
					TEMP = (*ALPHA) * (*X)[JX-(1)]
					IX = JX
					for I = J; I <= (*N); I++ {
						(*A)[I-(1)][J-(1)] = (*A)[I-(1)][J-(1)] + (*X)[IX-(1)]*TEMP
						IX = IX + (*INCX)
					}
				}
				JX = JX + (*INCX)
			}
		}
	}
	//*
	return
	//*
	//*     End of DSYR  .
	//*
}
