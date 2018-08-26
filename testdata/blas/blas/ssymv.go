package main
//*> \brief \b SSYMV
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE SSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
//*
//*       .. Scalar Arguments ..
//*       REAL ALPHA,BETA
//*       INTEGER INCX,INCY,LDA,N
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
//*> SSYMV  performs the matrix-vector  operation
//*>
//*>    y := alpha*A*x + beta*y,
//*>
//*> where alpha and beta are scalars, x and y are n element vectors and
//*> A is an n by n symmetric matrix.
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
//*>          ALPHA is REAL
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] A
//*> \verbatim
//*>          A is REAL array, dimension ( LDA, N )
//*>           Before entry with  UPLO = 'U' or 'u', the leading n by n
//*>           upper triangular part of the array A must contain the upper
//*>           triangular part of the symmetric matrix and the strictly
//*>           lower triangular part of A is not referenced.
//*>           Before entry with UPLO = 'L' or 'l', the leading n by n
//*>           lower triangular part of the array A must contain the lower
//*>           triangular part of the symmetric matrix and the strictly
//*>           upper triangular part of A is not referenced.
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in the calling (sub) program. LDA must be at least
//*>           max( 1, n ).
//*> \endverbatim
//*>
//*> \param[in] X
//*> \verbatim
//*>          X is REAL array, dimension at least
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
//*> \param[in] BETA
//*> \verbatim
//*>          BETA is REAL
//*>           On entry, BETA specifies the scalar beta. When BETA is
//*>           supplied as zero then Y need not be set on input.
//*> \endverbatim
//*>
//*> \param[in,out] Y
//*> \verbatim
//*>          Y is REAL array, dimension at least
//*>           ( 1 + ( n - 1 )*abs( INCY ) ).
//*>           Before entry, the incremented array Y must contain the n
//*>           element vector y. On exit, Y is overwritten by the updated
//*>           vector y.
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
func SSYMV(UPLO *byte, N *int, ALPHA *float64, A *[][]float64, LDA *int, X *[]float64, INCX *int, BETA *float64, Y *[]float64, INCY *int) {
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
	if !LSAME(UPLO, func()*[]byte{y:=[]byte("U");return &y}()) && !LSAME(UPLO, func()*[]byte{y:=[]byte("L");return &y}()) {
		INFO = 1
	} else if (*N) < 0 {
		INFO = 2
	} else if (*LDA) < MAX(func()*int{y:=1;return &y}(), N) {
		INFO = 5
	} else if (*INCX) == 0 {
		INFO = 7
	} else if (*INCY) == 0 {
		INFO = 10
	}
	if INFO != 0 {
		XERBLA(func()*[]byte{y:=[]byte("SSYMV ");return &y}(), &(INFO))
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
	//*     Start the operations. In this version the elements of A are
	//*     accessed sequentially with one pass through the triangular part
	//*     of A.
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
	if LSAME(UPLO, func()*[]byte{y:=[]byte("U");return &y}()) {
		//*
		//*        Form  y  when A is stored in upper triangle.
		//*
		if ((*INCX) == 1) && ((*INCY) == 1) {
			for J = 1; J <= (*N); J++ {
				TEMP1 = (*ALPHA) * (*X)[J-(1)]
				TEMP2 = ZERO
				for I = 1; I <= J-1; I++ {
					(*Y)[I-(1)] = (*Y)[I-(1)] + TEMP1*(*A)[I-(1)][J-(1)]
					TEMP2 = TEMP2 + (*A)[I-(1)][J-(1)]*(*X)[I-(1)]
				}
				(*Y)[J-(1)] = (*Y)[J-(1)] + TEMP1*(*A)[J-(1)][J-(1)] + (*ALPHA)*TEMP2
			}
		} else {
			JX = KX
			JY = KY
			for J = 1; J <= (*N); J++ {
				TEMP1 = (*ALPHA) * (*X)[JX-(1)]
				TEMP2 = ZERO
				IX = KX
				IY = KY
				for I = 1; I <= J-1; I++ {
					(*Y)[IY-(1)] = (*Y)[IY-(1)] + TEMP1*(*A)[I-(1)][J-(1)]
					TEMP2 = TEMP2 + (*A)[I-(1)][J-(1)]*(*X)[IX-(1)]
					IX = IX + (*INCX)
					IY = IY + (*INCY)
				}
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + TEMP1*(*A)[J-(1)][J-(1)] + (*ALPHA)*TEMP2
				JX = JX + (*INCX)
				JY = JY + (*INCY)
			}
		}
	} else {
		//*
		//*        Form  y  when A is stored in lower triangle.
		//*
		if ((*INCX) == 1) && ((*INCY) == 1) {
			for J = 1; J <= (*N); J++ {
				TEMP1 = (*ALPHA) * (*X)[J-(1)]
				TEMP2 = ZERO
				(*Y)[J-(1)] = (*Y)[J-(1)] + TEMP1*(*A)[J-(1)][J-(1)]
				for I = J + 1; I <= (*N); I++ {
					(*Y)[I-(1)] = (*Y)[I-(1)] + TEMP1*(*A)[I-(1)][J-(1)]
					TEMP2 = TEMP2 + (*A)[I-(1)][J-(1)]*(*X)[I-(1)]
				}
				(*Y)[J-(1)] = (*Y)[J-(1)] + (*ALPHA)*TEMP2
			}
		} else {
			JX = KX
			JY = KY
			for J = 1; J <= (*N); J++ {
				TEMP1 = (*ALPHA) * (*X)[JX-(1)]
				TEMP2 = ZERO
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + TEMP1*(*A)[J-(1)][J-(1)]
				IX = JX
				IY = JY
				for I = J + 1; I <= (*N); I++ {
					IX = IX + (*INCX)
					IY = IY + (*INCY)
					(*Y)[IY-(1)] = (*Y)[IY-(1)] + TEMP1*(*A)[I-(1)][J-(1)]
					TEMP2 = TEMP2 + (*A)[I-(1)][J-(1)]*(*X)[IX-(1)]
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
	//*     End of SSYMV .
	//*
}
