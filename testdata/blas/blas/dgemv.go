package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b DGEMV
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
//*
//*       .. Scalar Arguments ..
//*       DOUBLE PRECISION ALPHA,BETA
//*       INTEGER INCX,INCY,LDA,M,N
//*       CHARACTER TRANS
//*       ..
//*       .. Array Arguments ..
//*       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> DGEMV  performs one of the matrix-vector operations
//*>
//*>    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
//*>
//*> where alpha and beta are scalars, x and y are vectors and A is an
//*> m by n matrix.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] TRANS
//*> \verbatim
//*>          TRANS is CHARACTER*1
//*>           On entry, TRANS specifies the operation to be performed as
//*>           follows:
//*>
//*>              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
//*>
//*>              TRANS = 'T' or 't'   y := alpha*A**T*x + beta*y.
//*>
//*>              TRANS = 'C' or 'c'   y := alpha*A**T*x + beta*y.
//*> \endverbatim
//*>
//*> \param[in] M
//*> \verbatim
//*>          M is INTEGER
//*>           On entry, M specifies the number of rows of the matrix A.
//*>           M must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry, N specifies the number of columns of the matrix A.
//*>           N must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] ALPHA
//*> \verbatim
//*>          ALPHA is DOUBLE PRECISION.
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] A
//*> \verbatim
//*>          A is DOUBLE PRECISION array, dimension ( LDA, N )
//*>           Before entry, the leading m by n part of the array A must
//*>           contain the matrix of coefficients.
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in the calling (sub) program. LDA must be at least
//*>           max( 1, m ).
//*> \endverbatim
//*>
//*> \param[in] X
//*> \verbatim
//*>          X is DOUBLE PRECISION array, dimension at least
//*>           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
//*>           and at least
//*>           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
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
//*>          BETA is DOUBLE PRECISION.
//*>           On entry, BETA specifies the scalar beta. When BETA is
//*>           supplied as zero then Y need not be set on input.
//*> \endverbatim
//*>
//*> \param[in,out] Y
//*> \verbatim
//*>          Y is DOUBLE PRECISION array, dimension at least
//*>           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
//*>           and at least
//*>           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
//*>           Before entry with BETA non-zero, the incremented array Y
//*>           must contain the vector y. On exit, Y is overwritten by the
//*>           updated vector y.
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
//*> \ingroup double_blas_level2
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
func DGEMV(TRANS *byte, M *int, N *int, ALPHA *float64, A *[][]float64, LDA *int, X *[]float64, INCX *int, BETA *float64, Y *[]float64, INCY *int) {
	var ONE float64 = 1.0e+0
	var ZERO float64 = 0.0e+0
	var TEMP float64
	var I int
	var INFO int
	var IX int
	var IY int
	var J int
	var JX int
	var JY int
	var KX int
	var KY int
	var LENX int
	var LENY int
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
	if !LSAME(TRANS, func()*byte{y:=byte('N');return &y}()) && !LSAME(TRANS, func()*byte{y:=byte('T');return &y}()) && !LSAME(TRANS, func()*byte{y:=byte('C');return &y}()) {
		INFO = 1
	} else if (*M) < 0 {
		INFO = 2
	} else if (*N) < 0 {
		INFO = 3
	} else if (*LDA) < intrinsic.MAX(func()*int{y:=1;return &y}(), (*M)) {
		INFO = 6
	} else if (*INCX) == 0 {
		INFO = 8
	} else if (*INCY) == 0 {
		INFO = 11
	}
	if INFO != 0 {
		XERBLA(func()*[]byte{y:=[]byte("DGEMV ");return &y}(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if ((*M) == 0) || ((*N) == 0) || (((*ALPHA) == ZERO) && ((*BETA) == ONE)) {
		return
	}
	//*
	//*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
	//*     up the start points in  X  and  Y.
	//*
	if LSAME(TRANS, func()*byte{y:=byte('N');return &y}()) {
		LENX = (*N)
		LENY = (*M)
	} else {
		LENX = (*M)
		LENY = (*N)
	}
	if (*INCX) > 0 {
		KX = 1
	} else {
		KX = 1 - (LENX-1)*(*INCX)
	}
	if (*INCY) > 0 {
		KY = 1
	} else {
		KY = 1 - (LENY-1)*(*INCY)
	}
	//*
	//*     Start the operations. In this version the elements of A are
	//*     accessed sequentially with one pass through A.
	//*
	//*     First form  y := beta*y.
	//*
	if (*BETA) != ONE {
		if (*INCY) == 1 {
			if (*BETA) == ZERO {
				for I = 1; I <= LENY; I++ {
					(*Y)[I-(1)] = ZERO
				}
			} else {
				for I = 1; I <= LENY; I++ {
					(*Y)[I-(1)] = (*BETA) * (*Y)[I-(1)]
				}
			}
		} else {
			IY = KY
			if (*BETA) == ZERO {
				for I = 1; I <= LENY; I++ {
					(*Y)[IY-(1)] = ZERO
					IY = IY + (*INCY)
				}
			} else {
				for I = 1; I <= LENY; I++ {
					(*Y)[IY-(1)] = (*BETA) * (*Y)[IY-(1)]
					IY = IY + (*INCY)
				}
			}
		}
	}
	if (*ALPHA) == ZERO {
		return
	}
	if LSAME(TRANS, func()*byte{y:=byte('N');return &y}()) {
		//*
		//*        Form  y := alpha*A*x + y.
		//*
		JX = KX
		if (*INCY) == 1 {
			for J = 1; J <= (*N); J++ {
				TEMP = (*ALPHA) * (*X)[JX-(1)]
				for I = 1; I <= (*M); I++ {
					(*Y)[I-(1)] = (*Y)[I-(1)] + TEMP*(*A)[I-(1)][J-(1)]
				}
				JX = JX + (*INCX)
			}
		} else {
			for J = 1; J <= (*N); J++ {
				TEMP = (*ALPHA) * (*X)[JX-(1)]
				IY = KY
				for I = 1; I <= (*M); I++ {
					(*Y)[IY-(1)] = (*Y)[IY-(1)] + TEMP*(*A)[I-(1)][J-(1)]
					IY = IY + (*INCY)
				}
				JX = JX + (*INCX)
			}
		}
	} else {
		//*
		//*        Form  y := alpha*A**T*x + y.
		//*
		JY = KY
		if (*INCX) == 1 {
			for J = 1; J <= (*N); J++ {
				TEMP = ZERO
				for I = 1; I <= (*M); I++ {
					TEMP = TEMP + (*A)[I-(1)][J-(1)]*(*X)[I-(1)]
				}
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + (*ALPHA)*TEMP
				JY = JY + (*INCY)
			}
		} else {
			for J = 1; J <= (*N); J++ {
				TEMP = ZERO
				IX = KX
				for I = 1; I <= (*M); I++ {
					TEMP = TEMP + (*A)[I-(1)][J-(1)]*(*X)[IX-(1)]
					IX = IX + (*INCX)
				}
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + (*ALPHA)*TEMP
				JY = JY + (*INCY)
			}
		}
	}
	//*
	return
	//*
	//*     End of DGEMV .
	//*
}
