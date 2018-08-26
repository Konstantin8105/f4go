package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b ZGBMV
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE ZGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX*16 ALPHA,BETA
//*       INTEGER INCX,INCY,KL,KU,LDA,M,N
//*       CHARACTER TRANS
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX*16 A(LDA,*),X(*),Y(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> ZGBMV  performs one of the matrix-vector operations
//*>
//*>    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
//*>
//*>    y := alpha*A**H*x + beta*y,
//*>
//*> where alpha and beta are scalars, x and y are vectors and A is an
//*> m by n band matrix, with kl sub-diagonals and ku super-diagonals.
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
//*>              TRANS = 'C' or 'c'   y := alpha*A**H*x + beta*y.
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
//*> \param[in] KL
//*> \verbatim
//*>          KL is INTEGER
//*>           On entry, KL specifies the number of sub-diagonals of the
//*>           matrix A. KL must satisfy  0 .le. KL.
//*> \endverbatim
//*>
//*> \param[in] KU
//*> \verbatim
//*>          KU is INTEGER
//*>           On entry, KU specifies the number of super-diagonals of the
//*>           matrix A. KU must satisfy  0 .le. KU.
//*> \endverbatim
//*>
//*> \param[in] ALPHA
//*> \verbatim
//*>          ALPHA is COMPLEX*16
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] A
//*> \verbatim
//*>          A is COMPLEX*16 array, dimension ( LDA, N )
//*>           Before entry, the leading ( kl + ku + 1 ) by n part of the
//*>           array A must contain the matrix of coefficients, supplied
//*>           column by column, with the leading diagonal of the matrix in
//*>           row ( ku + 1 ) of the array, the first super-diagonal
//*>           starting at position 2 in row ku, the first sub-diagonal
//*>           starting at position 1 in row ( ku + 2 ), and so on.
//*>           Elements in the array A that do not correspond to elements
//*>           in the band matrix (such as the top left ku by ku triangle)
//*>           are not referenced.
//*>           The following program segment will transfer a band matrix
//*>           from conventional full matrix storage to band storage:
//*>
//*>                 DO 20, J = 1, N
//*>                    K = KU + 1 - J
//*>                    DO 10, I = MAX( 1, J - KU ), MIN( M, J + KL )
//*>                       A( K + I, J ) = matrix( I, J )
//*>              10    CONTINUE
//*>              20 CONTINUE
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in the calling (sub) program. LDA must be at least
//*>           ( kl + ku + 1 ).
//*> \endverbatim
//*>
//*> \param[in] X
//*> \verbatim
//*>          X is COMPLEX*16 array, dimension at least
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
//*>          BETA is COMPLEX*16
//*>           On entry, BETA specifies the scalar beta. When BETA is
//*>           supplied as zero then Y need not be set on input.
//*> \endverbatim
//*>
//*> \param[in,out] Y
//*> \verbatim
//*>          Y is COMPLEX*16 array, dimension at least
//*>           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
//*>           and at least
//*>           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
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
//*> \ingroup complex16_blas_level2
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
func ZGBMV(TRANS *byte, M *int, N *int, KL *int, KU *int, ALPHA *complex128, A *[][]complex128, LDA *int, X *[]complex128, INCX *int, BETA *complex128, Y *[]complex128, INCY *int) {
	var ONE complex128 = (1.0e+0 + (0.0e+0)*1i)
	var ZERO complex128 = (0.0e+0 + (0.0e+0)*1i)
	var TEMP complex128
	var I int
	var INFO int
	var IX int
	var IY int
	var J int
	var JX int
	var JY int
	var K int
	var KUP1 int
	var KX int
	var KY int
	var LENX int
	var LENY int
	var NOCONJ bool
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
	if !LSAME(TRANS, func()*[]byte{y:=[]byte("N");return &y}()) && !LSAME(TRANS, func()*[]byte{y:=[]byte("T");return &y}()) && !LSAME(TRANS, func()*[]byte{y:=[]byte("C");return &y}()) {
		INFO = 1
	} else if (*M) < 0 {
		INFO = 2
	} else if (*N) < 0 {
		INFO = 3
	} else if (*KL) < 0 {
		INFO = 4
	} else if (*KU) < 0 {
		INFO = 5
	} else if (*LDA) < ((*KL) + (*KU) + 1) {
		INFO = 8
	} else if (*INCX) == 0 {
		INFO = 10
	} else if (*INCY) == 0 {
		INFO = 13
	}
	if INFO != 0 {
		XERBLA(func()*[]byte{y:=[]byte("ZGBMV ");return &y}(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if ((*M) == 0) || ((*N) == 0) || (((*ALPHA) == ZERO) && ((*BETA) == ONE)) {
		return
	}
	//*
	NOCONJ = LSAME(TRANS, func()*[]byte{y:=[]byte("T");return &y}())
	//*
	//*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
	//*     up the start points in  X  and  Y.
	//*
	if LSAME(TRANS, func()*[]byte{y:=[]byte("N");return &y}()) {
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
	//*     accessed sequentially with one pass through the band part of A.
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
	KUP1 = (*KU) + 1
	if LSAME(TRANS, func()*[]byte{y:=[]byte("N");return &y}()) {
		//*
		//*        Form  y := alpha*A*x + y.
		//*
		JX = KX
		if (*INCY) == 1 {
			for J = 1; J <= (*N); J++ {
				TEMP = (*ALPHA) * (*X)[JX-(1)]
				K = KUP1 - J
				for I = MAX(func()*int{y:=1;return &y}(), J-(*KU)); I <= intrinsic.MIN((*M), J+(*KL)); I++ {
					(*Y)[I-(1)] = (*Y)[I-(1)] + TEMP*(*A)[K+I-(1)][J-(1)]
				}
				JX = JX + (*INCX)
			}
		} else {
			for J = 1; J <= (*N); J++ {
				TEMP = (*ALPHA) * (*X)[JX-(1)]
				IY = KY
				K = KUP1 - J
				for I = MAX(func()*int{y:=1;return &y}(), J-(*KU)); I <= intrinsic.MIN((*M), J+(*KL)); I++ {
					(*Y)[IY-(1)] = (*Y)[IY-(1)] + TEMP*(*A)[K+I-(1)][J-(1)]
					IY = IY + (*INCY)
				}
				JX = JX + (*INCX)
				if J > (*KU) {
					KY = KY + (*INCY)
				}
			}
		}
	} else {
		//*
		//*        Form  y := alpha*A**T*x + y  or  y := alpha*A**H*x + y.
		//*
		JY = KY
		if (*INCX) == 1 {
			for J = 1; J <= (*N); J++ {
				TEMP = ZERO
				K = KUP1 - J
				if NOCONJ {
					for I = MAX(func()*int{y:=1;return &y}(), J-(*KU)); I <= intrinsic.MIN((*M), J+(*KL)); I++ {
						TEMP = TEMP + (*A)[K+I-(1)][J-(1)]*(*X)[I-(1)]
					}
				} else {
					for I = MAX(func()*int{y:=1;return &y}(), J-(*KU)); I <= intrinsic.MIN((*M), J+(*KL)); I++ {
						TEMP = TEMP + DCONJG(&((*A)[K+I-(1)][J-(1)]))*(*X)[I-(1)]
					}
				}
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + (*ALPHA)*TEMP
				JY = JY + (*INCY)
			}
		} else {
			for J = 1; J <= (*N); J++ {
				TEMP = ZERO
				IX = KX
				K = KUP1 - J
				if NOCONJ {
					for I = MAX(func()*int{y:=1;return &y}(), J-(*KU)); I <= intrinsic.MIN((*M), J+(*KL)); I++ {
						TEMP = TEMP + (*A)[K+I-(1)][J-(1)]*(*X)[IX-(1)]
						IX = IX + (*INCX)
					}
				} else {
					for I = MAX(func()*int{y:=1;return &y}(), J-(*KU)); I <= intrinsic.MIN((*M), J+(*KL)); I++ {
						TEMP = TEMP + DCONJG(&((*A)[K+I-(1)][J-(1)]))*(*X)[IX-(1)]
						IX = IX + (*INCX)
					}
				}
				(*Y)[JY-(1)] = (*Y)[JY-(1)] + (*ALPHA)*TEMP
				JY = JY + (*INCY)
				if J > (*KU) {
					KX = KX + (*INCX)
				}
			}
		}
	}
	//*
	return
	//*
	//*     End of ZGBMV .
	//*
}
