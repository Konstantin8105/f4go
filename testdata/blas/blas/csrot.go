package main
//*> \brief \b CSROT
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CSROT( N, CX, INCX, CY, INCY, C, S )
//*
//*       .. Scalar Arguments ..
//*       INTEGER           INCX, INCY, N
//*       REAL              C, S
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX           CX( * ), CY( * )
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> CSROT applies a plane rotation, where the cos and sin (c and s) are real
//*> and the vectors cx and cy are complex.
//*> jack dongarra, linpack, 3/11/78.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry, N specifies the order of the vectors cx and cy.
//*>           N must be at least zero.
//*> \endverbatim
//*>
//*> \param[in,out] CX
//*> \verbatim
//*>          CX is COMPLEX array, dimension at least
//*>           ( 1 + ( N - 1 )*abs( INCX ) ).
//*>           Before entry, the incremented array CX must contain the n
//*>           element vector cx. On exit, CX is overwritten by the updated
//*>           vector cx.
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>           On entry, INCX specifies the increment for the elements of
//*>           CX. INCX must not be zero.
//*> \endverbatim
//*>
//*> \param[in,out] CY
//*> \verbatim
//*>          CY is COMPLEX array, dimension at least
//*>           ( 1 + ( N - 1 )*abs( INCY ) ).
//*>           Before entry, the incremented array CY must contain the n
//*>           element vector cy. On exit, CY is overwritten by the updated
//*>           vector cy.
//*> \endverbatim
//*>
//*> \param[in] INCY
//*> \verbatim
//*>          INCY is INTEGER
//*>           On entry, INCY specifies the increment for the elements of
//*>           CY. INCY must not be zero.
//*> \endverbatim
//*>
//*> \param[in] C
//*> \verbatim
//*>          C is REAL
//*>           On entry, C specifies the cosine, cos.
//*> \endverbatim
//*>
//*> \param[in] S
//*> \verbatim
//*>          S is REAL
//*>           On entry, S specifies the sine, sin.
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
//*> \ingroup complex_blas_level1
//*
//*  =====================================================================
func CSROT(N *int, CX *[]complex64, INCX *int, CY *[]complex64, INCY *int, C *float64, S *float64) {
	var I int
	var IX int
	var IY int
	var CTEMP complex64
	//*
	//*  -- Reference BLAS level1 routine (version 3.7.0) --
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
	//*     .. Local Scalars ..
	//*     ..
	//*     .. Executable Statements ..
	//*
	if (*N) <= 0 {
		return
	}
	if (*INCX) == 1 && (*INCY) == 1 {
		//*
		//*        code for both increments equal to 1
		//*
		for I = 1; I <= (*N); I++ {
			CTEMP = (*C)*(*CX)[I-(1)] + (*S)*(*CY)[I-(1)]
			(*CY)[I-(1)] = (*C)*(*CY)[I-(1)] - (*S)*(*CX)[I-(1)]
			(*CX)[I-(1)] = CTEMP
		}
	} else {
		//*
		//*        code for unequal increments or equal increments not equal
		//*          to 1
		//*
		IX = 1
		IY = 1
		if (*INCX) < 0 {
			IX = (-(*N)+1)*(*INCX) + 1
		}
		if (*INCY) < 0 {
			IY = (-(*N)+1)*(*INCY) + 1
		}
		for I = 1; I <= (*N); I++ {
			CTEMP = (*C)*(*CX)[IX-(1)] + (*S)*(*CY)[IY-(1)]
			(*CY)[IY-(1)] = (*C)*(*CY)[IY-(1)] - (*S)*(*CX)[IX-(1)]
			(*CX)[IX-(1)] = CTEMP
			IX = IX + (*INCX)
			IY = IY + (*INCY)
		}
	}
	return
}
