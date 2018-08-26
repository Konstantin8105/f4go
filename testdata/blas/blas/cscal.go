package main
//*> \brief \b CSCAL
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CSCAL(N,CA,CX,INCX)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX CA
//*       INTEGER INCX,N
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX CX(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    CSCAL scales a vector by a constant.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>         number of elements in input vector(s)
//*> \endverbatim
//*>
//*> \param[in] CA
//*> \verbatim
//*>          CA is COMPLEX
//*>           On entry, CA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in,out] CX
//*> \verbatim
//*>          CX is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>         storage spacing between elements of CX
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
//*> \date November 2017
//*
//*> \ingroup complex_blas_level1
//*
//*> \par Further Details:
//*  =====================
//*>
//*> \verbatim
//*>
//*>     jack dongarra, linpack,  3/11/78.
//*>     modified 3/93 to return if incx .le. 0.
//*>     modified 12/3/93, array(1) declarations changed to array(*)
//*> \endverbatim
//*>
//*  =====================================================================
func CSCAL(N *int, CA *complex64, CX *[]complex64, INCX *int) {
	var I int
	var NINCX int
	//*
	//*  -- Reference BLAS level1 routine (version 3.8.0) --
	//*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
	//*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	//*     November 2017
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
	if (*N) <= 0 || (*INCX) <= 0 {
		return
	}
	if (*INCX) == 1 {
		//*
		//*        code for increment equal to 1
		//*
		for I = 1; I <= (*N); I++ {
			(*CX)[I-(1)] = (*CA) * (*CX)[I-(1)]
		}
	} else {
		//*
		//*        code for increment not equal to 1
		//*
		NINCX = (*N) * (*INCX)
		for I = 1; I <= NINCX; I += (*INCX) {
			(*CX)[I-(1)] = (*CA) * (*CX)[I-(1)]
		}
	}
	return
}
