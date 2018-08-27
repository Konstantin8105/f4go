package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b IDAMAX
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       INTEGER FUNCTION IDAMAX(N,DX,INCX)
//*
//*       .. Scalar Arguments ..
//*       INTEGER INCX,N
//*       ..
//*       .. Array Arguments ..
//*       DOUBLE PRECISION DX(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    IDAMAX finds the index of the first element having maximum absolute value.
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
//*> \param[in] DX
//*> \verbatim
//*>          DX is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>         storage spacing between elements of SX
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
//*> \ingroup aux_blas
//*
//*> \par Further Details:
//*  =====================
//*>
//*> \verbatim
//*>
//*>     jack dongarra, linpack, 3/11/78.
//*>     modified 3/93 to return if incx .le. 0.
//*>     modified 12/3/93, array(1) declarations changed to array(*)
//*> \endverbatim
//*>
//*  =====================================================================
func IDAMAX(N *int, DX *[]float64, INCX *int) (IDAMAX_RES int) {
	var DMAX float64
	var I int
	var IX int
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
	//*     .. Intrinsic Functions ..
	//*     ..
	(IDAMAX_RES) = 0
	if (*N) < 1 || (*INCX) <= 0 {
		return
	}
	(IDAMAX_RES) = 1
	if (*N) == 1 {
		return
	}
	if (*INCX) == 1 {
		//*
		//*        code for increment equal to 1
		//*
		DMAX = intrinsic.ABS((*DX)[1-(1)])
		for I = 2; I <= (*N); I++ {
			if intrinsic.ABS((*DX)[I-(1)]) > DMAX {
				(IDAMAX_RES) = I
				DMAX = intrinsic.ABS((*DX)[I-(1)])
			}
		}
	} else {
		//*
		//*        code for increment not equal to 1
		//*
		IX = 1
		DMAX = intrinsic.ABS((*DX)[1-(1)])
		IX = IX + (*INCX)
		for I = 2; I <= (*N); I++ {
			if intrinsic.ABS((*DX)[IX-(1)]) > DMAX {
				(IDAMAX_RES) = I
				DMAX = intrinsic.ABS((*DX)[IX-(1)])
			}
			IX = IX + (*INCX)
		}
	}
	return
}
