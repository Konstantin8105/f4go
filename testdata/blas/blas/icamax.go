package main
//*> \brief \b ICAMAX
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       INTEGER FUNCTION ICAMAX(N,CX,INCX)
//*
//*       .. Scalar Arguments ..
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
//*>    ICAMAX finds the index of the first element having maximum |Re(.)| + |Im(.)|
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
//*> \param[in] CX
//*> \verbatim
//*>          CX is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
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
func ICAMAX(N *int, CX *[]complex64, INCX *int) (ICAMAX_RES int) {
	var SMAX float64
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
	//*     .. External Functions ..
	//*     ..
	(ICAMAX_RES) = 0
	if (*N) < 1 || (*INCX) <= 0 {
		return
	}
	(ICAMAX_RES) = 1
	if (*N) == 1 {
		return
	}
	if (*INCX) == 1 {
		//*
		//*        code for increment equal to 1
		//*
		SMAX = SCABS1(&((*CX)[1-(1)]))
		for I = 2; I <= (*N); I++ {
			if SCABS1(&((*CX)[I-(1)])) > SMAX {
				(ICAMAX_RES) = I
				SMAX = SCABS1(&((*CX)[I-(1)]))
			}
		}
	} else {
		//*
		//*        code for increment not equal to 1
		//*
		IX = 1
		SMAX = SCABS1(&((*CX)[1-(1)]))
		IX = IX + (*INCX)
		for I = 2; I <= (*N); I++ {
			if SCABS1(&((*CX)[IX-(1)])) > SMAX {
				(ICAMAX_RES) = I
				SMAX = SCABS1(&((*CX)[IX-(1)]))
			}
			IX = IX + (*INCX)
		}
	}
	return
}
