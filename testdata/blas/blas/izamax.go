package main
//*> \brief \b IZAMAX
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       INTEGER FUNCTION IZAMAX(N,ZX,INCX)
//*
//*       .. Scalar Arguments ..
//*       INTEGER INCX,N
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX*16 ZX(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    IZAMAX finds the index of the first element having maximum |Re(.)| + |Im(.)|
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
//*> \param[in] ZX
//*> \verbatim
//*>          ZX is COMPLEX*16 array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
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
//*>     jack dongarra, 1/15/85.
//*>     modified 3/93 to return if incx .le. 0.
//*>     modified 12/3/93, array(1) declarations changed to array(*)
//*> \endverbatim
//*>
//*  =====================================================================
func IZAMAX(N *int, ZX *[]complex128, INCX *int) (IZAMAX_RES int) {
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
	//*     .. External Functions ..
	//*     ..
	(IZAMAX_RES) = 0
	if (*N) < 1 || (*INCX) <= 0 {
		return
	}
	(IZAMAX_RES) = 1
	if (*N) == 1 {
		return
	}
	if (*INCX) == 1 {
		//*
		//*        code for increment equal to 1
		//*
		DMAX = DCABS1(&((*ZX)[1-(1)]))
		for I = 2; I <= (*N); I++ {
			if DCABS1(&((*ZX)[I-(1)])) > DMAX {
				(IZAMAX_RES) = I
				DMAX = DCABS1(&((*ZX)[I-(1)]))
			}
		}
	} else {
		//*
		//*        code for increment not equal to 1
		//*
		IX = 1
		DMAX = DCABS1(&((*ZX)[1-(1)]))
		IX = IX + (*INCX)
		for I = 2; I <= (*N); I++ {
			if DCABS1(&((*ZX)[IX-(1)])) > DMAX {
				(IZAMAX_RES) = I
				DMAX = DCABS1(&((*ZX)[IX-(1)]))
			}
			IX = IX + (*INCX)
		}
	}
	return
}
