package main
//*> \brief \b ISAMAX
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       INTEGER FUNCTION ISAMAX(N,SX,INCX)
//*
//*       .. Scalar Arguments ..
//*       INTEGER INCX,N
//*       ..
//*       .. Array Arguments ..
//*       REAL SX(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    ISAMAX finds the index of the first element having maximum absolute value.
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
//*> \param[in] SX
//*> \verbatim
//*>          SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
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
func ISAMAX(N *int, SX *[]float64, INCX *int) (ISAMAX_RES int) {
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
	//*     .. Intrinsic Functions ..
	//*     ..
	(ISAMAX_RES) = 0
	if (*N) < 1 || (*INCX) <= 0 {
		return
	}
	(ISAMAX_RES) = 1
	if (*N) == 1 {
		return
	}
	if (*INCX) == 1 {
		//*
		//*        code for increment equal to 1
		//*
		SMAX = ABS(&((*SX)[1-(1)]))
		for I = 2; I <= (*N); I++ {
			if ABS(&((*SX)[I-(1)])) > SMAX {
				(ISAMAX_RES) = I
				SMAX = ABS(&((*SX)[I-(1)]))
			}
		}
	} else {
		//*
		//*        code for increment not equal to 1
		//*
		IX = 1
		SMAX = ABS(&((*SX)[1-(1)]))
		IX = IX + (*INCX)
		for I = 2; I <= (*N); I++ {
			if ABS(&((*SX)[IX-(1)])) > SMAX {
				(ISAMAX_RES) = I
				SMAX = ABS(&((*SX)[IX-(1)]))
			}
			IX = IX + (*INCX)
		}
	}
	return
}
