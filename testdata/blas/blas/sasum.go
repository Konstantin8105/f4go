package main

import "github.com/Konstantin8105/f4go/intrinsic"

//*> \brief \b SASUM
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       REAL FUNCTION SASUM(N,SX,INCX)
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
//*>    SASUM takes the sum of the absolute values.
//*>    uses unrolled loops for increment equal to one.
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
//*> \ingroup single_blas_level1
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
func SASUM(N *int, SX *[]float64, INCX *int) (SASUM_RES float64) {
	var STEMP float64
	var I int
	var M int
	var MP1 int
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
	//*     .. Intrinsic Functions ..
	//*     ..
	(SASUM_RES) = 0.0e0
	STEMP = 0.0e0
	if (*N) <= 0 || (*INCX) <= 0 {
		return
	}
	if (*INCX) == 1 {
		//*        code for increment equal to 1
		//*
		//*
		//*        clean-up loop
		//*
		M = intrinsic.MOD((*N), int(6))
		if M != 0 {
			for I = 1; I <= M; I++ {
				STEMP = STEMP + intrinsic.ABS((*SX)[I-(1)])
			}
			if (*N) < 6 {
				(SASUM_RES) = STEMP
				return
			}
		}
		MP1 = M + 1
		for I = MP1; I <= (*N); I += 6 {
			STEMP = STEMP + intrinsic.ABS((*SX)[I-(1)]) + intrinsic.ABS((*SX)[I+1-(1)]) + intrinsic.ABS((*SX)[I+2-(1)]) + intrinsic.ABS((*SX)[I+3-(1)]) + intrinsic.ABS((*SX)[I+4-(1)]) + intrinsic.ABS((*SX)[I+5-(1)])
		}
	} else {
		//*
		//*        code for increment not equal to 1
		//*
		NINCX = (*N) * (*INCX)
		for I = 1; I <= NINCX; I += (*INCX) {
			STEMP = STEMP + intrinsic.ABS((*SX)[I-(1)])
		}
	}
	(SASUM_RES) = STEMP
	return
}
