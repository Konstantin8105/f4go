package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b SSWAP
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE SSWAP(N,SX,INCX,SY,INCY)
//*
//*       .. Scalar Arguments ..
//*       INTEGER INCX,INCY,N
//*       ..
//*       .. Array Arguments ..
//*       REAL SX(*),SY(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    SSWAP interchanges two vectors.
//*>    uses unrolled loops for increments equal to 1.
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
//*> \param[in,out] SX
//*> \verbatim
//*>          SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>         storage spacing between elements of SX
//*> \endverbatim
//*>
//*> \param[in,out] SY
//*> \verbatim
//*>          SY is REAL array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
//*> \endverbatim
//*>
//*> \param[in] INCY
//*> \verbatim
//*>          INCY is INTEGER
//*>         storage spacing between elements of SY
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
//*>     modified 12/3/93, array(1) declarations changed to array(*)
//*> \endverbatim
//*>
//*  =====================================================================
func SSWAP(N *int, SX *[]float64, INCX *int, SY *[]float64, INCY *int) {
	var STEMP float64
	var I int
	var IX int
	var IY int
	var M int
	var MP1 int
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
	if (*N) <= 0 {
		return
	}
	if (*INCX) == 1 && (*INCY) == 1 {
		//*
		//*       code for both increments equal to 1
		//*
		//*
		//*       clean-up loop
		//*
		M = intrinsic.MOD((*N), int(3))
		if M != 0 {
			for I = 1; I <= M; I++ {
				STEMP = (*SX)[I-(1)]
				(*SX)[I-(1)] = (*SY)[I-(1)]
				(*SY)[I-(1)] = STEMP
			}
			if (*N) < 3 {
				return
			}
		}
		MP1 = M + 1
		for I = MP1; I <= (*N); I += 3 {
			STEMP = (*SX)[I-(1)]
			(*SX)[I-(1)] = (*SY)[I-(1)]
			(*SY)[I-(1)] = STEMP
			STEMP = (*SX)[I+1-(1)]
			(*SX)[I+1-(1)] = (*SY)[I+1-(1)]
			(*SY)[I+1-(1)] = STEMP
			STEMP = (*SX)[I+2-(1)]
			(*SX)[I+2-(1)] = (*SY)[I+2-(1)]
			(*SY)[I+2-(1)] = STEMP
		}
	} else {
		//*
		//*       code for unequal increments or equal increments not equal
		//*         to 1
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
			STEMP = (*SX)[IX-(1)]
			(*SX)[IX-(1)] = (*SY)[IY-(1)]
			(*SY)[IY-(1)] = STEMP
			IX = IX + (*INCX)
			IY = IY + (*INCY)
		}
	}
	return
}
