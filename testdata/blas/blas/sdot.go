package main
//*> \brief \b SDOT
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       REAL FUNCTION SDOT(N,SX,INCX,SY,INCY)
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
//*>    SDOT forms the dot product of two vectors.
//*>    uses unrolled loops for increments equal to one.
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
//*>
//*> \param[in] SY
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
func SDOT(N *int, SX *[]float64, INCX *int, SY *[]float64, INCY *int) (SDOT_RES float64) {
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
	STEMP = 0.0e0
	(SDOT_RES) = 0.0e0
	if (*N) <= 0 {
		return
	}
	if (*INCX) == 1 && (*INCY) == 1 {
		//*
		//*        code for both increments equal to 1
		//*
		//*
		//*        clean-up loop
		//*
		M = MOD(N, func()*int{y:=5;return &y}())
		if M != 0 {
			for I = 1; I <= M; I++ {
				STEMP = STEMP + (*SX)[I-(1)]*(*SY)[I-(1)]
			}
			if (*N) < 5 {
				(SDOT_RES) = STEMP
				return
			}
		}
		MP1 = M + 1
		for I = MP1; I <= (*N); I += 5 {
			STEMP = STEMP + (*SX)[I-(1)]*(*SY)[I-(1)] + (*SX)[I+1-(1)]*(*SY)[I+1-(1)] + (*SX)[I+2-(1)]*(*SY)[I+2-(1)] + (*SX)[I+3-(1)]*(*SY)[I+3-(1)] + (*SX)[I+4-(1)]*(*SY)[I+4-(1)]
		}
	} else {
		//*
		//*        code for unequal increments or equal increments
		//*          not equal to 1
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
			STEMP = STEMP + (*SX)[IX-(1)]*(*SY)[IY-(1)]
			IX = IX + (*INCX)
			IY = IY + (*INCY)
		}
	}
	(SDOT_RES) = STEMP
	return
}
