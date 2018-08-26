package main

import "math"
//*> \brief \b SCNRM2
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       REAL FUNCTION SCNRM2(N,X,INCX)
//*
//*       .. Scalar Arguments ..
//*       INTEGER INCX,N
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX X(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> SCNRM2 returns the euclidean norm of a vector via the function
//*> name, so that
//*>
//*>    SCNRM2 := sqrt( x**H*x )
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
//*> \param[in] X
//*> \verbatim
//*>          X is COMPLEX array, dimension (N)
//*>         complex vector with N elements
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>         storage spacing between elements of X
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
//*>  -- This version written on 25-October-1982.
//*>     Modified on 14-October-1993 to inline the call to CLASSQ.
//*>     Sven Hammarling, Nag Ltd.
//*> \endverbatim
//*>
//*  =====================================================================
func SCNRM2(N *int, X *[]complex64, INCX *int) (SCNRM2_RES float64) {
	var ONE float64 = 1.0e+0
	var ZERO float64 = 0.0e+0
	var NORM float64
	var SCALE float64
	var SSQ float64
	var TEMP float64
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
	//*     .. Parameters ..
	//*     ..
	//*     .. Local Scalars ..
	//*     ..
	//*     .. Intrinsic Functions ..
	//*     ..
	if (*N) < 1 || (*INCX) < 1 {
		NORM = ZERO
	} else {
		SCALE = ZERO
		SSQ = ONE
		//*        The following loop is equivalent to this call to the LAPACK
		//*        auxiliary routine:
		//*        CALL CLASSQ( N, X, INCX, SCALE, SSQ )
		//*
		for IX = 1; IX <= 1+((*N)-1)*(*INCX); IX += (*INCX) {
			if real((*X)[IX-(1)]) != ZERO {
				TEMP = ABS(real((*X)[IX-(1)]))
				if SCALE < TEMP {
					SSQ = ONE + SSQ*math.Pow((SCALE/TEMP), 2)
					SCALE = TEMP
				} else {
					SSQ = SSQ + math.Pow((TEMP/SCALE), 2)
				}
			}
			if imag((*X)[IX-(1)]) != ZERO {
				TEMP = ABS(imag((*X)[IX-(1)]))
				if SCALE < TEMP {
					SSQ = ONE + SSQ*math.Pow((SCALE/TEMP), 2)
					SCALE = TEMP
				} else {
					SSQ = SSQ + math.Pow((TEMP/SCALE), 2)
				}
			}
		}
		NORM = SCALE * SQRT(&(SSQ))
	}
	//*
	(SCNRM2_RES) = NORM
	return
	//*
	//*     End of SCNRM2.
	//*
}
