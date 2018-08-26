package main

import "math"
import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b ZROTG
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE ZROTG(CA,CB,C,S)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX*16 CA,CB,S
//*       DOUBLE PRECISION C
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    ZROTG determines a double complex Givens rotation.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] CA
//*> \verbatim
//*>          CA is COMPLEX*16
//*> \endverbatim
//*>
//*> \param[in] CB
//*> \verbatim
//*>          CB is COMPLEX*16
//*> \endverbatim
//*>
//*> \param[out] C
//*> \verbatim
//*>          C is DOUBLE PRECISION
//*> \endverbatim
//*>
//*> \param[out] S
//*> \verbatim
//*>          S is COMPLEX*16
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
//*> \ingroup complex16_blas_level1
//*
//*  =====================================================================
func ZROTG(CA *complex128, CB *complex128, C *float64, S *complex128) {
	var ALPHA complex128
	var NORM float64
	var SCALE float64
	//*
	//*  -- Reference BLAS level1 routine (version 3.8.0) --
	//*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
	//*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	//*     November 2017
	//*
	//*     .. Scalar Arguments ..
	//*     ..
	//*
	//*  =====================================================================
	//*
	//*     .. Local Scalars ..
	//*     ..
	//*     .. Intrinsic Functions ..
	//*     ..
	if CDABS(CA) == 0.0e0 {
		(*C) = 0.0e0
		(*S) = (1.0e0 + (0.0e0)*1i)
		(*CA) = (*CB)
	} else {
		SCALE = CDABS(CA) + CDABS(CB)
		NORM = SCALE * DSQRT(math.Pow((CDABS((*CA)/DCMPLX(&(SCALE), func()*float64{y:=0.0e0;return &y}()))), 2)+math.Pow((CDABS((*CB)/DCMPLX(&(SCALE), func()*float64{y:=0.0e0;return &y}()))), 2))
		ALPHA = (*CA) / CDABS(CA)
		(*C) = CDABS(CA) / NORM
		(*S) = ALPHA * intrinsic.DCONJG((*CB)) / NORM
		(*CA) = ALPHA * NORM
	}
	return
}
