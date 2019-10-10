package main

import "github.com/Konstantin8105/f4go/intrinsic"
import "math"

//*> \brief \b CROTG
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CROTG(CA,CB,C,S)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX CA,CB,S
//*       REAL C
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> CROTG determines a complex Givens rotation.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] CA
//*> \verbatim
//*>          CA is COMPLEX
//*> \endverbatim
//*>
//*> \param[in] CB
//*> \verbatim
//*>          CB is COMPLEX
//*> \endverbatim
//*>
//*> \param[out] C
//*> \verbatim
//*>          C is REAL
//*> \endverbatim
//*>
//*> \param[out] S
//*> \verbatim
//*>          S is COMPLEX
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
//*  =====================================================================
func CROTG(CA *complex64, CB *complex64, C *float64, S *complex64) {
	var ALPHA complex64
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
	if intrinsic.CABS((*CA)) == 0. {
		(*C) = 0.
		(*S) = (1. + (0.)*1i)
		(*CA) = (*CB)
	} else {
		SCALE = intrinsic.CABS((*CA)) + intrinsic.CABS((*CB))
		NORM = SCALE * SQRT(math.Pow((intrinsic.CABS((*CA)/SCALE)), 2)+math.Pow((intrinsic.CABS((*CB)/SCALE)), 2))
		ALPHA = (*CA) / intrinsic.CABS((*CA))
		(*C) = intrinsic.CABS((*CA)) / NORM
		(*S) = ALPHA * intrinsic.CONJG((*CB)) / NORM
		(*CA) = ALPHA * complex(NORM, 0)
	}
	return
}
