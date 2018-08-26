package main

import "math"
//*> \brief \b SROTG
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE SROTG(SA,SB,C,S)
//*
//*       .. Scalar Arguments ..
//*       REAL C,S,SA,SB
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    SROTG construct givens plane rotation.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] SA
//*> \verbatim
//*>          SA is REAL
//*> \endverbatim
//*>
//*> \param[in] SB
//*> \verbatim
//*>          SB is REAL
//*> \endverbatim
//*>
//*> \param[out] C
//*> \verbatim
//*>          C is REAL
//*> \endverbatim
//*>
//*> \param[out] S
//*> \verbatim
//*>          S is REAL
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
//*> \endverbatim
//*>
//*  =====================================================================
func SROTG(SA *float64, SB *float64, C *float64, S *float64) {
	var R float64
	var ROE float64
	var SCALE float64
	var Z float64
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
	ROE = (*SB)
	if ABS(SA) > ABS(SB) {
		ROE = (*SA)
	}
	SCALE = ABS(SA) + ABS(SB)
	if SCALE == 0.0 {
		(*C) = 1.0
		(*S) = 0.0
		R = 0.0
		Z = 0.0
	} else {
		R = SCALE * SQRT(math.Pow(((*SA)/SCALE), 2)+math.Pow(((*SB)/SCALE), 2))
		R = SIGN(func()*float64{y:=1.0;return &y}(), &(ROE)) * R
		(*C) = (*SA) / R
		(*S) = (*SB) / R
		Z = 1.0
		if ABS(SA) > ABS(SB) {
			Z = (*S)
		}
		if ABS(SB) >= ABS(SA) && (*C) != 0.0 {
			Z = 1.0 / (*C)
		}
	}
	(*SA) = R
	(*SB) = Z
	return
}
