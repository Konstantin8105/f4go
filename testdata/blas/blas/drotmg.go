package main

import "math"
import "github.com/Konstantin8105/f4go/intrinsic"

//*> \brief \b DROTMG
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE DROTMG(DD1,DD2,DX1,DY1,DPARAM)
//*
//*       .. Scalar Arguments ..
//*       DOUBLE PRECISION DD1,DD2,DX1,DY1
//*       ..
//*       .. Array Arguments ..
//*       DOUBLE PRECISION DPARAM(5)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
//*>    THE SECOND COMPONENT OF THE 2-VECTOR  (DSQRT(DD1)*DX1,DSQRT(DD2)*>    DY2)**T.
//*>    WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
//*>
//*>    DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
//*>
//*>      (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
//*>    H=(          )    (          )    (          )    (          )
//*>      (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
//*>    LOCATIONS 2-4 OF DPARAM CONTAIN DH11, DH21, DH12, AND DH22
//*>    RESPECTIVELY. (VALUES OF 1.D0, -1.D0, OR 0.D0 IMPLIED BY THE
//*>    VALUE OF DPARAM(1) ARE NOT STORED IN DPARAM.)
//*>
//*>    THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
//*>    INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
//*>    OF DD1 AND DD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
//*>
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in,out] DD1
//*> \verbatim
//*>          DD1 is DOUBLE PRECISION
//*> \endverbatim
//*>
//*> \param[in,out] DD2
//*> \verbatim
//*>          DD2 is DOUBLE PRECISION
//*> \endverbatim
//*>
//*> \param[in,out] DX1
//*> \verbatim
//*>          DX1 is DOUBLE PRECISION
//*> \endverbatim
//*>
//*> \param[in] DY1
//*> \verbatim
//*>          DY1 is DOUBLE PRECISION
//*> \endverbatim
//*>
//*> \param[out] DPARAM
//*> \verbatim
//*>          DPARAM is DOUBLE PRECISION array, dimension (5)
//*>     DPARAM(1)=DFLAG
//*>     DPARAM(2)=DH11
//*>     DPARAM(3)=DH21
//*>     DPARAM(4)=DH12
//*>     DPARAM(5)=DH22
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
//*> \ingroup double_blas_level1
//*
//*  =====================================================================
func DROTMG(DD1 *float64, DD2 *float64, DX1 *float64, DY1 *float64, DPARAM *[5]float64) {
	var DFLAG float64
	var DH11 float64
	var DH12 float64
	var DH21 float64
	var DH22 float64
	var DP1 float64
	var DP2 float64
	var DQ1 float64
	var DQ2 float64
	var DTEMP float64
	var DU float64
	var GAM float64
	var GAMSQ float64
	var ONE float64
	var RGAMSQ float64
	var TWO float64
	var ZERO float64
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
	//*     .. Data statements ..
	//*
	ZERO = 0.e0
	ONE = 1.e0
	TWO = 2.e0
	GAM = 4096.e0
	GAMSQ = 16777216.e0
	RGAMSQ = 5.9604645e-8
	//*     ..
	if (*DD1) < ZERO {
		//*        GO ZERO-H-D-AND-DX1..
		DFLAG = -ONE
		DH11 = ZERO
		DH12 = ZERO
		DH21 = ZERO
		DH22 = ZERO
		//*
		(*DD1) = ZERO
		(*DD2) = ZERO
		(*DX1) = ZERO
	} else {
		//*        CASE-DD1-NONNEGATIVE
		DP2 = (*DD2) * (*DY1)
		if DP2 == ZERO {
			DFLAG = -TWO
			(*DPARAM)[1-(1)] = DFLAG
			return
		}
		//*        REGULAR-CASE..
		DP1 = (*DD1) * (*DX1)
		DQ2 = DP2 * (*DY1)
		DQ1 = DP1 * (*DX1)
		//*
		if intrinsic.ABS(DQ1) > intrinsic.ABS(DQ2) {
			DH21 = -(*DY1) / (*DX1)
			DH12 = DP2 / DP1
			//*
			DU = ONE - DH12*DH21
			//*
			if DU > ZERO {
				DFLAG = ZERO
				(*DD1) = (*DD1) / DU
				(*DD2) = (*DD2) / DU
				(*DX1) = (*DX1) * DU
			}
		} else {
			if DQ2 < ZERO {
				//*              GO ZERO-H-D-AND-DX1..
				DFLAG = -ONE
				DH11 = ZERO
				DH12 = ZERO
				DH21 = ZERO
				DH22 = ZERO
				//*
				(*DD1) = ZERO
				(*DD2) = ZERO
				(*DX1) = ZERO
			} else {
				DFLAG = ONE
				DH11 = DP1 / DP2
				DH22 = (*DX1) / (*DY1)
				DU = ONE + DH11*DH22
				DTEMP = (*DD2) / DU
				(*DD2) = (*DD1) / DU
				(*DD1) = DTEMP
				(*DX1) = (*DY1) * DU
			}
		}
		//*     PROCEDURE..SCALE-CHECK
		if (*DD1) != ZERO {
			for ((*DD1) <= RGAMSQ) || ((*DD1) >= GAMSQ) {
				if DFLAG == ZERO {
					DH11 = ONE
					DH22 = ONE
					DFLAG = -ONE
				} else {
					DH21 = -ONE
					DH12 = ONE
					DFLAG = -ONE
				}
				if (*DD1) <= RGAMSQ {
					(*DD1) = (*DD1) * math.Pow(GAM, 2)
					(*DX1) = (*DX1) / GAM
					DH11 = DH11 / GAM
					DH12 = DH12 / GAM
				} else {
					(*DD1) = (*DD1) / math.Pow(GAM, 2)
					(*DX1) = (*DX1) * GAM
					DH11 = DH11 * GAM
					DH12 = DH12 * GAM
				}
			}
		}
		if (*DD2) != ZERO {
			for (intrinsic.ABS((*DD2)) <= RGAMSQ) || (intrinsic.ABS((*DD2)) >= GAMSQ) {
				if DFLAG == ZERO {
					DH11 = ONE
					DH22 = ONE
					DFLAG = -ONE
				} else {
					DH21 = -ONE
					DH12 = ONE
					DFLAG = -ONE
				}
				if intrinsic.ABS((*DD2)) <= RGAMSQ {
					(*DD2) = (*DD2) * math.Pow(GAM, 2)
					DH21 = DH21 / GAM
					DH22 = DH22 / GAM
				} else {
					(*DD2) = (*DD2) / math.Pow(GAM, 2)
					DH21 = DH21 * GAM
					DH22 = DH22 * GAM
				}
			}
		}
	}
	if DFLAG < ZERO {
		(*DPARAM)[2-(1)] = DH11
		(*DPARAM)[3-(1)] = DH21
		(*DPARAM)[4-(1)] = DH12
		(*DPARAM)[5-(1)] = DH22
	} else if DFLAG == ZERO {
		(*DPARAM)[3-(1)] = DH21
		(*DPARAM)[4-(1)] = DH12
	} else {
		(*DPARAM)[2-(1)] = DH11
		(*DPARAM)[5-(1)] = DH22
	}
	(*DPARAM)[1-(1)] = DFLAG
	return
}
