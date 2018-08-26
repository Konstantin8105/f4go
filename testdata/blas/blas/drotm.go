package main
//*> \brief \b DROTM
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE DROTM(N,DX,INCX,DY,INCY,DPARAM)
//*
//*       .. Scalar Arguments ..
//*       INTEGER INCX,INCY,N
//*       ..
//*       .. Array Arguments ..
//*       DOUBLE PRECISION DPARAM(5),DX(*),DY(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
//*>
//*>    (DX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF DX ARE IN
//*>    (DY**T)
//*>
//*>    DX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
//*>    LX = (-INCX)*N, AND SIMILARLY FOR SY USING LY AND INCY.
//*>    WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
//*>
//*>    DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
//*>
//*>      (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
//*>    H=(          )    (          )    (          )    (          )
//*>      (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
//*>    SEE DROTMG FOR A DESCRIPTION OF DATA STORAGE IN DPARAM.
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
//*> \param[in,out] DX
//*> \verbatim
//*>          DX is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>         storage spacing between elements of DX
//*> \endverbatim
//*>
//*> \param[in,out] DY
//*> \verbatim
//*>          DY is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
//*> \endverbatim
//*>
//*> \param[in] INCY
//*> \verbatim
//*>          INCY is INTEGER
//*>         storage spacing between elements of DY
//*> \endverbatim
//*>
//*> \param[in] DPARAM
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
func DROTM(N *int, DX *[]float64, INCX *int, DY *[]float64, INCY *int, DPARAM *[5]float64) {
	var DFLAG float64
	var DH11 float64
	var DH12 float64
	var DH21 float64
	var DH22 float64
	var TWO float64
	var W float64
	var Z float64
	var ZERO float64
	var I int
	var KX int
	var KY int
	var NSTEPS int
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
	//*     .. Data statements ..
	ZERO = 0.e0
	TWO = 2.e0
	//*     ..
	//*
	DFLAG = (*DPARAM)[1-(1)]
	if (*N) <= 0 || (DFLAG+TWO == ZERO) {
		return
	}
	if (*INCX) == (*INCY) && (*INCX) > 0 {
		//*
		NSTEPS = (*N) * (*INCX)
		if DFLAG < ZERO {
			DH11 = (*DPARAM)[2-(1)]
			DH12 = (*DPARAM)[4-(1)]
			DH21 = (*DPARAM)[3-(1)]
			DH22 = (*DPARAM)[5-(1)]
			for I = 1; I <= NSTEPS; I += (*INCX) {
				W = (*DX)[I-(1)]
				Z = (*DY)[I-(1)]
				(*DX)[I-(1)] = W*DH11 + Z*DH12
				(*DY)[I-(1)] = W*DH21 + Z*DH22
			}
		} else if DFLAG == ZERO {
			DH12 = (*DPARAM)[4-(1)]
			DH21 = (*DPARAM)[3-(1)]
			for I = 1; I <= NSTEPS; I += (*INCX) {
				W = (*DX)[I-(1)]
				Z = (*DY)[I-(1)]
				(*DX)[I-(1)] = W + Z*DH12
				(*DY)[I-(1)] = W*DH21 + Z
			}
		} else {
			DH11 = (*DPARAM)[2-(1)]
			DH22 = (*DPARAM)[5-(1)]
			for I = 1; I <= NSTEPS; I += (*INCX) {
				W = (*DX)[I-(1)]
				Z = (*DY)[I-(1)]
				(*DX)[I-(1)] = W*DH11 + Z
				(*DY)[I-(1)] = -W + DH22*Z
			}
		}
	} else {
		KX = 1
		KY = 1
		if (*INCX) < 0 {
			KX = 1 + (1-(*N))*(*INCX)
		}
		if (*INCY) < 0 {
			KY = 1 + (1-(*N))*(*INCY)
		}
		//*
		if DFLAG < ZERO {
			DH11 = (*DPARAM)[2-(1)]
			DH12 = (*DPARAM)[4-(1)]
			DH21 = (*DPARAM)[3-(1)]
			DH22 = (*DPARAM)[5-(1)]
			for I = 1; I <= (*N); I++ {
				W = (*DX)[KX-(1)]
				Z = (*DY)[KY-(1)]
				(*DX)[KX-(1)] = W*DH11 + Z*DH12
				(*DY)[KY-(1)] = W*DH21 + Z*DH22
				KX = KX + (*INCX)
				KY = KY + (*INCY)
			}
		} else if DFLAG == ZERO {
			DH12 = (*DPARAM)[4-(1)]
			DH21 = (*DPARAM)[3-(1)]
			for I = 1; I <= (*N); I++ {
				W = (*DX)[KX-(1)]
				Z = (*DY)[KY-(1)]
				(*DX)[KX-(1)] = W + Z*DH12
				(*DY)[KY-(1)] = W*DH21 + Z
				KX = KX + (*INCX)
				KY = KY + (*INCY)
			}
		} else {
			DH11 = (*DPARAM)[2-(1)]
			DH22 = (*DPARAM)[5-(1)]
			for I = 1; I <= (*N); I++ {
				W = (*DX)[KX-(1)]
				Z = (*DY)[KY-(1)]
				(*DX)[KX-(1)] = W*DH11 + Z
				(*DY)[KY-(1)] = -W + DH22*Z
				KX = KX + (*INCX)
				KY = KY + (*INCY)
			}
		}
	}
	return
}
