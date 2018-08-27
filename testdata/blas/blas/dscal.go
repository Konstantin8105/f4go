package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b DSCAL
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE DSCAL(N,DA,DX,INCX)
//*
//*       .. Scalar Arguments ..
//*       DOUBLE PRECISION DA
//*       INTEGER INCX,N
//*       ..
//*       .. Array Arguments ..
//*       DOUBLE PRECISION DX(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    DSCAL scales a vector by a constant.
//*>    uses unrolled loops for increment equal to 1.
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
//*> \param[in] DA
//*> \verbatim
//*>          DA is DOUBLE PRECISION
//*>           On entry, DA specifies the scalar alpha.
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
func DSCAL(N *int, DA *float64, DX *[]float64, INCX *int) {
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
	if (*N) <= 0 || (*INCX) <= 0 {
		return
	}
	if (*INCX) == 1 {
		//*
		//*        code for increment equal to 1
		//*
		//*
		//*        clean-up loop
		//*
		M = intrinsic.MOD((*N), int(5))
		if M != 0 {
			for I = 1; I <= M; I++ {
				(*DX)[I-(1)] = (*DA) * (*DX)[I-(1)]
			}
			if (*N) < 5 {
				return
			}
		}
		MP1 = M + 1
		for I = MP1; I <= (*N); I += 5 {
			(*DX)[I-(1)] = (*DA) * (*DX)[I-(1)]
			(*DX)[I+1-(1)] = (*DA) * (*DX)[I+1-(1)]
			(*DX)[I+2-(1)] = (*DA) * (*DX)[I+2-(1)]
			(*DX)[I+3-(1)] = (*DA) * (*DX)[I+3-(1)]
			(*DX)[I+4-(1)] = (*DA) * (*DX)[I+4-(1)]
		}
	} else {
		//*
		//*        code for increment not equal to 1
		//*
		NINCX = (*N) * (*INCX)
		for I = 1; I <= NINCX; I += (*INCX) {
			(*DX)[I-(1)] = (*DA) * (*DX)[I-(1)]
		}
	}
	return
}
