package main

//*> \brief \b ZDSCAL
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE ZDSCAL(N,DA,ZX,INCX)
//*
//*       .. Scalar Arguments ..
//*       DOUBLE PRECISION DA
//*       INTEGER INCX,N
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX*16 ZX(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*>    ZDSCAL scales a vector by a constant.
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
//*> \param[in,out] ZX
//*> \verbatim
//*>          ZX is COMPLEX*16 array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>         storage spacing between elements of ZX
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
//*> \par Further Details:
//*  =====================
//*>
//*> \verbatim
//*>
//*>     jack dongarra, 3/11/78.
//*>     modified 3/93 to return if incx .le. 0.
//*>     modified 12/3/93, array(1) declarations changed to array(*)
//*> \endverbatim
//*>
//*  =====================================================================
func ZDSCAL(N *int, DA *float64, ZX *[]complex128, INCX *int) {
	var I int
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
		for I = 1; I <= (*N); I++ {
			(*ZX)[I-(1)] = DCMPLX(DA, func() *float64 { y := 0.0e0; return &y }()) * (*ZX)[I-(1)]
		}
	} else {
		//*
		//*        code for increment not equal to 1
		//*
		NINCX = (*N) * (*INCX)
		for I = 1; I <= NINCX; I += (*INCX) {
			(*ZX)[I-(1)] = DCMPLX(DA, func() *float64 { y := 0.0e0; return &y }()) * (*ZX)[I-(1)]
		}
	}
	return
}
