package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b XERBLA_ARRAY
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE XERBLA_ARRAY(SRNAME_ARRAY, SRNAME_LEN, INFO)
//*
//*       .. Scalar Arguments ..
//*       INTEGER SRNAME_LEN, INFO
//*       ..
//*       .. Array Arguments ..
//*       CHARACTER(1) SRNAME_ARRAY(SRNAME_LEN)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> XERBLA_ARRAY assists other languages in calling XERBLA, the LAPACK
//*> and BLAS error handler.  Rather than taking a Fortran string argument
//*> as the function's name, XERBLA_ARRAY takes an array of single
//*> characters along with the array's length.  XERBLA_ARRAY then copies
//*> up to 32 characters of that array into a Fortran string and passes
//*> that to XERBLA.  If called with a non-positive SRNAME_LEN,
//*> XERBLA_ARRAY will call XERBLA with a string of all blank characters.
//*>
//*> Say some macro or other device makes XERBLA_ARRAY available to C99
//*> by a name lapack_xerbla and with a common Fortran calling convention.
//*> Then a C99 program could invoke XERBLA via:
//*>    {
//*>      int flen = strlen(__func__);
//*>      lapack_xerbla(__func__, &flen, &info);
//*>    }
//*>
//*> Providing XERBLA_ARRAY is not necessary for intercepting LAPACK
//*> errors.  XERBLA_ARRAY calls XERBLA.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] SRNAME_ARRAY
//*> \verbatim
//*>          SRNAME_ARRAY is CHARACTER(1) array, dimension (SRNAME_LEN)
//*>          The name of the routine which called XERBLA_ARRAY.
//*> \endverbatim
//*>
//*> \param[in] SRNAME_LEN
//*> \verbatim
//*>          SRNAME_LEN is INTEGER
//*>          The length of the name in SRNAME_ARRAY.
//*> \endverbatim
//*>
//*> \param[in] INFO
//*> \verbatim
//*>          INFO is INTEGER
//*>          The position of the invalid parameter in the parameter list
//*>          of the calling routine.
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
//*> \date December 2016
//*
//*> \ingroup aux_blas
//*
//*  =====================================================================
func XERBLA_ARRAY(SRNAME_ARRAY *[]byte, SRNAME_LEN *int, INFO *int) {
	var I int
	var SRNAME []byte = make([]byte, 32)
	//*
	//*  -- Reference BLAS level1 routine (version 3.7.0) --
	//*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
	//*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	//*     December 2016
	//*
	//*     .. Scalar Arguments ..
	//*     ..
	//*     .. Array Arguments ..
	//*     ..
	//*
	//* =====================================================================
	//*
	//*     ..
	//*     .. Local Scalars ..
	//*     ..
	//*     .. Local Arrays ..
	//*     ..
	//*     .. Intrinsic Functions ..
	//*     ..
	//*     .. External Functions ..
	//*     ..
	//*     .. Executable Statements ..
	SRNAME = *func()*[]byte{y:=[]byte("");return &y}()
	for I = 1; I <= intrinsic.MIN((*SRNAME_LEN), len(SRNAME)); I++ {
		SRNAME[I-(1)] = (*SRNAME_ARRAY)[I-(1)]
	}
	XERBLA(&(SRNAME), INFO)
	return
}
