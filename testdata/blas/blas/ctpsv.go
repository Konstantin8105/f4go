package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b CTPSV
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CTPSV(UPLO,TRANS,DIAG,N,AP,X,INCX)
//*
//*       .. Scalar Arguments ..
//*       INTEGER INCX,N
//*       CHARACTER DIAG,TRANS,UPLO
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX AP(*),X(*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> CTPSV  solves one of the systems of equations
//*>
//*>    A*x = b,   or   A**T*x = b,   or   A**H*x = b,
//*>
//*> where b and x are n element vectors and A is an n by n unit, or
//*> non-unit, upper or lower triangular matrix, supplied in packed form.
//*>
//*> No test for singularity or near-singularity is included in this
//*> routine. Such tests must be performed before calling this routine.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] UPLO
//*> \verbatim
//*>          UPLO is CHARACTER*1
//*>           On entry, UPLO specifies whether the matrix is an upper or
//*>           lower triangular matrix as follows:
//*>
//*>              UPLO = 'U' or 'u'   A is an upper triangular matrix.
//*>
//*>              UPLO = 'L' or 'l'   A is a lower triangular matrix.
//*> \endverbatim
//*>
//*> \param[in] TRANS
//*> \verbatim
//*>          TRANS is CHARACTER*1
//*>           On entry, TRANS specifies the equations to be solved as
//*>           follows:
//*>
//*>              TRANS = 'N' or 'n'   A*x = b.
//*>
//*>              TRANS = 'T' or 't'   A**T*x = b.
//*>
//*>              TRANS = 'C' or 'c'   A**H*x = b.
//*> \endverbatim
//*>
//*> \param[in] DIAG
//*> \verbatim
//*>          DIAG is CHARACTER*1
//*>           On entry, DIAG specifies whether or not A is unit
//*>           triangular as follows:
//*>
//*>              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
//*>
//*>              DIAG = 'N' or 'n'   A is not assumed to be unit
//*>                                  triangular.
//*> \endverbatim
//*>
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry, N specifies the order of the matrix A.
//*>           N must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] AP
//*> \verbatim
//*>          AP is COMPLEX array, dimension at least
//*>           ( ( n*( n + 1 ) )/2 ).
//*>           Before entry with  UPLO = 'U' or 'u', the array AP must
//*>           contain the upper triangular matrix packed sequentially,
//*>           column by column, so that AP( 1 ) contains a( 1, 1 ),
//*>           AP( 2 ) and AP( 3 ) contain a( 1, 2 ) and a( 2, 2 )
//*>           respectively, and so on.
//*>           Before entry with UPLO = 'L' or 'l', the array AP must
//*>           contain the lower triangular matrix packed sequentially,
//*>           column by column, so that AP( 1 ) contains a( 1, 1 ),
//*>           AP( 2 ) and AP( 3 ) contain a( 2, 1 ) and a( 3, 1 )
//*>           respectively, and so on.
//*>           Note that when  DIAG = 'U' or 'u', the diagonal elements of
//*>           A are not referenced, but are assumed to be unity.
//*> \endverbatim
//*>
//*> \param[in,out] X
//*> \verbatim
//*>          X is COMPLEX array, dimension at least
//*>           ( 1 + ( n - 1 )*abs( INCX ) ).
//*>           Before entry, the incremented array X must contain the n
//*>           element right-hand side vector b. On exit, X is overwritten
//*>           with the solution vector x.
//*> \endverbatim
//*>
//*> \param[in] INCX
//*> \verbatim
//*>          INCX is INTEGER
//*>           On entry, INCX specifies the increment for the elements of
//*>           X. INCX must not be zero.
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
//*> \ingroup complex_blas_level2
//*
//*> \par Further Details:
//*  =====================
//*>
//*> \verbatim
//*>
//*>  Level 2 Blas routine.
//*>
//*>  -- Written on 22-October-1986.
//*>     Jack Dongarra, Argonne National Lab.
//*>     Jeremy Du Croz, Nag Central Office.
//*>     Sven Hammarling, Nag Central Office.
//*>     Richard Hanson, Sandia National Labs.
//*> \endverbatim
//*>
//*  =====================================================================
func CTPSV(UPLO *byte, TRANS *byte, DIAG *byte, N *int, AP *[]complex64, X *[]complex64, INCX *int) {
	var ZERO complex64 = (0.0e+0 + (0.0e+0)*1i)
	var TEMP complex64
	var I int
	var INFO int
	var IX int
	var J int
	var JX int
	var K int
	var KK int
	var KX int
	var NOCONJ bool
	var NOUNIT bool
	//*
	//*  -- Reference BLAS level2 routine (version 3.7.0) --
	//*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
	//*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
	//*     December 2016
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
	//*     .. External Functions ..
	//*     ..
	//*     .. External Subroutines ..
	//*     ..
	//*     .. Intrinsic Functions ..
	//*     ..
	//*
	//*     Test the input parameters.
	//*
	INFO = 0
	if !LSAME(UPLO, func()*byte{y:=byte('U');return &y}()) && !LSAME(UPLO, func()*byte{y:=byte('L');return &y}()) {
		INFO = 1
	} else if !LSAME(TRANS, func()*byte{y:=byte('N');return &y}()) && !LSAME(TRANS, func()*byte{y:=byte('T');return &y}()) && !LSAME(TRANS, func()*byte{y:=byte('C');return &y}()) {
		INFO = 2
	} else if !LSAME(DIAG, func()*byte{y:=byte('U');return &y}()) && !LSAME(DIAG, func()*byte{y:=byte('N');return &y}()) {
		INFO = 3
	} else if (*N) < 0 {
		INFO = 4
	} else if (*INCX) == 0 {
		INFO = 7
	}
	if INFO != 0 {
		XERBLA(func()*[]byte{y:=[]byte("CTPSV ");return &y}(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if (*N) == 0 {
		return
	}
	//*
	NOCONJ = LSAME(TRANS, func()*byte{y:=byte('T');return &y}())
	NOUNIT = LSAME(DIAG, func()*byte{y:=byte('N');return &y}())
	//*
	//*     Set up the start point in X if the increment is not unity. This
	//*     will be  ( N - 1 )*INCX  too small for descending loops.
	//*
	if (*INCX) <= 0 {
		KX = 1 - ((*N)-1)*(*INCX)
	} else if (*INCX) != 1 {
		KX = 1
	}
	//*
	//*     Start the operations. In this version the elements of AP are
	//*     accessed sequentially with one pass through AP.
	//*
	if LSAME(TRANS, func()*byte{y:=byte('N');return &y}()) {
		//*
		//*        Form  x := inv( A )*x.
		//*
		if LSAME(UPLO, func()*byte{y:=byte('U');return &y}()) {
			KK = ((*N) * ((*N) + 1)) / 2
			if (*INCX) == 1 {
				for J = (*N); J <= 1; J += -1 {
					if (*X)[J-(1)] != ZERO {
						if NOUNIT {
							(*X)[J-(1)] = (*X)[J-(1)] / (*AP)[KK-(1)]
						}
						TEMP = (*X)[J-(1)]
						K = KK - 1
						for I = J - 1; I <= 1; I += -1 {
							(*X)[I-(1)] = (*X)[I-(1)] - TEMP*(*AP)[K-(1)]
							K = K - 1
						}
					}
					KK = KK - J
				}
			} else {
				JX = KX + ((*N)-1)*(*INCX)
				for J = (*N); J <= 1; J += -1 {
					if (*X)[JX-(1)] != ZERO {
						if NOUNIT {
							(*X)[JX-(1)] = (*X)[JX-(1)] / (*AP)[KK-(1)]
						}
						TEMP = (*X)[JX-(1)]
						IX = JX
						for K = KK - 1; K <= KK-J+1; K += -1 {
							IX = IX - (*INCX)
							(*X)[IX-(1)] = (*X)[IX-(1)] - TEMP*(*AP)[K-(1)]
						}
					}
					JX = JX - (*INCX)
					KK = KK - J
				}
			}
		} else {
			KK = 1
			if (*INCX) == 1 {
				for J = 1; J <= (*N); J++ {
					if (*X)[J-(1)] != ZERO {
						if NOUNIT {
							(*X)[J-(1)] = (*X)[J-(1)] / (*AP)[KK-(1)]
						}
						TEMP = (*X)[J-(1)]
						K = KK + 1
						for I = J + 1; I <= (*N); I++ {
							(*X)[I-(1)] = (*X)[I-(1)] - TEMP*(*AP)[K-(1)]
							K = K + 1
						}
					}
					KK = KK + ((*N) - J + 1)
				}
			} else {
				JX = KX
				for J = 1; J <= (*N); J++ {
					if (*X)[JX-(1)] != ZERO {
						if NOUNIT {
							(*X)[JX-(1)] = (*X)[JX-(1)] / (*AP)[KK-(1)]
						}
						TEMP = (*X)[JX-(1)]
						IX = JX
						for K = KK + 1; K <= KK+(*N)-J; K++ {
							IX = IX + (*INCX)
							(*X)[IX-(1)] = (*X)[IX-(1)] - TEMP*(*AP)[K-(1)]
						}
					}
					JX = JX + (*INCX)
					KK = KK + ((*N) - J + 1)
				}
			}
		}
	} else {
		//*
		//*        Form  x := inv( A**T )*x  or  x := inv( A**H )*x.
		//*
		if LSAME(UPLO, func()*byte{y:=byte('U');return &y}()) {
			KK = 1
			if (*INCX) == 1 {
				for J = 1; J <= (*N); J++ {
					TEMP = (*X)[J-(1)]
					K = KK
					if NOCONJ {
						for I = 1; I <= J-1; I++ {
							TEMP = TEMP - (*AP)[K-(1)]*(*X)[I-(1)]
							K = K + 1
						}
						if NOUNIT {
							TEMP = TEMP / (*AP)[KK+J-1-(1)]
						}
					} else {
						for I = 1; I <= J-1; I++ {
							TEMP = TEMP - intrinsic.CONJG((*AP)[K-(1)])*(*X)[I-(1)]
							K = K + 1
						}
						if NOUNIT {
							TEMP = TEMP / intrinsic.CONJG((*AP)[KK+J-1-(1)])
						}
					}
					(*X)[J-(1)] = TEMP
					KK = KK + J
				}
			} else {
				JX = KX
				for J = 1; J <= (*N); J++ {
					TEMP = (*X)[JX-(1)]
					IX = KX
					if NOCONJ {
						for K = KK; K <= KK+J-2; K++ {
							TEMP = TEMP - (*AP)[K-(1)]*(*X)[IX-(1)]
							IX = IX + (*INCX)
						}
						if NOUNIT {
							TEMP = TEMP / (*AP)[KK+J-1-(1)]
						}
					} else {
						for K = KK; K <= KK+J-2; K++ {
							TEMP = TEMP - intrinsic.CONJG((*AP)[K-(1)])*(*X)[IX-(1)]
							IX = IX + (*INCX)
						}
						if NOUNIT {
							TEMP = TEMP / intrinsic.CONJG((*AP)[KK+J-1-(1)])
						}
					}
					(*X)[JX-(1)] = TEMP
					JX = JX + (*INCX)
					KK = KK + J
				}
			}
		} else {
			KK = ((*N) * ((*N) + 1)) / 2
			if (*INCX) == 1 {
				for J = (*N); J <= 1; J += -1 {
					TEMP = (*X)[J-(1)]
					K = KK
					if NOCONJ {
						for I = (*N); I <= J+1; I += -1 {
							TEMP = TEMP - (*AP)[K-(1)]*(*X)[I-(1)]
							K = K - 1
						}
						if NOUNIT {
							TEMP = TEMP / (*AP)[KK-(*N)+J-(1)]
						}
					} else {
						for I = (*N); I <= J+1; I += -1 {
							TEMP = TEMP - intrinsic.CONJG((*AP)[K-(1)])*(*X)[I-(1)]
							K = K - 1
						}
						if NOUNIT {
							TEMP = TEMP / intrinsic.CONJG((*AP)[KK-(*N)+J-(1)])
						}
					}
					(*X)[J-(1)] = TEMP
					KK = KK - ((*N) - J + 1)
				}
			} else {
				KX = KX + ((*N)-1)*(*INCX)
				JX = KX
				for J = (*N); J <= 1; J += -1 {
					TEMP = (*X)[JX-(1)]
					IX = KX
					if NOCONJ {
						for K = KK; K <= KK-((*N)-(J+1)); K += -1 {
							TEMP = TEMP - (*AP)[K-(1)]*(*X)[IX-(1)]
							IX = IX - (*INCX)
						}
						if NOUNIT {
							TEMP = TEMP / (*AP)[KK-(*N)+J-(1)]
						}
					} else {
						for K = KK; K <= KK-((*N)-(J+1)); K += -1 {
							TEMP = TEMP - intrinsic.CONJG((*AP)[K-(1)])*(*X)[IX-(1)]
							IX = IX - (*INCX)
						}
						if NOUNIT {
							TEMP = TEMP / intrinsic.CONJG((*AP)[KK-(*N)+J-(1)])
						}
					}
					(*X)[JX-(1)] = TEMP
					JX = JX - (*INCX)
					KK = KK - ((*N) - J + 1)
				}
			}
		}
	}
	//*
	return
	//*
	//*     End of CTPSV .
	//*
}
