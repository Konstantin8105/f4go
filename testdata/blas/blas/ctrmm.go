package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b CTRMM
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX ALPHA
//*       INTEGER LDA,LDB,M,N
//*       CHARACTER DIAG,SIDE,TRANSA,UPLO
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX A(LDA,*),B(LDB,*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> CTRMM  performs one of the matrix-matrix operations
//*>
//*>    B := alpha*op( A )*B,   or   B := alpha*B*op( A )
//*>
//*> where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
//*> non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
//*>
//*>    op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] SIDE
//*> \verbatim
//*>          SIDE is CHARACTER*1
//*>           On entry,  SIDE specifies whether  op( A ) multiplies B from
//*>           the left or right as follows:
//*>
//*>              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
//*>
//*>              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
//*> \endverbatim
//*>
//*> \param[in] UPLO
//*> \verbatim
//*>          UPLO is CHARACTER*1
//*>           On entry, UPLO specifies whether the matrix A is an upper or
//*>           lower triangular matrix as follows:
//*>
//*>              UPLO = 'U' or 'u'   A is an upper triangular matrix.
//*>
//*>              UPLO = 'L' or 'l'   A is a lower triangular matrix.
//*> \endverbatim
//*>
//*> \param[in] TRANSA
//*> \verbatim
//*>          TRANSA is CHARACTER*1
//*>           On entry, TRANSA specifies the form of op( A ) to be used in
//*>           the matrix multiplication as follows:
//*>
//*>              TRANSA = 'N' or 'n'   op( A ) = A.
//*>
//*>              TRANSA = 'T' or 't'   op( A ) = A**T.
//*>
//*>              TRANSA = 'C' or 'c'   op( A ) = A**H.
//*> \endverbatim
//*>
//*> \param[in] DIAG
//*> \verbatim
//*>          DIAG is CHARACTER*1
//*>           On entry, DIAG specifies whether or not A is unit triangular
//*>           as follows:
//*>
//*>              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
//*>
//*>              DIAG = 'N' or 'n'   A is not assumed to be unit
//*>                                  triangular.
//*> \endverbatim
//*>
//*> \param[in] M
//*> \verbatim
//*>          M is INTEGER
//*>           On entry, M specifies the number of rows of B. M must be at
//*>           least zero.
//*> \endverbatim
//*>
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry, N specifies the number of columns of B.  N must be
//*>           at least zero.
//*> \endverbatim
//*>
//*> \param[in] ALPHA
//*> \verbatim
//*>          ALPHA is COMPLEX
//*>           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
//*>           zero then  A is not referenced and  B need not be set before
//*>           entry.
//*> \endverbatim
//*>
//*> \param[in] A
//*> \verbatim
//*>          A is COMPLEX array, dimension ( LDA, k ), where k is m
//*>           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
//*>           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
//*>           upper triangular part of the array  A must contain the upper
//*>           triangular matrix  and the strictly lower triangular part of
//*>           A is not referenced.
//*>           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
//*>           lower triangular part of the array  A must contain the lower
//*>           triangular matrix  and the strictly upper triangular part of
//*>           A is not referenced.
//*>           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
//*>           A  are not referenced either,  but are assumed to be  unity.
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
//*>           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
//*>           then LDA must be at least max( 1, n ).
//*> \endverbatim
//*>
//*> \param[in,out] B
//*> \verbatim
//*>          B is COMPLEX array, dimension ( LDB, N ).
//*>           Before entry,  the leading  m by n part of the array  B must
//*>           contain the matrix  B,  and  on exit  is overwritten  by the
//*>           transformed matrix.
//*> \endverbatim
//*>
//*> \param[in] LDB
//*> \verbatim
//*>          LDB is INTEGER
//*>           On entry, LDB specifies the first dimension of B as declared
//*>           in  the  calling  (sub)  program.   LDB  must  be  at  least
//*>           max( 1, m ).
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
//*> \ingroup complex_blas_level3
//*
//*> \par Further Details:
//*  =====================
//*>
//*> \verbatim
//*>
//*>  Level 3 Blas routine.
//*>
//*>  -- Written on 8-February-1989.
//*>     Jack Dongarra, Argonne National Laboratory.
//*>     Iain Duff, AERE Harwell.
//*>     Jeremy Du Croz, Numerical Algorithms Group Ltd.
//*>     Sven Hammarling, Numerical Algorithms Group Ltd.
//*> \endverbatim
//*>
//*  =====================================================================
func CTRMM(SIDE *byte, UPLO *byte, TRANSA *byte, DIAG *byte, M *int, N *int, ALPHA *complex64, A *[][]complex64, LDA *int, B *[][]complex64, LDB *int) {
	var TEMP complex64
	var I int
	var INFO int
	var J int
	var K int
	var NROWA int
	var LSIDE bool
	var NOCONJ bool
	var NOUNIT bool
	var UPPER bool
	var ONE complex64 = (1.0e+0 + (0.0e+0)*1i)
	var ZERO complex64 = (0.0e+0 + (0.0e+0)*1i)
	//*
	//*  -- Reference BLAS level3 routine (version 3.7.0) --
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
	//*     .. External Functions ..
	//*     ..
	//*     .. External Subroutines ..
	//*     ..
	//*     .. Intrinsic Functions ..
	//*     ..
	//*     .. Local Scalars ..
	//*     ..
	//*     .. Parameters ..
	//*     ..
	//*
	//*     Test the input parameters.
	//*
	LSIDE = LSAME(SIDE, func()*[]byte{y:=[]byte("L");return &y}())
	if LSIDE {
		NROWA = (*M)
	} else {
		NROWA = (*N)
	}
	NOCONJ = LSAME(TRANSA, func()*[]byte{y:=[]byte("T");return &y}())
	NOUNIT = LSAME(DIAG, func()*[]byte{y:=[]byte("N");return &y}())
	UPPER = LSAME(UPLO, func()*[]byte{y:=[]byte("U");return &y}())
	//*
	INFO = 0
	if (!LSIDE) && (!LSAME(SIDE, func()*[]byte{y:=[]byte("R");return &y}())) {
		INFO = 1
	} else if (!UPPER) && (!LSAME(UPLO, func()*[]byte{y:=[]byte("L");return &y}())) {
		INFO = 2
	} else if (!LSAME(TRANSA, func()*[]byte{y:=[]byte("N");return &y}())) && (!LSAME(TRANSA, func()*[]byte{y:=[]byte("T");return &y}())) && (!LSAME(TRANSA, func()*[]byte{y:=[]byte("C");return &y}())) {
		INFO = 3
	} else if (!LSAME(DIAG, func()*[]byte{y:=[]byte("U");return &y}())) && (!LSAME(DIAG, func()*[]byte{y:=[]byte("N");return &y}())) {
		INFO = 4
	} else if (*M) < 0 {
		INFO = 5
	} else if (*N) < 0 {
		INFO = 6
	} else if (*LDA) < intrinsic.MAX(func()*int{y:=1;return &y}(), NROWA) {
		INFO = 9
	} else if (*LDB) < intrinsic.MAX(func()*int{y:=1;return &y}(), (*M)) {
		INFO = 11
	}
	if INFO != 0 {
		XERBLA(func()*[]byte{y:=[]byte("CTRMM ");return &y}(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if (*M) == 0 || (*N) == 0 {
		return
	}
	//*
	//*     And when  alpha.eq.zero.
	//*
	if (*ALPHA) == ZERO {
		for J = 1; J <= (*N); J++ {
			for I = 1; I <= (*M); I++ {
				(*B)[I-(1)][J-(1)] = ZERO
			}
		}
		return
	}
	//*
	//*     Start the operations.
	//*
	if LSIDE {
		if LSAME(TRANSA, func()*[]byte{y:=[]byte("N");return &y}()) {
			//*
			//*           Form  B := alpha*A*B.
			//*
			if UPPER {
				for J = 1; J <= (*N); J++ {
					for K = 1; K <= (*M); K++ {
						if (*B)[K-(1)][J-(1)] != ZERO {
							TEMP = (*ALPHA) * (*B)[K-(1)][J-(1)]
							for I = 1; I <= K-1; I++ {
								(*B)[I-(1)][J-(1)] = (*B)[I-(1)][J-(1)] + TEMP*(*A)[I-(1)][K-(1)]
							}
							if NOUNIT {
								TEMP = TEMP * (*A)[K-(1)][K-(1)]
							}
							(*B)[K-(1)][J-(1)] = TEMP
						}
					}
				}
			} else {
				for J = 1; J <= (*N); J++ {
					for K = (*M); K <= 1; K += -1 {
						if (*B)[K-(1)][J-(1)] != ZERO {
							TEMP = (*ALPHA) * (*B)[K-(1)][J-(1)]
							(*B)[K-(1)][J-(1)] = TEMP
							if NOUNIT {
								(*B)[K-(1)][J-(1)] = (*B)[K-(1)][J-(1)] * (*A)[K-(1)][K-(1)]
							}
							for I = K + 1; I <= (*M); I++ {
								(*B)[I-(1)][J-(1)] = (*B)[I-(1)][J-(1)] + TEMP*(*A)[I-(1)][K-(1)]
							}
						}
					}
				}
			}
		} else {
			//*
			//*           Form  B := alpha*A**T*B   or   B := alpha*A**H*B.
			//*
			if UPPER {
				for J = 1; J <= (*N); J++ {
					for I = (*M); I <= 1; I += -1 {
						TEMP = (*B)[I-(1)][J-(1)]
						if NOCONJ {
							if NOUNIT {
								TEMP = TEMP * (*A)[I-(1)][I-(1)]
							}
							for K = 1; K <= I-1; K++ {
								TEMP = TEMP + (*A)[K-(1)][I-(1)]*(*B)[K-(1)][J-(1)]
							}
						} else {
							if NOUNIT {
								TEMP = TEMP * CONJG(&((*A)[I-(1)][I-(1)]))
							}
							for K = 1; K <= I-1; K++ {
								TEMP = TEMP + CONJG(&((*A)[K-(1)][I-(1)]))*(*B)[K-(1)][J-(1)]
							}
						}
						(*B)[I-(1)][J-(1)] = (*ALPHA) * TEMP
					}
				}
			} else {
				for J = 1; J <= (*N); J++ {
					for I = 1; I <= (*M); I++ {
						TEMP = (*B)[I-(1)][J-(1)]
						if NOCONJ {
							if NOUNIT {
								TEMP = TEMP * (*A)[I-(1)][I-(1)]
							}
							for K = I + 1; K <= (*M); K++ {
								TEMP = TEMP + (*A)[K-(1)][I-(1)]*(*B)[K-(1)][J-(1)]
							}
						} else {
							if NOUNIT {
								TEMP = TEMP * CONJG(&((*A)[I-(1)][I-(1)]))
							}
							for K = I + 1; K <= (*M); K++ {
								TEMP = TEMP + CONJG(&((*A)[K-(1)][I-(1)]))*(*B)[K-(1)][J-(1)]
							}
						}
						(*B)[I-(1)][J-(1)] = (*ALPHA) * TEMP
					}
				}
			}
		}
	} else {
		if LSAME(TRANSA, func()*[]byte{y:=[]byte("N");return &y}()) {
			//*
			//*           Form  B := alpha*B*A.
			//*
			if UPPER {
				for J = (*N); J <= 1; J += -1 {
					TEMP = (*ALPHA)
					if NOUNIT {
						TEMP = TEMP * (*A)[J-(1)][J-(1)]
					}
					for I = 1; I <= (*M); I++ {
						(*B)[I-(1)][J-(1)] = TEMP * (*B)[I-(1)][J-(1)]
					}
					for K = 1; K <= J-1; K++ {
						if (*A)[K-(1)][J-(1)] != ZERO {
							TEMP = (*ALPHA) * (*A)[K-(1)][J-(1)]
							for I = 1; I <= (*M); I++ {
								(*B)[I-(1)][J-(1)] = (*B)[I-(1)][J-(1)] + TEMP*(*B)[I-(1)][K-(1)]
							}
						}
					}
				}
			} else {
				for J = 1; J <= (*N); J++ {
					TEMP = (*ALPHA)
					if NOUNIT {
						TEMP = TEMP * (*A)[J-(1)][J-(1)]
					}
					for I = 1; I <= (*M); I++ {
						(*B)[I-(1)][J-(1)] = TEMP * (*B)[I-(1)][J-(1)]
					}
					for K = J + 1; K <= (*N); K++ {
						if (*A)[K-(1)][J-(1)] != ZERO {
							TEMP = (*ALPHA) * (*A)[K-(1)][J-(1)]
							for I = 1; I <= (*M); I++ {
								(*B)[I-(1)][J-(1)] = (*B)[I-(1)][J-(1)] + TEMP*(*B)[I-(1)][K-(1)]
							}
						}
					}
				}
			}
		} else {
			//*
			//*           Form  B := alpha*B*A**T   or   B := alpha*B*A**H.
			//*
			if UPPER {
				for K = 1; K <= (*N); K++ {
					for J = 1; J <= K-1; J++ {
						if (*A)[J-(1)][K-(1)] != ZERO {
							if NOCONJ {
								TEMP = (*ALPHA) * (*A)[J-(1)][K-(1)]
							} else {
								TEMP = (*ALPHA) * CONJG(&((*A)[J-(1)][K-(1)]))
							}
							for I = 1; I <= (*M); I++ {
								(*B)[I-(1)][J-(1)] = (*B)[I-(1)][J-(1)] + TEMP*(*B)[I-(1)][K-(1)]
							}
						}
					}
					TEMP = (*ALPHA)
					if NOUNIT {
						if NOCONJ {
							TEMP = TEMP * (*A)[K-(1)][K-(1)]
						} else {
							TEMP = TEMP * CONJG(&((*A)[K-(1)][K-(1)]))
						}
					}
					if TEMP != ONE {
						for I = 1; I <= (*M); I++ {
							(*B)[I-(1)][K-(1)] = TEMP * (*B)[I-(1)][K-(1)]
						}
					}
				}
			} else {
				for K = (*N); K <= 1; K += -1 {
					for J = K + 1; J <= (*N); J++ {
						if (*A)[J-(1)][K-(1)] != ZERO {
							if NOCONJ {
								TEMP = (*ALPHA) * (*A)[J-(1)][K-(1)]
							} else {
								TEMP = (*ALPHA) * CONJG(&((*A)[J-(1)][K-(1)]))
							}
							for I = 1; I <= (*M); I++ {
								(*B)[I-(1)][J-(1)] = (*B)[I-(1)][J-(1)] + TEMP*(*B)[I-(1)][K-(1)]
							}
						}
					}
					TEMP = (*ALPHA)
					if NOUNIT {
						if NOCONJ {
							TEMP = TEMP * (*A)[K-(1)][K-(1)]
						} else {
							TEMP = TEMP * CONJG(&((*A)[K-(1)][K-(1)]))
						}
					}
					if TEMP != ONE {
						for I = 1; I <= (*M); I++ {
							(*B)[I-(1)][K-(1)] = TEMP * (*B)[I-(1)][K-(1)]
						}
					}
				}
			}
		}
	}
	//*
	return
	//*
	//*     End of CTRMM .
	//*
}
