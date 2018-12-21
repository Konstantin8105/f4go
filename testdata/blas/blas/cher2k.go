package main

import "github.com/Konstantin8105/f4go/intrinsic"

//*> \brief \b CHER2K
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX ALPHA
//*       REAL BETA
//*       INTEGER K,LDA,LDB,LDC,N
//*       CHARACTER TRANS,UPLO
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX A(LDA,*),B(LDB,*),C(LDC,*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> CHER2K  performs one of the hermitian rank 2k operations
//*>
//*>    C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
//*>
//*> or
//*>
//*>    C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
//*>
//*> where  alpha and beta  are scalars with  beta  real,  C is an  n by n
//*> hermitian matrix and  A and B  are  n by k matrices in the first case
//*> and  k by n  matrices in the second case.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] UPLO
//*> \verbatim
//*>          UPLO is CHARACTER*1
//*>           On  entry,   UPLO  specifies  whether  the  upper  or  lower
//*>           triangular  part  of the  array  C  is to be  referenced  as
//*>           follows:
//*>
//*>              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
//*>                                  is to be referenced.
//*>
//*>              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
//*>                                  is to be referenced.
//*> \endverbatim
//*>
//*> \param[in] TRANS
//*> \verbatim
//*>          TRANS is CHARACTER*1
//*>           On entry,  TRANS  specifies the operation to be performed as
//*>           follows:
//*>
//*>              TRANS = 'N' or 'n'    C := alpha*A*B**H          +
//*>                                         conjg( alpha )*B*A**H +
//*>                                         beta*C.
//*>
//*>              TRANS = 'C' or 'c'    C := alpha*A**H*B          +
//*>                                         conjg( alpha )*B**H*A +
//*>                                         beta*C.
//*> \endverbatim
//*>
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry,  N specifies the order of the matrix C.  N must be
//*>           at least zero.
//*> \endverbatim
//*>
//*> \param[in] K
//*> \verbatim
//*>          K is INTEGER
//*>           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
//*>           of  columns  of the  matrices  A and B,  and on  entry  with
//*>           TRANS = 'C' or 'c',  K  specifies  the number of rows of the
//*>           matrices  A and B.  K must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] ALPHA
//*> \verbatim
//*>          ALPHA is COMPLEX
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] A
//*> \verbatim
//*>          A is COMPLEX array, dimension ( LDA, ka ), where ka is
//*>           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
//*>           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
//*>           part of the array  A  must contain the matrix  A,  otherwise
//*>           the leading  k by n  part of the array  A  must contain  the
//*>           matrix A.
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
//*>           then  LDA must be at least  max( 1, n ), otherwise  LDA must
//*>           be at least  max( 1, k ).
//*> \endverbatim
//*>
//*> \param[in] B
//*> \verbatim
//*>          B is COMPLEX array, dimension ( LDB, kb ), where kb is
//*>           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
//*>           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
//*>           part of the array  B  must contain the matrix  B,  otherwise
//*>           the leading  k by n  part of the array  B  must contain  the
//*>           matrix B.
//*> \endverbatim
//*>
//*> \param[in] LDB
//*> \verbatim
//*>          LDB is INTEGER
//*>           On entry, LDB specifies the first dimension of B as declared
//*>           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
//*>           then  LDB must be at least  max( 1, n ), otherwise  LDB must
//*>           be at least  max( 1, k ).
//*> \endverbatim
//*>
//*> \param[in] BETA
//*> \verbatim
//*>          BETA is REAL
//*>           On entry, BETA specifies the scalar beta.
//*> \endverbatim
//*>
//*> \param[in,out] C
//*> \verbatim
//*>          C is COMPLEX array, dimension ( LDC, N )
//*>           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
//*>           upper triangular part of the array C must contain the upper
//*>           triangular part  of the  hermitian matrix  and the strictly
//*>           lower triangular part of C is not referenced.  On exit, the
//*>           upper triangular part of the array  C is overwritten by the
//*>           upper triangular part of the updated matrix.
//*>           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
//*>           lower triangular part of the array C must contain the lower
//*>           triangular part  of the  hermitian matrix  and the strictly
//*>           upper triangular part of C is not referenced.  On exit, the
//*>           lower triangular part of the array  C is overwritten by the
//*>           lower triangular part of the updated matrix.
//*>           Note that the imaginary parts of the diagonal elements need
//*>           not be set,  they are assumed to be zero,  and on exit they
//*>           are set to zero.
//*> \endverbatim
//*>
//*> \param[in] LDC
//*> \verbatim
//*>          LDC is INTEGER
//*>           On entry, LDC specifies the first dimension of C as declared
//*>           in  the  calling  (sub)  program.   LDC  must  be  at  least
//*>           max( 1, n ).
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
//*>
//*>  -- Modified 8-Nov-93 to set C(J,J) to REAL( C(J,J) ) when BETA = 1.
//*>     Ed Anderson, Cray Research Inc.
//*> \endverbatim
//*>
//*  =====================================================================
func CHER2K(UPLO *byte, TRANS *byte, N *int, K *int, ALPHA *complex64, A *[][]complex64, LDA *int, B *[][]complex64, LDB *int, BETA *float64, C *[][]complex64, LDC *int) {
	var TEMP1 complex64
	var TEMP2 complex64
	var I int
	var INFO int
	var J int
	var L int
	var NROWA int
	var UPPER bool
	var ONE float64 = 1.0e+0
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
	if LSAME(TRANS, func() *byte { y := byte('N'); return &y }()) {
		NROWA = (*N)
	} else {
		NROWA = (*K)
	}
	UPPER = LSAME(UPLO, func() *byte { y := byte('U'); return &y }())
	//*
	INFO = 0
	if (!UPPER) && (!LSAME(UPLO, func() *byte { y := byte('L'); return &y }())) {
		INFO = 1
	} else if (!LSAME(TRANS, func() *byte { y := byte('N'); return &y }())) && (!LSAME(TRANS, func() *byte { y := byte('C'); return &y }())) {
		INFO = 2
	} else if (*N) < 0 {
		INFO = 3
	} else if (*K) < 0 {
		INFO = 4
	} else if (*LDA) < intrinsic.MAX(int(1), NROWA) {
		INFO = 7
	} else if (*LDB) < intrinsic.MAX(int(1), NROWA) {
		INFO = 9
	} else if (*LDC) < intrinsic.MAX(int(1), (*N)) {
		INFO = 12
	}
	if INFO != 0 {
		XERBLA(func() *[]byte { y := []byte("CHER2K"); return &y }(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if ((*N) == 0) || ((((*ALPHA) == ZERO) || ((*K) == 0)) && ((*BETA) == ONE)) {
		return
	}
	//*
	//*     And when  alpha.eq.zero.
	//*
	if (*ALPHA) == ZERO {
		if UPPER {
			if (*BETA) == real(ZERO) {
				for J = 1; J <= (*N); J++ {
					for I = 1; I <= J; I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				}
			} else {
				for J = 1; J <= (*N); J++ {
					for I = 1; I <= J-1; I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
					(*C)[J-(1)][J-(1)] = (*BETA) * real((*C)[J-(1)][J-(1)])
				}
			}
		} else {
			if (*BETA) == real(ZERO) {
				for J = 1; J <= (*N); J++ {
					for I = J; I <= (*N); I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				}
			} else {
				for J = 1; J <= (*N); J++ {
					(*C)[J-(1)][J-(1)] = (*BETA) * real((*C)[J-(1)][J-(1)])
					for I = J + 1; I <= (*N); I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
				}
			}
		}
		return
	}
	//*
	//*     Start the operations.
	//*
	if LSAME(TRANS, func() *byte { y := byte('N'); return &y }()) {
		//*
		//*        Form  C := alpha*A*B**H + conjg( alpha )*B*A**H +
		//*                   C.
		//*
		if UPPER {
			for J = 1; J <= (*N); J++ {
				if (*BETA) == real(ZERO) {
					for I = 1; I <= J; I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				} else if (*BETA) != ONE {
					for I = 1; I <= J-1; I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
					(*C)[J-(1)][J-(1)] = (*BETA) * real((*C)[J-(1)][J-(1)])
				} else {
					(*C)[J-(1)][J-(1)] = real((*C)[J-(1)][J-(1)])
				}
				for L = 1; L <= (*K); L++ {
					if ((*A)[J-(1)][L-(1)] != ZERO) || ((*B)[J-(1)][L-(1)] != ZERO) {
						TEMP1 = (*ALPHA) * intrinsic.CONJG((*B)[J-(1)][L-(1)])
						TEMP2 = intrinsic.CONJG((*ALPHA) * (*A)[J-(1)][L-(1)])
						for I = 1; I <= J-1; I++ {
							(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + (*A)[I-(1)][L-(1)]*TEMP1 + (*B)[I-(1)][L-(1)]*TEMP2
						}
						(*C)[J-(1)][J-(1)] = real((*C)[J-(1)][J-(1)]) + real((*A)[J-(1)][L-(1)]*TEMP1+(*B)[J-(1)][L-(1)]*TEMP2)
					}
				}
			}
		} else {
			for J = 1; J <= (*N); J++ {
				if (*BETA) == real(ZERO) {
					for I = J; I <= (*N); I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				} else if (*BETA) != ONE {
					for I = J + 1; I <= (*N); I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
					(*C)[J-(1)][J-(1)] = (*BETA) * real((*C)[J-(1)][J-(1)])
				} else {
					(*C)[J-(1)][J-(1)] = real((*C)[J-(1)][J-(1)])
				}
				for L = 1; L <= (*K); L++ {
					if ((*A)[J-(1)][L-(1)] != ZERO) || ((*B)[J-(1)][L-(1)] != ZERO) {
						TEMP1 = (*ALPHA) * intrinsic.CONJG((*B)[J-(1)][L-(1)])
						TEMP2 = intrinsic.CONJG((*ALPHA) * (*A)[J-(1)][L-(1)])
						for I = J + 1; I <= (*N); I++ {
							(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + (*A)[I-(1)][L-(1)]*TEMP1 + (*B)[I-(1)][L-(1)]*TEMP2
						}
						(*C)[J-(1)][J-(1)] = real((*C)[J-(1)][J-(1)]) + real((*A)[J-(1)][L-(1)]*TEMP1+(*B)[J-(1)][L-(1)]*TEMP2)
					}
				}
			}
		}
	} else {
		//*
		//*        Form  C := alpha*A**H*B + conjg( alpha )*B**H*A +
		//*                   C.
		//*
		if UPPER {
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= J; I++ {
					TEMP1 = ZERO
					TEMP2 = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP1 = TEMP1 + intrinsic.CONJG((*A)[L-(1)][I-(1)])*(*B)[L-(1)][J-(1)]
						TEMP2 = TEMP2 + intrinsic.CONJG((*B)[L-(1)][I-(1)])*(*A)[L-(1)][J-(1)]
					}
					if I == J {
						if (*BETA) == real(ZERO) {
							(*C)[J-(1)][J-(1)] = real((*ALPHA)*TEMP1 + intrinsic.CONJG((*ALPHA))*TEMP2)
						} else {
							(*C)[J-(1)][J-(1)] = (*BETA)*real((*C)[J-(1)][J-(1)]) + real((*ALPHA)*TEMP1+intrinsic.CONJG((*ALPHA))*TEMP2)
						}
					} else {
						if (*BETA) == real(ZERO) {
							(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP1 + intrinsic.CONJG((*ALPHA))*TEMP2
						} else {
							(*C)[I-(1)][J-(1)] = (*BETA)*(*C)[I-(1)][J-(1)] + (*ALPHA)*TEMP1 + intrinsic.CONJG((*ALPHA))*TEMP2
						}
					}
				}
			}
		} else {
			for J = 1; J <= (*N); J++ {
				for I = J; I <= (*N); I++ {
					TEMP1 = ZERO
					TEMP2 = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP1 = TEMP1 + intrinsic.CONJG((*A)[L-(1)][I-(1)])*(*B)[L-(1)][J-(1)]
						TEMP2 = TEMP2 + intrinsic.CONJG((*B)[L-(1)][I-(1)])*(*A)[L-(1)][J-(1)]
					}
					if I == J {
						if (*BETA) == real(ZERO) {
							(*C)[J-(1)][J-(1)] = real((*ALPHA)*TEMP1 + intrinsic.CONJG((*ALPHA))*TEMP2)
						} else {
							(*C)[J-(1)][J-(1)] = (*BETA)*real((*C)[J-(1)][J-(1)]) + real((*ALPHA)*TEMP1+intrinsic.CONJG((*ALPHA))*TEMP2)
						}
					} else {
						if (*BETA) == real(ZERO) {
							(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP1 + intrinsic.CONJG((*ALPHA))*TEMP2
						} else {
							(*C)[I-(1)][J-(1)] = (*BETA)*(*C)[I-(1)][J-(1)] + (*ALPHA)*TEMP1 + intrinsic.CONJG((*ALPHA))*TEMP2
						}
					}
				}
			}
		}
	}
	//*
	return
	//*
	//*     End of CHER2K.
	//*
}
