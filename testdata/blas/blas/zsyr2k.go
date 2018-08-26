package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b ZSYR2K
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE ZSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX*16 ALPHA,BETA
//*       INTEGER K,LDA,LDB,LDC,N
//*       CHARACTER TRANS,UPLO
//*       ..
//*       .. Array Arguments ..
//*       COMPLEX*16 A(LDA,*),B(LDB,*),C(LDC,*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> ZSYR2K  performs one of the symmetric rank 2k operations
//*>
//*>    C := alpha*A*B**T + alpha*B*A**T + beta*C,
//*>
//*> or
//*>
//*>    C := alpha*A**T*B + alpha*B**T*A + beta*C,
//*>
//*> where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
//*> and  A and B  are  n by k  matrices  in the  first  case  and  k by n
//*> matrices in the second case.
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
//*>              TRANS = 'N' or 'n'    C := alpha*A*B**T + alpha*B*A**T +
//*>                                         beta*C.
//*>
//*>              TRANS = 'T' or 't'    C := alpha*A**T*B + alpha*B**T*A +
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
//*>           TRANS = 'T' or 't',  K  specifies  the number of rows of the
//*>           matrices  A and B.  K must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] ALPHA
//*> \verbatim
//*>          ALPHA is COMPLEX*16
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] A
//*> \verbatim
//*>          A is COMPLEX*16 array, dimension ( LDA, ka ), where ka is
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
//*>          B is COMPLEX*16 array, dimension ( LDB, kb ), where kb is
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
//*>          BETA is COMPLEX*16
//*>           On entry, BETA specifies the scalar beta.
//*> \endverbatim
//*>
//*> \param[in,out] C
//*> \verbatim
//*>          C is COMPLEX*16 array, dimension ( LDC, N )
//*>           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
//*>           upper triangular part of the array C must contain the upper
//*>           triangular part  of the  symmetric matrix  and the strictly
//*>           lower triangular part of C is not referenced.  On exit, the
//*>           upper triangular part of the array  C is overwritten by the
//*>           upper triangular part of the updated matrix.
//*>           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
//*>           lower triangular part of the array C must contain the lower
//*>           triangular part  of the  symmetric matrix  and the strictly
//*>           upper triangular part of C is not referenced.  On exit, the
//*>           lower triangular part of the array  C is overwritten by the
//*>           lower triangular part of the updated matrix.
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
//*> \ingroup complex16_blas_level3
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
func ZSYR2K(UPLO *byte, TRANS *byte, N *int, K *int, ALPHA *complex128, A *[][]complex128, LDA *int, B *[][]complex128, LDB *int, BETA *complex128, C *[][]complex128, LDC *int) {
	var TEMP1 complex128
	var TEMP2 complex128
	var I int
	var INFO int
	var J int
	var L int
	var NROWA int
	var UPPER bool
	var ONE complex128 = (1.0e+0 + (0.0e+0)*1i)
	var ZERO complex128 = (0.0e+0 + (0.0e+0)*1i)
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
	if LSAME(TRANS, func()*[]byte{y:=[]byte("N");return &y}()) {
		NROWA = (*N)
	} else {
		NROWA = (*K)
	}
	UPPER = LSAME(UPLO, func()*[]byte{y:=[]byte("U");return &y}())
	//*
	INFO = 0
	if (!UPPER) && (!LSAME(UPLO, func()*[]byte{y:=[]byte("L");return &y}())) {
		INFO = 1
	} else if (!LSAME(TRANS, func()*[]byte{y:=[]byte("N");return &y}())) && (!LSAME(TRANS, func()*[]byte{y:=[]byte("T");return &y}())) {
		INFO = 2
	} else if (*N) < 0 {
		INFO = 3
	} else if (*K) < 0 {
		INFO = 4
	} else if (*LDA) < intrinsic.MAX(func()*int{y:=1;return &y}(), NROWA) {
		INFO = 7
	} else if (*LDB) < intrinsic.MAX(func()*int{y:=1;return &y}(), NROWA) {
		INFO = 9
	} else if (*LDC) < intrinsic.MAX(func()*int{y:=1;return &y}(), (*N)) {
		INFO = 12
	}
	if INFO != 0 {
		XERBLA(func()*[]byte{y:=[]byte("ZSYR2K");return &y}(), &(INFO))
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
			if (*BETA) == ZERO {
				for J = 1; J <= (*N); J++ {
					for I = 1; I <= J; I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				}
			} else {
				for J = 1; J <= (*N); J++ {
					for I = 1; I <= J; I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
				}
			}
		} else {
			if (*BETA) == ZERO {
				for J = 1; J <= (*N); J++ {
					for I = J; I <= (*N); I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				}
			} else {
				for J = 1; J <= (*N); J++ {
					for I = J; I <= (*N); I++ {
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
	if LSAME(TRANS, func()*[]byte{y:=[]byte("N");return &y}()) {
		//*
		//*        Form  C := alpha*A*B**T + alpha*B*A**T + C.
		//*
		if UPPER {
			for J = 1; J <= (*N); J++ {
				if (*BETA) == ZERO {
					for I = 1; I <= J; I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				} else if (*BETA) != ONE {
					for I = 1; I <= J; I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
				}
				for L = 1; L <= (*K); L++ {
					if ((*A)[J-(1)][L-(1)] != ZERO) || ((*B)[J-(1)][L-(1)] != ZERO) {
						TEMP1 = (*ALPHA) * (*B)[J-(1)][L-(1)]
						TEMP2 = (*ALPHA) * (*A)[J-(1)][L-(1)]
						for I = 1; I <= J; I++ {
							(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + (*A)[I-(1)][L-(1)]*TEMP1 + (*B)[I-(1)][L-(1)]*TEMP2
						}
					}
				}
			}
		} else {
			for J = 1; J <= (*N); J++ {
				if (*BETA) == ZERO {
					for I = J; I <= (*N); I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				} else if (*BETA) != ONE {
					for I = J; I <= (*N); I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
				}
				for L = 1; L <= (*K); L++ {
					if ((*A)[J-(1)][L-(1)] != ZERO) || ((*B)[J-(1)][L-(1)] != ZERO) {
						TEMP1 = (*ALPHA) * (*B)[J-(1)][L-(1)]
						TEMP2 = (*ALPHA) * (*A)[J-(1)][L-(1)]
						for I = J; I <= (*N); I++ {
							(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + (*A)[I-(1)][L-(1)]*TEMP1 + (*B)[I-(1)][L-(1)]*TEMP2
						}
					}
				}
			}
		}
	} else {
		//*
		//*        Form  C := alpha*A**T*B + alpha*B**T*A + C.
		//*
		if UPPER {
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= J; I++ {
					TEMP1 = ZERO
					TEMP2 = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP1 = TEMP1 + (*A)[L-(1)][I-(1)]*(*B)[L-(1)][J-(1)]
						TEMP2 = TEMP2 + (*B)[L-(1)][I-(1)]*(*A)[L-(1)][J-(1)]
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP1 + (*ALPHA)*TEMP2
					} else {
						(*C)[I-(1)][J-(1)] = (*BETA)*(*C)[I-(1)][J-(1)] + (*ALPHA)*TEMP1 + (*ALPHA)*TEMP2
					}
				}
			}
		} else {
			for J = 1; J <= (*N); J++ {
				for I = J; I <= (*N); I++ {
					TEMP1 = ZERO
					TEMP2 = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP1 = TEMP1 + (*A)[L-(1)][I-(1)]*(*B)[L-(1)][J-(1)]
						TEMP2 = TEMP2 + (*B)[L-(1)][I-(1)]*(*A)[L-(1)][J-(1)]
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP1 + (*ALPHA)*TEMP2
					} else {
						(*C)[I-(1)][J-(1)] = (*BETA)*(*C)[I-(1)][J-(1)] + (*ALPHA)*TEMP1 + (*ALPHA)*TEMP2
					}
				}
			}
		}
	}
	//*
	return
	//*
	//*     End of ZSYR2K.
	//*
}
