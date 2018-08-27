package main

import "github.com/Konstantin8105/f4go/intrinsic"
//*> \brief \b CGEMM
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE CGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
//*
//*       .. Scalar Arguments ..
//*       COMPLEX ALPHA,BETA
//*       INTEGER K,LDA,LDB,LDC,M,N
//*       CHARACTER TRANSA,TRANSB
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
//*> CGEMM  performs one of the matrix-matrix operations
//*>
//*>    C := alpha*op( A )*op( B ) + beta*C,
//*>
//*> where  op( X ) is one of
//*>
//*>    op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,
//*>
//*> alpha and beta are scalars, and A, B and C are matrices, with op( A )
//*> an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] TRANSA
//*> \verbatim
//*>          TRANSA is CHARACTER*1
//*>           On entry, TRANSA specifies the form of op( A ) to be used in
//*>           the matrix multiplication as follows:
//*>
//*>              TRANSA = 'N' or 'n',  op( A ) = A.
//*>
//*>              TRANSA = 'T' or 't',  op( A ) = A**T.
//*>
//*>              TRANSA = 'C' or 'c',  op( A ) = A**H.
//*> \endverbatim
//*>
//*> \param[in] TRANSB
//*> \verbatim
//*>          TRANSB is CHARACTER*1
//*>           On entry, TRANSB specifies the form of op( B ) to be used in
//*>           the matrix multiplication as follows:
//*>
//*>              TRANSB = 'N' or 'n',  op( B ) = B.
//*>
//*>              TRANSB = 'T' or 't',  op( B ) = B**T.
//*>
//*>              TRANSB = 'C' or 'c',  op( B ) = B**H.
//*> \endverbatim
//*>
//*> \param[in] M
//*> \verbatim
//*>          M is INTEGER
//*>           On entry,  M  specifies  the number  of rows  of the  matrix
//*>           op( A )  and of the  matrix  C.  M  must  be at least  zero.
//*> \endverbatim
//*>
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry,  N  specifies the number  of columns of the matrix
//*>           op( B ) and the number of columns of the matrix C. N must be
//*>           at least zero.
//*> \endverbatim
//*>
//*> \param[in] K
//*> \verbatim
//*>          K is INTEGER
//*>           On entry,  K  specifies  the number of columns of the matrix
//*>           op( A ) and the number of rows of the matrix op( B ). K must
//*>           be at least  zero.
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
//*>           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
//*>           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
//*>           part of the array  A  must contain the matrix  A,  otherwise
//*>           the leading  k by m  part of the array  A  must contain  the
//*>           matrix A.
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
//*>           LDA must be at least  max( 1, m ), otherwise  LDA must be at
//*>           least  max( 1, k ).
//*> \endverbatim
//*>
//*> \param[in] B
//*> \verbatim
//*>          B is COMPLEX array, dimension ( LDB, kb ), where kb is
//*>           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
//*>           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
//*>           part of the array  B  must contain the matrix  B,  otherwise
//*>           the leading  n by k  part of the array  B  must contain  the
//*>           matrix B.
//*> \endverbatim
//*>
//*> \param[in] LDB
//*> \verbatim
//*>          LDB is INTEGER
//*>           On entry, LDB specifies the first dimension of B as declared
//*>           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
//*>           LDB must be at least  max( 1, k ), otherwise  LDB must be at
//*>           least  max( 1, n ).
//*> \endverbatim
//*>
//*> \param[in] BETA
//*> \verbatim
//*>          BETA is COMPLEX
//*>           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
//*>           supplied as zero then C need not be set on input.
//*> \endverbatim
//*>
//*> \param[in,out] C
//*> \verbatim
//*>          C is COMPLEX array, dimension ( LDC, N )
//*>           Before entry, the leading  m by n  part of the array  C must
//*>           contain the matrix  C,  except when  beta  is zero, in which
//*>           case C need not be set on entry.
//*>           On exit, the array  C  is overwritten by the  m by n  matrix
//*>           ( alpha*op( A )*op( B ) + beta*C ).
//*> \endverbatim
//*>
//*> \param[in] LDC
//*> \verbatim
//*>          LDC is INTEGER
//*>           On entry, LDC specifies the first dimension of C as declared
//*>           in  the  calling  (sub)  program.   LDC  must  be  at  least
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
func CGEMM(TRANSA *byte, TRANSB *byte, M *int, N *int, K *int, ALPHA *complex64, A *[][]complex64, LDA *int, B *[][]complex64, LDB *int, BETA *complex64, C *[][]complex64, LDC *int) {
	var TEMP complex64
	var I int
	var INFO int
	var J int
	var L int
	var NCOLA int
	var NROWA int
	var NROWB int
	var CONJA bool
	var CONJB bool
	var NOTA bool
	var NOTB bool
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
	//*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
	//*     conjugated or transposed, set  CONJA and CONJB  as true if  A  and
	//*     B  respectively are to be  transposed but  not conjugated  and set
	//*     NROWA, NCOLA and  NROWB  as the number of rows and  columns  of  A
	//*     and the number of rows of  B  respectively.
	//*
	NOTA = LSAME(TRANSA, func()*byte{y:=byte('N');return &y}())
	NOTB = LSAME(TRANSB, func()*byte{y:=byte('N');return &y}())
	CONJA = LSAME(TRANSA, func()*byte{y:=byte('C');return &y}())
	CONJB = LSAME(TRANSB, func()*byte{y:=byte('C');return &y}())
	if NOTA {
		NROWA = (*M)
		NCOLA = (*K)
	} else {
		NROWA = (*K)
		NCOLA = (*M)
	}
	if NOTB {
		NROWB = (*K)
	} else {
		NROWB = (*N)
	}
	//*
	//*     Test the input parameters.
	//*
	INFO = 0
	if (!NOTA) && (!CONJA) && (!LSAME(TRANSA, func()*byte{y:=byte('T');return &y}())) {
		INFO = 1
	} else if (!NOTB) && (!CONJB) && (!LSAME(TRANSB, func()*byte{y:=byte('T');return &y}())) {
		INFO = 2
	} else if (*M) < 0 {
		INFO = 3
	} else if (*N) < 0 {
		INFO = 4
	} else if (*K) < 0 {
		INFO = 5
	} else if (*LDA) < intrinsic.MAX(int(1), NROWA) {
		INFO = 8
	} else if (*LDB) < intrinsic.MAX(int(1), NROWB) {
		INFO = 10
	} else if (*LDC) < intrinsic.MAX(int(1), (*M)) {
		INFO = 13
	}
	if INFO != 0 {
		XERBLA(func()*[]byte{y:=[]byte("CGEMM ");return &y}(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if ((*M) == 0) || ((*N) == 0) || ((((*ALPHA) == ZERO) || ((*K) == 0)) && ((*BETA) == ONE)) {
		return
	}
	//*
	//*     And when  alpha.eq.zero.
	//*
	if (*ALPHA) == ZERO {
		if (*BETA) == ZERO {
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					(*C)[I-(1)][J-(1)] = ZERO
				}
			}
		} else {
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
				}
			}
		}
		return
	}
	//*
	//*     Start the operations.
	//*
	if NOTB {
		if NOTA {
			//*
			//*           Form  C := alpha*A*B + beta*C.
			//*
			for J = 1; J <= (*N); J++ {
				if (*BETA) == ZERO {
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				} else if (*BETA) != ONE {
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
				}
				for L = 1; L <= (*K); L++ {
					TEMP = (*ALPHA) * (*B)[L-(1)][J-(1)]
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + TEMP*(*A)[I-(1)][L-(1)]
					}
				}
			}
		} else if CONJA {
			//*
			//*           Form  C := alpha*A**H*B + beta*C.
			//*
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					TEMP = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP = TEMP + intrinsic.CONJG((*A)[L-(1)][I-(1)])*(*B)[L-(1)][J-(1)]
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = (*ALPHA) * TEMP
					} else {
						(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP + (*BETA)*(*C)[I-(1)][J-(1)]
					}
				}
			}
		} else {
			//*
			//*           Form  C := alpha*A**T*B + beta*C
			//*
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					TEMP = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP = TEMP + (*A)[L-(1)][I-(1)]*(*B)[L-(1)][J-(1)]
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = (*ALPHA) * TEMP
					} else {
						(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP + (*BETA)*(*C)[I-(1)][J-(1)]
					}
				}
			}
		}
	} else if NOTA {
		if CONJB {
			//*
			//*           Form  C := alpha*A*B**H + beta*C.
			//*
			for J = 1; J <= (*N); J++ {
				if (*BETA) == ZERO {
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				} else if (*BETA) != ONE {
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
				}
				for L = 1; L <= (*K); L++ {
					TEMP = (*ALPHA) * intrinsic.CONJG((*B)[J-(1)][L-(1)])
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + TEMP*(*A)[I-(1)][L-(1)]
					}
				}
			}
		} else {
			//*
			//*           Form  C := alpha*A*B**T + beta*C
			//*
			for J = 1; J <= (*N); J++ {
				if (*BETA) == ZERO {
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = ZERO
					}
				} else if (*BETA) != ONE {
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = (*BETA) * (*C)[I-(1)][J-(1)]
					}
				}
				for L = 1; L <= (*K); L++ {
					TEMP = (*ALPHA) * (*B)[J-(1)][L-(1)]
					for I = 1; I <= (*M); I++ {
						(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + TEMP*(*A)[I-(1)][L-(1)]
					}
				}
			}
		}
	} else if CONJA {
		if CONJB {
			//*
			//*           Form  C := alpha*A**H*B**H + beta*C.
			//*
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					TEMP = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP = TEMP + intrinsic.CONJG((*A)[L-(1)][I-(1)])*intrinsic.CONJG((*B)[J-(1)][L-(1)])
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = (*ALPHA) * TEMP
					} else {
						(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP + (*BETA)*(*C)[I-(1)][J-(1)]
					}
				}
			}
		} else {
			//*
			//*           Form  C := alpha*A**H*B**T + beta*C
			//*
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					TEMP = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP = TEMP + intrinsic.CONJG((*A)[L-(1)][I-(1)])*(*B)[J-(1)][L-(1)]
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = (*ALPHA) * TEMP
					} else {
						(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP + (*BETA)*(*C)[I-(1)][J-(1)]
					}
				}
			}
		}
	} else {
		if CONJB {
			//*
			//*           Form  C := alpha*A**T*B**H + beta*C
			//*
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					TEMP = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP = TEMP + (*A)[L-(1)][I-(1)]*intrinsic.CONJG((*B)[J-(1)][L-(1)])
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = (*ALPHA) * TEMP
					} else {
						(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP + (*BETA)*(*C)[I-(1)][J-(1)]
					}
				}
			}
		} else {
			//*
			//*           Form  C := alpha*A**T*B**T + beta*C
			//*
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					TEMP = ZERO
					for L = 1; L <= (*K); L++ {
						TEMP = TEMP + (*A)[L-(1)][I-(1)]*(*B)[J-(1)][L-(1)]
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = (*ALPHA) * TEMP
					} else {
						(*C)[I-(1)][J-(1)] = (*ALPHA)*TEMP + (*BETA)*(*C)[I-(1)][J-(1)]
					}
				}
			}
		}
	}
	//*
	return
	//*
	//*     End of CGEMM .
	//*
}
