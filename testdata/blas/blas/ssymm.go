package main
//*> \brief \b SSYMM
//*
//*  =========== DOCUMENTATION ===========
//*
//* Online html documentation available at
//*            http://www.netlib.org/lapack/explore-html/
//*
//*  Definition:
//*  ===========
//*
//*       SUBROUTINE SSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
//*
//*       .. Scalar Arguments ..
//*       REAL ALPHA,BETA
//*       INTEGER LDA,LDB,LDC,M,N
//*       CHARACTER SIDE,UPLO
//*       ..
//*       .. Array Arguments ..
//*       REAL A(LDA,*),B(LDB,*),C(LDC,*)
//*       ..
//*
//*
//*> \par Purpose:
//*  =============
//*>
//*> \verbatim
//*>
//*> SSYMM  performs one of the matrix-matrix operations
//*>
//*>    C := alpha*A*B + beta*C,
//*>
//*> or
//*>
//*>    C := alpha*B*A + beta*C,
//*>
//*> where alpha and beta are scalars,  A is a symmetric matrix and  B and
//*> C are  m by n matrices.
//*> \endverbatim
//*
//*  Arguments:
//*  ==========
//*
//*> \param[in] SIDE
//*> \verbatim
//*>          SIDE is CHARACTER*1
//*>           On entry,  SIDE  specifies whether  the  symmetric matrix  A
//*>           appears on the  left or right  in the  operation as follows:
//*>
//*>              SIDE = 'L' or 'l'   C := alpha*A*B + beta*C,
//*>
//*>              SIDE = 'R' or 'r'   C := alpha*B*A + beta*C,
//*> \endverbatim
//*>
//*> \param[in] UPLO
//*> \verbatim
//*>          UPLO is CHARACTER*1
//*>           On  entry,   UPLO  specifies  whether  the  upper  or  lower
//*>           triangular  part  of  the  symmetric  matrix   A  is  to  be
//*>           referenced as follows:
//*>
//*>              UPLO = 'U' or 'u'   Only the upper triangular part of the
//*>                                  symmetric matrix is to be referenced.
//*>
//*>              UPLO = 'L' or 'l'   Only the lower triangular part of the
//*>                                  symmetric matrix is to be referenced.
//*> \endverbatim
//*>
//*> \param[in] M
//*> \verbatim
//*>          M is INTEGER
//*>           On entry,  M  specifies the number of rows of the matrix  C.
//*>           M  must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] N
//*> \verbatim
//*>          N is INTEGER
//*>           On entry, N specifies the number of columns of the matrix C.
//*>           N  must be at least zero.
//*> \endverbatim
//*>
//*> \param[in] ALPHA
//*> \verbatim
//*>          ALPHA is REAL
//*>           On entry, ALPHA specifies the scalar alpha.
//*> \endverbatim
//*>
//*> \param[in] A
//*> \verbatim
//*>          A is REAL array, dimension ( LDA, ka ), where ka is
//*>           m  when  SIDE = 'L' or 'l'  and is  n otherwise.
//*>           Before entry  with  SIDE = 'L' or 'l',  the  m by m  part of
//*>           the array  A  must contain the  symmetric matrix,  such that
//*>           when  UPLO = 'U' or 'u', the leading m by m upper triangular
//*>           part of the array  A  must contain the upper triangular part
//*>           of the  symmetric matrix and the  strictly  lower triangular
//*>           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
//*>           the leading  m by m  lower triangular part  of the  array  A
//*>           must  contain  the  lower triangular part  of the  symmetric
//*>           matrix and the  strictly upper triangular part of  A  is not
//*>           referenced.
//*>           Before entry  with  SIDE = 'R' or 'r',  the  n by n  part of
//*>           the array  A  must contain the  symmetric matrix,  such that
//*>           when  UPLO = 'U' or 'u', the leading n by n upper triangular
//*>           part of the array  A  must contain the upper triangular part
//*>           of the  symmetric matrix and the  strictly  lower triangular
//*>           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
//*>           the leading  n by n  lower triangular part  of the  array  A
//*>           must  contain  the  lower triangular part  of the  symmetric
//*>           matrix and the  strictly upper triangular part of  A  is not
//*>           referenced.
//*> \endverbatim
//*>
//*> \param[in] LDA
//*> \verbatim
//*>          LDA is INTEGER
//*>           On entry, LDA specifies the first dimension of A as declared
//*>           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
//*>           LDA must be at least  max( 1, m ), otherwise  LDA must be at
//*>           least  max( 1, n ).
//*> \endverbatim
//*>
//*> \param[in] B
//*> \verbatim
//*>          B is REAL array, dimension ( LDB, N )
//*>           Before entry, the leading  m by n part of the array  B  must
//*>           contain the matrix B.
//*> \endverbatim
//*>
//*> \param[in] LDB
//*> \verbatim
//*>          LDB is INTEGER
//*>           On entry, LDB specifies the first dimension of B as declared
//*>           in  the  calling  (sub)  program.   LDB  must  be  at  least
//*>           max( 1, m ).
//*> \endverbatim
//*>
//*> \param[in] BETA
//*> \verbatim
//*>          BETA is REAL
//*>           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
//*>           supplied as zero then C need not be set on input.
//*> \endverbatim
//*>
//*> \param[in,out] C
//*> \verbatim
//*>          C is REAL array, dimension ( LDC, N )
//*>           Before entry, the leading  m by n  part of the array  C must
//*>           contain the matrix  C,  except when  beta  is zero, in which
//*>           case C need not be set on entry.
//*>           On exit, the array  C  is overwritten by the  m by n updated
//*>           matrix.
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
//*> \ingroup single_blas_level3
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
func SSYMM(SIDE *byte, UPLO *byte, M *int, N *int, ALPHA *float64, A *[][]float64, LDA *int, B *[][]float64, LDB *int, BETA *float64, C *[][]float64, LDC *int) {
	var TEMP1 float64
	var TEMP2 float64
	var I int
	var INFO int
	var J int
	var K int
	var NROWA int
	var UPPER bool
	var ONE float64 = 1.0e+0
	var ZERO float64 = 0.0e+0
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
	//*     Set NROWA as the number of rows of A.
	//*
	if LSAME(SIDE, func()*[]byte{y:=[]byte("L");return &y}()) {
		NROWA = (*M)
	} else {
		NROWA = (*N)
	}
	UPPER = LSAME(UPLO, func()*[]byte{y:=[]byte("U");return &y}())
	//*
	//*     Test the input parameters.
	//*
	INFO = 0
	if (!LSAME(SIDE, func()*[]byte{y:=[]byte("L");return &y}())) && (!LSAME(SIDE, func()*[]byte{y:=[]byte("R");return &y}())) {
		INFO = 1
	} else if (!UPPER) && (!LSAME(UPLO, func()*[]byte{y:=[]byte("L");return &y}())) {
		INFO = 2
	} else if (*M) < 0 {
		INFO = 3
	} else if (*N) < 0 {
		INFO = 4
	} else if (*LDA) < MAX(func()*int{y:=1;return &y}(), &(NROWA)) {
		INFO = 7
	} else if (*LDB) < MAX(func()*int{y:=1;return &y}(), M) {
		INFO = 9
	} else if (*LDC) < MAX(func()*int{y:=1;return &y}(), M) {
		INFO = 12
	}
	if INFO != 0 {
		XERBLA(func()*[]byte{y:=[]byte("SSYMM ");return &y}(), &(INFO))
		return
	}
	//*
	//*     Quick return if possible.
	//*
	if ((*M) == 0) || ((*N) == 0) || (((*ALPHA) == ZERO) && ((*BETA) == ONE)) {
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
	if LSAME(SIDE, func()*[]byte{y:=[]byte("L");return &y}()) {
		//*
		//*        Form  C := alpha*A*B + beta*C.
		//*
		if UPPER {
			for J = 1; J <= (*N); J++ {
				for I = 1; I <= (*M); I++ {
					TEMP1 = (*ALPHA) * (*B)[I-(1)][J-(1)]
					TEMP2 = ZERO
					for K = 1; K <= I-1; K++ {
						(*C)[K-(1)][J-(1)] = (*C)[K-(1)][J-(1)] + TEMP1*(*A)[K-(1)][I-(1)]
						TEMP2 = TEMP2 + (*B)[K-(1)][J-(1)]*(*A)[K-(1)][I-(1)]
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = TEMP1*(*A)[I-(1)][I-(1)] + (*ALPHA)*TEMP2
					} else {
						(*C)[I-(1)][J-(1)] = (*BETA)*(*C)[I-(1)][J-(1)] + TEMP1*(*A)[I-(1)][I-(1)] + (*ALPHA)*TEMP2
					}
				}
			}
		} else {
			for J = 1; J <= (*N); J++ {
				for I = (*M); I <= 1; I += -1 {
					TEMP1 = (*ALPHA) * (*B)[I-(1)][J-(1)]
					TEMP2 = ZERO
					for K = I + 1; K <= (*M); K++ {
						(*C)[K-(1)][J-(1)] = (*C)[K-(1)][J-(1)] + TEMP1*(*A)[K-(1)][I-(1)]
						TEMP2 = TEMP2 + (*B)[K-(1)][J-(1)]*(*A)[K-(1)][I-(1)]
					}
					if (*BETA) == ZERO {
						(*C)[I-(1)][J-(1)] = TEMP1*(*A)[I-(1)][I-(1)] + (*ALPHA)*TEMP2
					} else {
						(*C)[I-(1)][J-(1)] = (*BETA)*(*C)[I-(1)][J-(1)] + TEMP1*(*A)[I-(1)][I-(1)] + (*ALPHA)*TEMP2
					}
				}
			}
		}
	} else {
		//*
		//*        Form  C := alpha*B*A + beta*C.
		//*
		for J = 1; J <= (*N); J++ {
			TEMP1 = (*ALPHA) * (*A)[J-(1)][J-(1)]
			if (*BETA) == ZERO {
				for I = 1; I <= (*M); I++ {
					(*C)[I-(1)][J-(1)] = TEMP1 * (*B)[I-(1)][J-(1)]
				}
			} else {
				for I = 1; I <= (*M); I++ {
					(*C)[I-(1)][J-(1)] = (*BETA)*(*C)[I-(1)][J-(1)] + TEMP1*(*B)[I-(1)][J-(1)]
				}
			}
			for K = 1; K <= J-1; K++ {
				if UPPER {
					TEMP1 = (*ALPHA) * (*A)[K-(1)][J-(1)]
				} else {
					TEMP1 = (*ALPHA) * (*A)[J-(1)][K-(1)]
				}
				for I = 1; I <= (*M); I++ {
					(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + TEMP1*(*B)[I-(1)][K-(1)]
				}
			}
			for K = J + 1; K <= (*N); K++ {
				if UPPER {
					TEMP1 = (*ALPHA) * (*A)[J-(1)][K-(1)]
				} else {
					TEMP1 = (*ALPHA) * (*A)[K-(1)][J-(1)]
				}
				for I = 1; I <= (*M); I++ {
					(*C)[I-(1)][J-(1)] = (*C)[I-(1)][J-(1)] + TEMP1*(*B)[I-(1)][K-(1)]
				}
			}
		}
	}
	//*
	return
	//*
	//*     End of SSYMM .
	//*
}
