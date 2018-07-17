(string) (len=15839) "COMMENT\t> \\brief \\b DSYMM\nCOMMENT\t*\nCOMMENT\t*  =========== DOCUMENTATION ===========\nCOMMENT\t*\nCOMMENT\t* Online html documentation available at\nCOMMENT\t*            http://www.netlib.org/lapack/explore-html/\nCOMMENT\t*\nCOMMENT\t*  Definition:\nCOMMENT\t*  ===========\nCOMMENT\t*\nCOMMENT\t*       SUBROUTINE DSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)\nCOMMENT\t*\nCOMMENT\t*       .. Scalar Arguments ..\nCOMMENT\t*       DOUBLE PRECISION ALPHA,BETA\nCOMMENT\t*       INTEGER LDA,LDB,LDC,M,N\nCOMMENT\t*       CHARACTER SIDE,UPLO\nCOMMENT\t*       ..\nCOMMENT\t*       .. Array Arguments ..\nCOMMENT\t*       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)\nCOMMENT\t*       ..\nCOMMENT\t*\nCOMMENT\t*\nCOMMENT\t*> \\par Purpose:\nCOMMENT\t*  =============\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*> DSYMM  performs one of the matrix-matrix operations\nCOMMENT\t*>\nCOMMENT\t*>    C := alpha*A*B + beta*C,\nCOMMENT\t*>\nCOMMENT\t*> or\nCOMMENT\t*>\nCOMMENT\t*>    C := alpha*B*A + beta*C,\nCOMMENT\t*>\nCOMMENT\t*> where alpha and beta are scalars,  A is a symmetric matrix and  B and\nCOMMENT\t*> C are  m by n matrices.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Arguments:\nCOMMENT\t*  ==========\nCOMMENT\t*\nCOMMENT\t*> \\param[in] SIDE\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          SIDE is CHARACTER*1\nCOMMENT\t*>           On entry,  SIDE  specifies whether  the  symmetric matrix  A\nCOMMENT\t*>           appears on the  left or right  in the  operation as follows:\nCOMMENT\t*>\nCOMMENT\t*>              SIDE = 'L' or 'l'   C := alpha*A*B + beta*C,\nCOMMENT\t*>\nCOMMENT\t*>              SIDE = 'R' or 'r'   C := alpha*B*A + beta*C,\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] UPLO\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          UPLO is CHARACTER*1\nCOMMENT\t*>           On  entry,   UPLO  specifies  whether  the  upper  or  lower\nCOMMENT\t*>           triangular  part  of  the  symmetric  matrix   A  is  to  be\nCOMMENT\t*>           referenced as follows:\nCOMMENT\t*>\nCOMMENT\t*>              UPLO = 'U' or 'u'   Only the upper triangular part of the\nCOMMENT\t*>                                  symmetric matrix is to be referenced.\nCOMMENT\t*>\nCOMMENT\t*>              UPLO = 'L' or 'l'   Only the lower triangular part of the\nCOMMENT\t*>                                  symmetric matrix is to be referenced.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] M\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          M is INTEGER\nCOMMENT\t*>           On entry,  M  specifies the number of rows of the matrix  C.\nCOMMENT\t*>           M  must be at least zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] N\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          N is INTEGER\nCOMMENT\t*>           On entry, N specifies the number of columns of the matrix C.\nCOMMENT\t*>           N  must be at least zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] ALPHA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          ALPHA is DOUBLE PRECISION.\nCOMMENT\t*>           On entry, ALPHA specifies the scalar alpha.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] A\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          A is DOUBLE PRECISION array, dimension ( LDA, ka ), where ka is\nCOMMENT\t*>           m  when  SIDE = 'L' or 'l'  and is  n otherwise.\nCOMMENT\t*>           Before entry  with  SIDE = 'L' or 'l',  the  m by m  part of\nCOMMENT\t*>           the array  A  must contain the  symmetric matrix,  such that\nCOMMENT\t*>           when  UPLO = 'U' or 'u', the leading m by m upper triangular\nCOMMENT\t*>           part of the array  A  must contain the upper triangular part\nCOMMENT\t*>           of the  symmetric matrix and the  strictly  lower triangular\nCOMMENT\t*>           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',\nCOMMENT\t*>           the leading  m by m  lower triangular part  of the  array  A\nCOMMENT\t*>           must  contain  the  lower triangular part  of the  symmetric\nCOMMENT\t*>           matrix and the  strictly upper triangular part of  A  is not\nCOMMENT\t*>           referenced.\nCOMMENT\t*>           Before entry  with  SIDE = 'R' or 'r',  the  n by n  part of\nCOMMENT\t*>           the array  A  must contain the  symmetric matrix,  such that\nCOMMENT\t*>           when  UPLO = 'U' or 'u', the leading n by n upper triangular\nCOMMENT\t*>           part of the array  A  must contain the upper triangular part\nCOMMENT\t*>           of the  symmetric matrix and the  strictly  lower triangular\nCOMMENT\t*>           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',\nCOMMENT\t*>           the leading  n by n  lower triangular part  of the  array  A\nCOMMENT\t*>           must  contain  the  lower triangular part  of the  symmetric\nCOMMENT\t*>           matrix and the  strictly upper triangular part of  A  is not\nCOMMENT\t*>           referenced.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] LDA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          LDA is INTEGER\nCOMMENT\t*>           On entry, LDA specifies the first dimension of A as declared\nCOMMENT\t*>           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then\nCOMMENT\t*>           LDA must be at least  max( 1, m ), otherwise  LDA must be at\nCOMMENT\t*>           least  max( 1, n ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] B\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          B is DOUBLE PRECISION array, dimension ( LDB, N )\nCOMMENT\t*>           Before entry, the leading  m by n part of the array  B  must\nCOMMENT\t*>           contain the matrix B.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] LDB\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          LDB is INTEGER\nCOMMENT\t*>           On entry, LDB specifies the first dimension of B as declared\nCOMMENT\t*>           in  the  calling  (sub)  program.   LDB  must  be  at  least\nCOMMENT\t*>           max( 1, m ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] BETA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          BETA is DOUBLE PRECISION.\nCOMMENT\t*>           On entry,  BETA  specifies the scalar  beta.  When  BETA  is\nCOMMENT\t*>           supplied as zero then C need not be set on input.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in,out] C\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          C is DOUBLE PRECISION array, dimension ( LDC, N )\nCOMMENT\t*>           Before entry, the leading  m by n  part of the array  C must\nCOMMENT\t*>           contain the matrix  C,  except when  beta  is zero, in which\nCOMMENT\t*>           case C need not be set on entry.\nCOMMENT\t*>           On exit, the array  C  is overwritten by the  m by n updated\nCOMMENT\t*>           matrix.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] LDC\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          LDC is INTEGER\nCOMMENT\t*>           On entry, LDC specifies the first dimension of C as declared\nCOMMENT\t*>           in  the  calling  (sub)  program.   LDC  must  be  at  least\nCOMMENT\t*>           max( 1, m ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Authors:\nCOMMENT\t*  ========\nCOMMENT\t*\nCOMMENT\t*> \\author Univ. of Tennessee\nCOMMENT\t*> \\author Univ. of California Berkeley\nCOMMENT\t*> \\author Univ. of Colorado Denver\nCOMMENT\t*> \\author NAG Ltd.\nCOMMENT\t*\nCOMMENT\t*> \\date December 2016\nCOMMENT\t*\nCOMMENT\t*> \\ingroup double_blas_level3\nCOMMENT\t*\nCOMMENT\t*> \\par Further Details:\nCOMMENT\t*  =====================\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*>  Level 3 Blas routine.\nCOMMENT\t*>\nCOMMENT\t*>  -- Written on 8-February-1989.\nCOMMENT\t*>     Jack Dongarra, Argonne National Laboratory.\nCOMMENT\t*>     Iain Duff, AERE Harwell.\nCOMMENT\t*>     Jeremy Du Croz, Numerical Algorithms Group Ltd.\nCOMMENT\t*>     Sven Hammarling, Numerical Algorithms Group Ltd.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*  =====================================================================\ntoken(96)\tSUBROUTINE\nIDENT\tDSYMM\n(\t(\nIDENT\tSIDE\n,\t,\nIDENT\tUPLO\n,\t,\nIDENT\tM\n,\t,\nIDENT\tN\n,\t,\nIDENT\tALPHA\n,\t,\nIDENT\tA\n,\t,\nIDENT\tLDA\n,\t,\nIDENT\tB\n,\t,\nIDENT\tLDB\n,\t,\nIDENT\tBETA\n,\t,\nIDENT\tC\n,\t,\nIDENT\tLDC\n)\t)\nCOMMENT\t*\nCOMMENT\t*  -- Reference BLAS level3 routine (version 3.7.0) --\nCOMMENT\t*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --\nCOMMENT\t*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--\nCOMMENT\t*     December 2016\nCOMMENT\t*\nCOMMENT\t*     .. Scalar Arguments ..\nIDENT\tDOUBLE\nIDENT\tPRECISION\nIDENT\tALPHA\n,\t,\nIDENT\tBETA\nIDENT\tINTEGER\nIDENT\tLDA\n,\t,\nIDENT\tLDB\n,\t,\nIDENT\tLDC\n,\t,\nIDENT\tM\n,\t,\nIDENT\tN\nIDENT\tCHARACTER\nIDENT\tSIDE\n,\t,\nIDENT\tUPLO\nCOMMENT\t*     ..\nCOMMENT\t*     .. Array Arguments ..\nIDENT\tDOUBLE\nIDENT\tPRECISION\nIDENT\tA\n(\t(\nIDENT\tLDA\n,\t,\n*\t*)\n,\t,\nIDENT\tB\n(\t(\nIDENT\tLDB\n,\t,\n*\t*)\n,\t,\nIDENT\tC\n(\t(\nIDENT\tLDC\n,\t,\n*\t*)\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*  =====================================================================\nCOMMENT\t*\nCOMMENT\t*     .. External Functions ..\nIDENT\tLOGICAL\nIDENT\tLSAME\nIDENT\tEXTERNAL\nIDENT\tLSAME\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Subroutines ..\nIDENT\tEXTERNAL\nIDENT\tXERBLA\nCOMMENT\t*     ..\nCOMMENT\t*     .. Intrinsic Functions ..\nIDENT\tINTRINSIC\nIDENT\tMAX\nCOMMENT\t*     ..\nCOMMENT\t*     .. Local Scalars ..\nIDENT\tDOUBLE\nIDENT\tPRECISION\nIDENT\tTEMP1\n,\t,\nIDENT\tTEMP2\nIDENT\tINTEGER\nIDENT\tI\n,\t,\nIDENT\tINFO\n,\t,\nIDENT\tJ\n,\t,\nIDENT\tK\n,\t,\nIDENT\tNROWA\nIDENT\tLOGICAL\nIDENT\tUPPER\nCOMMENT\t*     ..\nCOMMENT\t*     .. Parameters ..\nIDENT\tDOUBLE\nIDENT\tPRECISION\nIDENT\tONE\n,\t,\nIDENT\tZERO\nIDENT\tPARAMETER\n(\t(\nIDENT\tONE\n=\t=\nIDENT\t1\n.\t.\nIDENT\t0\nIDENT\tD\n+\t+\nIDENT\t0\n,\t,\nIDENT\tZERO\n=\t=\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tD\n+\t+\nIDENT\t0\n)\t)\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*     Set NROWA as the number of rows of A.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tLSAME\n(\t(\nIDENT\tSIDE\n,\t,\nSTRING\t'L'\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tNROWA\n=\t=\nIDENT\tM\nIDENT\tELSE\nIDENT\tNROWA\n=\t=\nIDENT\tN\nIDENT\tEND\nIDENT\tIF\nIDENT\tUPPER\n=\t=\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'U'\n)\t)\nCOMMENT\t*\nCOMMENT\t*     Test the input parameters.\nCOMMENT\t*\nIDENT\tINFO\n=\t=\nIDENT\t0\nIDENT\tIF\n(\t(\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tSIDE\n,\t,\nSTRING\t'L'\n)\t)\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tSIDE\n,\t,\nSTRING\t'R'\n)\t)\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tIF\n(\t(\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tUPPER\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'L'\n)\t)\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t2\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tM\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t3\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tN\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t4\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tLDA\n.\t.\nIDENT\tLT\n.\t.\nIDENT\tMAX\n(\t(\nIDENT\t1\n,\t,\nIDENT\tNROWA\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t7\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tLDB\n.\t.\nIDENT\tLT\n.\t.\nIDENT\tMAX\n(\t(\nIDENT\t1\n,\t,\nIDENT\tM\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t9\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tLDC\n.\t.\nIDENT\tLT\n.\t.\nIDENT\tMAX\n(\t(\nIDENT\t1\n,\t,\nIDENT\tM\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t12\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINFO\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tCALL\nIDENT\tXERBLA\n(\t(\nSTRING\t'DSYMM '\n,\t,\nIDENT\tINFO\n)\t)\nIDENT\tRETURN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Quick return if possible.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tM\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tN\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n+\t+\n(\t(\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tONE\n)\t)\n)\t)\n)\t)\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     And when  alpha.eq.zero.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t20\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t10\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\t10\nIDENT\tCONTINUE\nIDENT\t20\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t40\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t30\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t30\nIDENT\tCONTINUE\nIDENT\t40\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tRETURN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Start the operations.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tLSAME\n(\t(\nIDENT\tSIDE\n,\t,\nSTRING\t'L'\n)\t)\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*        Form  C := alpha*A*B + beta*C.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tUPPER\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t70\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t60\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*B\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t50\nIDENT\tK\n=\t=\nIDENT\t1\n,\t,\nIDENT\tI\n-\t-\nIDENT\t1\nIDENT\tC\n(\t(\nIDENT\tK\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tC\n(\t(\nIDENT\tK\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*A\n(\t(\nIDENT\tK\n,\t,\nIDENT\tI\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tTEMP2\n+\t+\nIDENT\tB\n(\t(\nIDENT\tK\n,\t,\nIDENT\tJ\n)\t)\n*\t*A\n(\t(\nIDENT\tK\n,\t,\nIDENT\tI\n)\t)\nIDENT\t50\nIDENT\tCONTINUE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tTEMP1\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tI\n)\t)\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP2\nIDENT\tELSE\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tI\n)\t)\n+\t+\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP2\nIDENT\tEND\nIDENT\tIF\nIDENT\t60\nIDENT\tCONTINUE\nIDENT\t70\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t100\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t90\nIDENT\tI\n=\t=\nIDENT\tM\n,\t,\nIDENT\t1\n,\t,\n-\t-\nIDENT\t1\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*B\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t80\nIDENT\tK\n=\t=\nIDENT\tI\n+\t+\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tK\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tC\n(\t(\nIDENT\tK\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*A\n(\t(\nIDENT\tK\n,\t,\nIDENT\tI\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tTEMP2\n+\t+\nIDENT\tB\n(\t(\nIDENT\tK\n,\t,\nIDENT\tJ\n)\t)\n*\t*A\n(\t(\nIDENT\tK\n,\t,\nIDENT\tI\n)\t)\nIDENT\t80\nIDENT\tCONTINUE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tTEMP1\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tI\n)\t)\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP2\nIDENT\tELSE\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tI\n)\t)\n+\t+\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP2\nIDENT\tEND\nIDENT\tIF\nIDENT\t90\nIDENT\tCONTINUE\nIDENT\t100\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*        Form  C := alpha*B*A + beta*C.\nCOMMENT\t*\nIDENT\tDO\nIDENT\t170\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*A\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t110\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tTEMP1\n*\t*B\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t110\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t120\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*B\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t120\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tDO\nIDENT\t140\nIDENT\tK\n=\t=\nIDENT\t1\n,\t,\nIDENT\tJ\n-\t-\nIDENT\t1\nIDENT\tIF\n(\t(\nIDENT\tUPPER\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*A\n(\t(\nIDENT\tK\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tELSE\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*A\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tK\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\tDO\nIDENT\t130\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*B\n(\t(\nIDENT\tI\n,\t,\nIDENT\tK\n)\t)\nIDENT\t130\nIDENT\tCONTINUE\nIDENT\t140\nIDENT\tCONTINUE\nIDENT\tDO\nIDENT\t160\nIDENT\tK\n=\t=\nIDENT\tJ\n+\t+\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\nIDENT\tUPPER\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*A\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tK\n)\t)\nIDENT\tELSE\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*A\n(\t(\nIDENT\tK\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\tDO\nIDENT\t150\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*B\n(\t(\nIDENT\tI\n,\t,\nIDENT\tK\n)\t)\nIDENT\t150\nIDENT\tCONTINUE\nIDENT\t160\nIDENT\tCONTINUE\nIDENT\t170\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     End of DSYMM .\nCOMMENT\t*\nIDENT\tEND\n"
