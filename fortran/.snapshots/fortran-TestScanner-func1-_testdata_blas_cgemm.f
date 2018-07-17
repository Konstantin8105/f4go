(string) (len=20270) "COMMENT\t> \\brief \\b CGEMM\nCOMMENT\t*\nCOMMENT\t*  =========== DOCUMENTATION ===========\nCOMMENT\t*\nCOMMENT\t* Online html documentation available at\nCOMMENT\t*            http://www.netlib.org/lapack/explore-html/\nCOMMENT\t*\nCOMMENT\t*  Definition:\nCOMMENT\t*  ===========\nCOMMENT\t*\nCOMMENT\t*       SUBROUTINE CGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)\nCOMMENT\t*\nCOMMENT\t*       .. Scalar Arguments ..\nCOMMENT\t*       COMPLEX ALPHA,BETA\nCOMMENT\t*       INTEGER K,LDA,LDB,LDC,M,N\nCOMMENT\t*       CHARACTER TRANSA,TRANSB\nCOMMENT\t*       ..\nCOMMENT\t*       .. Array Arguments ..\nCOMMENT\t*       COMPLEX A(LDA,*),B(LDB,*),C(LDC,*)\nCOMMENT\t*       ..\nCOMMENT\t*\nCOMMENT\t*\nCOMMENT\t*> \\par Purpose:\nCOMMENT\t*  =============\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*> CGEMM  performs one of the matrix-matrix operations\nCOMMENT\t*>\nCOMMENT\t*>    C := alpha*op( A )*op( B ) + beta*C,\nCOMMENT\t*>\nCOMMENT\t*> where  op( X ) is one of\nCOMMENT\t*>\nCOMMENT\t*>    op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,\nCOMMENT\t*>\nCOMMENT\t*> alpha and beta are scalars, and A, B and C are matrices, with op( A )\nCOMMENT\t*> an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Arguments:\nCOMMENT\t*  ==========\nCOMMENT\t*\nCOMMENT\t*> \\param[in] TRANSA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          TRANSA is CHARACTER*1\nCOMMENT\t*>           On entry, TRANSA specifies the form of op( A ) to be used in\nCOMMENT\t*>           the matrix multiplication as follows:\nCOMMENT\t*>\nCOMMENT\t*>              TRANSA = 'N' or 'n',  op( A ) = A.\nCOMMENT\t*>\nCOMMENT\t*>              TRANSA = 'T' or 't',  op( A ) = A**T.\nCOMMENT\t*>\nCOMMENT\t*>              TRANSA = 'C' or 'c',  op( A ) = A**H.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] TRANSB\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          TRANSB is CHARACTER*1\nCOMMENT\t*>           On entry, TRANSB specifies the form of op( B ) to be used in\nCOMMENT\t*>           the matrix multiplication as follows:\nCOMMENT\t*>\nCOMMENT\t*>              TRANSB = 'N' or 'n',  op( B ) = B.\nCOMMENT\t*>\nCOMMENT\t*>              TRANSB = 'T' or 't',  op( B ) = B**T.\nCOMMENT\t*>\nCOMMENT\t*>              TRANSB = 'C' or 'c',  op( B ) = B**H.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] M\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          M is INTEGER\nCOMMENT\t*>           On entry,  M  specifies  the number  of rows  of the  matrix\nCOMMENT\t*>           op( A )  and of the  matrix  C.  M  must  be at least  zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] N\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          N is INTEGER\nCOMMENT\t*>           On entry,  N  specifies the number  of columns of the matrix\nCOMMENT\t*>           op( B ) and the number of columns of the matrix C. N must be\nCOMMENT\t*>           at least zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] K\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          K is INTEGER\nCOMMENT\t*>           On entry,  K  specifies  the number of columns of the matrix\nCOMMENT\t*>           op( A ) and the number of rows of the matrix op( B ). K must\nCOMMENT\t*>           be at least  zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] ALPHA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          ALPHA is COMPLEX\nCOMMENT\t*>           On entry, ALPHA specifies the scalar alpha.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] A\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          A is COMPLEX array, dimension ( LDA, ka ), where ka is\nCOMMENT\t*>           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.\nCOMMENT\t*>           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k\nCOMMENT\t*>           part of the array  A  must contain the matrix  A,  otherwise\nCOMMENT\t*>           the leading  k by m  part of the array  A  must contain  the\nCOMMENT\t*>           matrix A.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] LDA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          LDA is INTEGER\nCOMMENT\t*>           On entry, LDA specifies the first dimension of A as declared\nCOMMENT\t*>           in the calling (sub) program. When  TRANSA = 'N' or 'n' then\nCOMMENT\t*>           LDA must be at least  max( 1, m ), otherwise  LDA must be at\nCOMMENT\t*>           least  max( 1, k ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] B\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          B is COMPLEX array, dimension ( LDB, kb ), where kb is\nCOMMENT\t*>           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.\nCOMMENT\t*>           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n\nCOMMENT\t*>           part of the array  B  must contain the matrix  B,  otherwise\nCOMMENT\t*>           the leading  n by k  part of the array  B  must contain  the\nCOMMENT\t*>           matrix B.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] LDB\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          LDB is INTEGER\nCOMMENT\t*>           On entry, LDB specifies the first dimension of B as declared\nCOMMENT\t*>           in the calling (sub) program. When  TRANSB = 'N' or 'n' then\nCOMMENT\t*>           LDB must be at least  max( 1, k ), otherwise  LDB must be at\nCOMMENT\t*>           least  max( 1, n ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] BETA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          BETA is COMPLEX\nCOMMENT\t*>           On entry,  BETA  specifies the scalar  beta.  When  BETA  is\nCOMMENT\t*>           supplied as zero then C need not be set on input.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in,out] C\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          C is COMPLEX array, dimension ( LDC, N )\nCOMMENT\t*>           Before entry, the leading  m by n  part of the array  C must\nCOMMENT\t*>           contain the matrix  C,  except when  beta  is zero, in which\nCOMMENT\t*>           case C need not be set on entry.\nCOMMENT\t*>           On exit, the array  C  is overwritten by the  m by n  matrix\nCOMMENT\t*>           ( alpha*op( A )*op( B ) + beta*C ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] LDC\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          LDC is INTEGER\nCOMMENT\t*>           On entry, LDC specifies the first dimension of C as declared\nCOMMENT\t*>           in  the  calling  (sub)  program.   LDC  must  be  at  least\nCOMMENT\t*>           max( 1, m ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Authors:\nCOMMENT\t*  ========\nCOMMENT\t*\nCOMMENT\t*> \\author Univ. of Tennessee\nCOMMENT\t*> \\author Univ. of California Berkeley\nCOMMENT\t*> \\author Univ. of Colorado Denver\nCOMMENT\t*> \\author NAG Ltd.\nCOMMENT\t*\nCOMMENT\t*> \\date December 2016\nCOMMENT\t*\nCOMMENT\t*> \\ingroup complex_blas_level3\nCOMMENT\t*\nCOMMENT\t*> \\par Further Details:\nCOMMENT\t*  =====================\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*>  Level 3 Blas routine.\nCOMMENT\t*>\nCOMMENT\t*>  -- Written on 8-February-1989.\nCOMMENT\t*>     Jack Dongarra, Argonne National Laboratory.\nCOMMENT\t*>     Iain Duff, AERE Harwell.\nCOMMENT\t*>     Jeremy Du Croz, Numerical Algorithms Group Ltd.\nCOMMENT\t*>     Sven Hammarling, Numerical Algorithms Group Ltd.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*  =====================================================================\ntoken(96)\tSUBROUTINE\nIDENT\tCGEMM\n(\t(\nIDENT\tTRANSA\n,\t,\nIDENT\tTRANSB\n,\t,\nIDENT\tM\n,\t,\nIDENT\tN\n,\t,\nIDENT\tK\n,\t,\nIDENT\tALPHA\n,\t,\nIDENT\tA\n,\t,\nIDENT\tLDA\n,\t,\nIDENT\tB\n,\t,\nIDENT\tLDB\n,\t,\nIDENT\tBETA\n,\t,\nIDENT\tC\n,\t,\nIDENT\tLDC\n)\t)\nCOMMENT\t*\nCOMMENT\t*  -- Reference BLAS level3 routine (version 3.7.0) --\nCOMMENT\t*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --\nCOMMENT\t*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--\nCOMMENT\t*     December 2016\nCOMMENT\t*\nCOMMENT\t*     .. Scalar Arguments ..\nIDENT\tCOMPLEX\nIDENT\tALPHA\n,\t,\nIDENT\tBETA\nIDENT\tINTEGER\nIDENT\tK\n,\t,\nIDENT\tLDA\n,\t,\nIDENT\tLDB\n,\t,\nIDENT\tLDC\n,\t,\nIDENT\tM\n,\t,\nIDENT\tN\nIDENT\tCHARACTER\nIDENT\tTRANSA\n,\t,\nIDENT\tTRANSB\nCOMMENT\t*     ..\nCOMMENT\t*     .. Array Arguments ..\nIDENT\tCOMPLEX\nIDENT\tA\n(\t(\nIDENT\tLDA\n,\t,\n*\t*)\n,\t,\nIDENT\tB\n(\t(\nIDENT\tLDB\n,\t,\n*\t*)\n,\t,\nIDENT\tC\n(\t(\nIDENT\tLDC\n,\t,\n*\t*)\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*  =====================================================================\nCOMMENT\t*\nCOMMENT\t*     .. External Functions ..\nIDENT\tLOGICAL\nIDENT\tLSAME\nIDENT\tEXTERNAL\nIDENT\tLSAME\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Subroutines ..\nIDENT\tEXTERNAL\nIDENT\tXERBLA\nCOMMENT\t*     ..\nCOMMENT\t*     .. Intrinsic Functions ..\nIDENT\tINTRINSIC\nIDENT\tCONJG\n,\t,\nIDENT\tMAX\nCOMMENT\t*     ..\nCOMMENT\t*     .. Local Scalars ..\nIDENT\tCOMPLEX\nIDENT\tTEMP\nIDENT\tINTEGER\nIDENT\tI\n,\t,\nIDENT\tINFO\n,\t,\nIDENT\tJ\n,\t,\nIDENT\tL\n,\t,\nIDENT\tNCOLA\n,\t,\nIDENT\tNROWA\n,\t,\nIDENT\tNROWB\nIDENT\tLOGICAL\nIDENT\tCONJA\n,\t,\nIDENT\tCONJB\n,\t,\nIDENT\tNOTA\n,\t,\nIDENT\tNOTB\nCOMMENT\t*     ..\nCOMMENT\t*     .. Parameters ..\nIDENT\tCOMPLEX\nIDENT\tONE\nIDENT\tPARAMETER\n(\t(\nIDENT\tONE\n=\t=\n(\t(\nIDENT\t1\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n,\t,\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n)\t)\n)\t)\nIDENT\tCOMPLEX\nIDENT\tZERO\nIDENT\tPARAMETER\n(\t(\nIDENT\tZERO\n=\t=\n(\t(\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n,\t,\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n)\t)\n)\t)\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not\nCOMMENT\t*     conjugated or transposed, set  CONJA and CONJB  as true if  A  and\nCOMMENT\t*     B  respectively are to be  transposed but  not conjugated  and set\nCOMMENT\t*     NROWA, NCOLA and  NROWB  as the number of rows and  columns  of  A\nCOMMENT\t*     and the number of rows of  B  respectively.\nCOMMENT\t*\nIDENT\tNOTA\n=\t=\nIDENT\tLSAME\n(\t(\nIDENT\tTRANSA\n,\t,\nSTRING\t'N'\n)\t)\nIDENT\tNOTB\n=\t=\nIDENT\tLSAME\n(\t(\nIDENT\tTRANSB\n,\t,\nSTRING\t'N'\n)\t)\nIDENT\tCONJA\n=\t=\nIDENT\tLSAME\n(\t(\nIDENT\tTRANSA\n,\t,\nSTRING\t'C'\n)\t)\nIDENT\tCONJB\n=\t=\nIDENT\tLSAME\n(\t(\nIDENT\tTRANSB\n,\t,\nSTRING\t'C'\n)\t)\nIDENT\tIF\n(\t(\nIDENT\tNOTA\n)\t)\nIDENT\tTHEN\nIDENT\tNROWA\n=\t=\nIDENT\tM\nIDENT\tNCOLA\n=\t=\nIDENT\tK\nIDENT\tELSE\nIDENT\tNROWA\n=\t=\nIDENT\tK\nIDENT\tNCOLA\n=\t=\nIDENT\tM\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tNOTB\n)\t)\nIDENT\tTHEN\nIDENT\tNROWB\n=\t=\nIDENT\tK\nIDENT\tELSE\nIDENT\tNROWB\n=\t=\nIDENT\tN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Test the input parameters.\nCOMMENT\t*\nIDENT\tINFO\n=\t=\nIDENT\t0\nIDENT\tIF\n(\t(\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tNOTA\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tCONJA\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n+\t+\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tTRANSA\n,\t,\nSTRING\t'T'\n)\t)\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tIF\n(\t(\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tNOTB\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tCONJB\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n+\t+\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tTRANSB\n,\t,\nSTRING\t'T'\n)\t)\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t2\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tM\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t3\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tN\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t4\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tK\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t5\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tLDA\n.\t.\nIDENT\tLT\n.\t.\nIDENT\tMAX\n(\t(\nIDENT\t1\n,\t,\nIDENT\tNROWA\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t8\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tLDB\n.\t.\nIDENT\tLT\n.\t.\nIDENT\tMAX\n(\t(\nIDENT\t1\n,\t,\nIDENT\tNROWB\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t10\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tLDC\n.\t.\nIDENT\tLT\n.\t.\nIDENT\tMAX\n(\t(\nIDENT\t1\n,\t,\nIDENT\tM\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t13\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINFO\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tCALL\nIDENT\tXERBLA\n(\t(\nSTRING\t'CGEMM '\n,\t,\nIDENT\tINFO\n)\t)\nIDENT\tRETURN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Quick return if possible.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tM\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tN\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n+\t+\n(\t(\n(\t(\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tK\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tONE\n)\t)\n)\t)\n)\t)\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     And when  alpha.eq.zero.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t20\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t10\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\t10\nIDENT\tCONTINUE\nIDENT\t20\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t40\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t30\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t30\nIDENT\tCONTINUE\nIDENT\t40\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tRETURN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Start the operations.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tNOTB\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tNOTA\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A*B + beta*C.\nCOMMENT\t*\nIDENT\tDO\nIDENT\t90\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t50\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\t50\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tONE\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t60\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t60\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tDO\nIDENT\t80\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*B\n(\t(\nIDENT\tL\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tDO\nIDENT\t70\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tL\n)\t)\nIDENT\t70\nIDENT\tCONTINUE\nIDENT\t80\nIDENT\tCONTINUE\nIDENT\t90\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tCONJA\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A**H*B + beta*C.\nCOMMENT\t*\nIDENT\tDO\nIDENT\t120\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t110\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t100\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tTEMP\n+\t+\nIDENT\tCONJG\n(\t(\nIDENT\tA\n(\t(\nIDENT\tL\n,\t,\nIDENT\tI\n)\t)\n)\t)\n*\t*B\n(\t(\nIDENT\tL\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t100\nIDENT\tCONTINUE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\nIDENT\tELSE\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\n+\t+\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\t110\nIDENT\tCONTINUE\nIDENT\t120\nIDENT\tCONTINUE\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A**T*B + beta*C\nCOMMENT\t*\nIDENT\tDO\nIDENT\t150\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t140\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t130\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tTEMP\n+\t+\nIDENT\tA\n(\t(\nIDENT\tL\n,\t,\nIDENT\tI\n)\t)\n*\t*B\n(\t(\nIDENT\tL\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t130\nIDENT\tCONTINUE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\nIDENT\tELSE\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\n+\t+\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\t140\nIDENT\tCONTINUE\nIDENT\t150\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tNOTA\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tCONJB\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A*B**H + beta*C.\nCOMMENT\t*\nIDENT\tDO\nIDENT\t200\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t160\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\t160\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tONE\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t170\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t170\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tDO\nIDENT\t190\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*C\nIDENT\tONJG\n(\t(\nIDENT\tB\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tL\n)\t)\n)\t)\nIDENT\tDO\nIDENT\t180\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tL\n)\t)\nIDENT\t180\nIDENT\tCONTINUE\nIDENT\t190\nIDENT\tCONTINUE\nIDENT\t200\nIDENT\tCONTINUE\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A*B**T + beta*C\nCOMMENT\t*\nIDENT\tDO\nIDENT\t250\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t210\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\t210\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tONE\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t220\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t220\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tDO\nIDENT\t240\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*B\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tL\n)\t)\nIDENT\tDO\nIDENT\t230\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tL\n)\t)\nIDENT\t230\nIDENT\tCONTINUE\nIDENT\t240\nIDENT\tCONTINUE\nIDENT\t250\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tCONJA\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tCONJB\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A**H*B**H + beta*C.\nCOMMENT\t*\nIDENT\tDO\nIDENT\t280\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t270\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t260\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tTEMP\n+\t+\nIDENT\tCONJG\n(\t(\nIDENT\tA\n(\t(\nIDENT\tL\n,\t,\nIDENT\tI\n)\t)\n)\t)\n*\t*C\nIDENT\tONJG\n(\t(\nIDENT\tB\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tL\n)\t)\n)\t)\nIDENT\t260\nIDENT\tCONTINUE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\nIDENT\tELSE\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\n+\t+\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\t270\nIDENT\tCONTINUE\nIDENT\t280\nIDENT\tCONTINUE\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A**H*B**T + beta*C\nCOMMENT\t*\nIDENT\tDO\nIDENT\t310\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t300\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t290\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tTEMP\n+\t+\nIDENT\tCONJG\n(\t(\nIDENT\tA\n(\t(\nIDENT\tL\n,\t,\nIDENT\tI\n)\t)\n)\t)\n*\t*B\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tL\n)\t)\nIDENT\t290\nIDENT\tCONTINUE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\nIDENT\tELSE\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\n+\t+\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\t300\nIDENT\tCONTINUE\nIDENT\t310\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tCONJB\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A**T*B**H + beta*C\nCOMMENT\t*\nIDENT\tDO\nIDENT\t340\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t330\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t320\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tTEMP\n+\t+\nIDENT\tA\n(\t(\nIDENT\tL\n,\t,\nIDENT\tI\n)\t)\n*\t*C\nIDENT\tONJG\n(\t(\nIDENT\tB\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tL\n)\t)\n)\t)\nIDENT\t320\nIDENT\tCONTINUE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\nIDENT\tELSE\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\n+\t+\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\t330\nIDENT\tCONTINUE\nIDENT\t340\nIDENT\tCONTINUE\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*           Form  C := alpha*A**T*B**T + beta*C\nCOMMENT\t*\nIDENT\tDO\nIDENT\t370\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tDO\nIDENT\t360\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t350\nIDENT\tL\n=\t=\nIDENT\t1\n,\t,\nIDENT\tK\nIDENT\tTEMP\n=\t=\nIDENT\tTEMP\n+\t+\nIDENT\tA\n(\t(\nIDENT\tL\n,\t,\nIDENT\tI\n)\t)\n*\t*B\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tL\n)\t)\nIDENT\t350\nIDENT\tCONTINUE\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\nIDENT\tELSE\nIDENT\tC\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\n+\t+\nIDENT\tBETA\n*\t*C\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\t360\nIDENT\tCONTINUE\nIDENT\t370\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     End of CGEMM .\nCOMMENT\t*\nIDENT\tEND\n"
