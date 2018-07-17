(string) (len=12507) "COMMENT\t> \\brief \\b SGEMV\nCOMMENT\t*\nCOMMENT\t*  =========== DOCUMENTATION ===========\nCOMMENT\t*\nCOMMENT\t* Online html documentation available at\nCOMMENT\t*            http://www.netlib.org/lapack/explore-html/\nCOMMENT\t*\nCOMMENT\t*  Definition:\nCOMMENT\t*  ===========\nCOMMENT\t*\nCOMMENT\t*       SUBROUTINE SGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)\nCOMMENT\t*\nCOMMENT\t*       .. Scalar Arguments ..\nCOMMENT\t*       REAL ALPHA,BETA\nCOMMENT\t*       INTEGER INCX,INCY,LDA,M,N\nCOMMENT\t*       CHARACTER TRANS\nCOMMENT\t*       ..\nCOMMENT\t*       .. Array Arguments ..\nCOMMENT\t*       REAL A(LDA,*),X(*),Y(*)\nCOMMENT\t*       ..\nCOMMENT\t*\nCOMMENT\t*\nCOMMENT\t*> \\par Purpose:\nCOMMENT\t*  =============\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*> SGEMV  performs one of the matrix-vector operations\nCOMMENT\t*>\nCOMMENT\t*>    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,\nCOMMENT\t*>\nCOMMENT\t*> where alpha and beta are scalars, x and y are vectors and A is an\nCOMMENT\t*> m by n matrix.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Arguments:\nCOMMENT\t*  ==========\nCOMMENT\t*\nCOMMENT\t*> \\param[in] TRANS\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          TRANS is CHARACTER*1\nCOMMENT\t*>           On entry, TRANS specifies the operation to be performed as\nCOMMENT\t*>           follows:\nCOMMENT\t*>\nCOMMENT\t*>              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.\nCOMMENT\t*>\nCOMMENT\t*>              TRANS = 'T' or 't'   y := alpha*A**T*x + beta*y.\nCOMMENT\t*>\nCOMMENT\t*>              TRANS = 'C' or 'c'   y := alpha*A**T*x + beta*y.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] M\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          M is INTEGER\nCOMMENT\t*>           On entry, M specifies the number of rows of the matrix A.\nCOMMENT\t*>           M must be at least zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] N\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          N is INTEGER\nCOMMENT\t*>           On entry, N specifies the number of columns of the matrix A.\nCOMMENT\t*>           N must be at least zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] ALPHA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          ALPHA is REAL\nCOMMENT\t*>           On entry, ALPHA specifies the scalar alpha.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] A\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          A is REAL array, dimension ( LDA, N )\nCOMMENT\t*>           Before entry, the leading m by n part of the array A must\nCOMMENT\t*>           contain the matrix of coefficients.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] LDA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          LDA is INTEGER\nCOMMENT\t*>           On entry, LDA specifies the first dimension of A as declared\nCOMMENT\t*>           in the calling (sub) program. LDA must be at least\nCOMMENT\t*>           max( 1, m ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] X\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          X is REAL array, dimension at least\nCOMMENT\t*>           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'\nCOMMENT\t*>           and at least\nCOMMENT\t*>           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.\nCOMMENT\t*>           Before entry, the incremented array X must contain the\nCOMMENT\t*>           vector x.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] INCX\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          INCX is INTEGER\nCOMMENT\t*>           On entry, INCX specifies the increment for the elements of\nCOMMENT\t*>           X. INCX must not be zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] BETA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          BETA is REAL\nCOMMENT\t*>           On entry, BETA specifies the scalar beta. When BETA is\nCOMMENT\t*>           supplied as zero then Y need not be set on input.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in,out] Y\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          Y is REAL array, dimension at least\nCOMMENT\t*>           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'\nCOMMENT\t*>           and at least\nCOMMENT\t*>           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.\nCOMMENT\t*>           Before entry with BETA non-zero, the incremented array Y\nCOMMENT\t*>           must contain the vector y. On exit, Y is overwritten by the\nCOMMENT\t*>           updated vector y.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] INCY\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          INCY is INTEGER\nCOMMENT\t*>           On entry, INCY specifies the increment for the elements of\nCOMMENT\t*>           Y. INCY must not be zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Authors:\nCOMMENT\t*  ========\nCOMMENT\t*\nCOMMENT\t*> \\author Univ. of Tennessee\nCOMMENT\t*> \\author Univ. of California Berkeley\nCOMMENT\t*> \\author Univ. of Colorado Denver\nCOMMENT\t*> \\author NAG Ltd.\nCOMMENT\t*\nCOMMENT\t*> \\date December 2016\nCOMMENT\t*\nCOMMENT\t*> \\ingroup single_blas_level2\nCOMMENT\t*\nCOMMENT\t*> \\par Further Details:\nCOMMENT\t*  =====================\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*>  Level 2 Blas routine.\nCOMMENT\t*>  The vector and matrix arguments are not referenced when N = 0, or M = 0\nCOMMENT\t*>\nCOMMENT\t*>  -- Written on 22-October-1986.\nCOMMENT\t*>     Jack Dongarra, Argonne National Lab.\nCOMMENT\t*>     Jeremy Du Croz, Nag Central Office.\nCOMMENT\t*>     Sven Hammarling, Nag Central Office.\nCOMMENT\t*>     Richard Hanson, Sandia National Labs.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*  =====================================================================\ntoken(96)\tSUBROUTINE\nIDENT\tSGEMV\n(\t(\nIDENT\tTRANS\n,\t,\nIDENT\tM\n,\t,\nIDENT\tN\n,\t,\nIDENT\tALPHA\n,\t,\nIDENT\tA\n,\t,\nIDENT\tLDA\n,\t,\nIDENT\tX\n,\t,\nIDENT\tINCX\n,\t,\nIDENT\tBETA\n,\t,\nIDENT\tY\n,\t,\nIDENT\tINCY\n)\t)\nCOMMENT\t*\nCOMMENT\t*  -- Reference BLAS level2 routine (version 3.7.0) --\nCOMMENT\t*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --\nCOMMENT\t*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--\nCOMMENT\t*     December 2016\nCOMMENT\t*\nCOMMENT\t*     .. Scalar Arguments ..\nIDENT\tREAL\nIDENT\tALPHA\n,\t,\nIDENT\tBETA\nIDENT\tINTEGER\nIDENT\tINCX\n,\t,\nIDENT\tINCY\n,\t,\nIDENT\tLDA\n,\t,\nIDENT\tM\n,\t,\nIDENT\tN\nIDENT\tCHARACTER\nIDENT\tTRANS\nCOMMENT\t*     ..\nCOMMENT\t*     .. Array Arguments ..\nIDENT\tREAL\nIDENT\tA\n(\t(\nIDENT\tLDA\n,\t,\n*\t*)\n,\t,\nIDENT\tX\n(\t(\n*\t*)\n,\t,\nIDENT\tY\n(\t(\n*\t*)\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*  =====================================================================\nCOMMENT\t*\nCOMMENT\t*     .. Parameters ..\nIDENT\tREAL\nIDENT\tONE\n,\t,\nIDENT\tZERO\nIDENT\tPARAMETER\n(\t(\nIDENT\tONE\n=\t=\nIDENT\t1\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n,\t,\nIDENT\tZERO\n=\t=\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n)\t)\nCOMMENT\t*     ..\nCOMMENT\t*     .. Local Scalars ..\nIDENT\tREAL\nIDENT\tTEMP\nIDENT\tINTEGER\nIDENT\tI\n,\t,\nIDENT\tINFO\n,\t,\nIDENT\tIX\n,\t,\nIDENT\tIY\n,\t,\nIDENT\tJ\n,\t,\nIDENT\tJX\n,\t,\nIDENT\tJY\n,\t,\nIDENT\tKX\n,\t,\nIDENT\tKY\n,\t,\nIDENT\tLENX\n,\t,\nIDENT\tLENY\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Functions ..\nIDENT\tLOGICAL\nIDENT\tLSAME\nIDENT\tEXTERNAL\nIDENT\tLSAME\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Subroutines ..\nIDENT\tEXTERNAL\nIDENT\tXERBLA\nCOMMENT\t*     ..\nCOMMENT\t*     .. Intrinsic Functions ..\nIDENT\tINTRINSIC\nIDENT\tMAX\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*     Test the input parameters.\nCOMMENT\t*\nIDENT\tINFO\n=\t=\nIDENT\t0\nIDENT\tIF\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tTRANS\n,\t,\nSTRING\t'N'\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tTRANS\n,\t,\nSTRING\t'T'\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n+\t+\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tTRANS\n,\t,\nSTRING\t'C'\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tM\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t2\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tN\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t3\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tLDA\n.\t.\nIDENT\tLT\n.\t.\nIDENT\tMAX\n(\t(\nIDENT\t1\n,\t,\nIDENT\tM\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t6\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t8\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t11\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINFO\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tCALL\nIDENT\tXERBLA\n(\t(\nSTRING\t'SGEMV '\n,\t,\nIDENT\tINFO\n)\t)\nIDENT\tRETURN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Quick return if possible.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tM\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tN\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n+\t+\n(\t(\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tONE\n)\t)\n)\t)\n)\t)\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set\nCOMMENT\t*     up the start points in  X  and  Y.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tLSAME\n(\t(\nIDENT\tTRANS\n,\t,\nSTRING\t'N'\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tLENX\n=\t=\nIDENT\tN\nIDENT\tLENY\n=\t=\nIDENT\tM\nIDENT\tELSE\nIDENT\tLENX\n=\t=\nIDENT\tM\nIDENT\tLENY\n=\t=\nIDENT\tN\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tGT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tKX\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tKX\n=\t=\nIDENT\t1\n-\t-\n(\t(\nIDENT\tLENX\n-\t-\nIDENT\t1\n)\t)\n*\t*I\nIDENT\tNCX\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tGT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tKY\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tKY\n=\t=\nIDENT\t1\n-\t-\n(\t(\nIDENT\tLENY\n-\t-\nIDENT\t1\n)\t)\n*\t*I\nIDENT\tNCY\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Start the operations. In this version the elements of A are\nCOMMENT\t*     accessed sequentially with one pass through A.\nCOMMENT\t*\nCOMMENT\t*     First form  y := beta*y.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tONE\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t10\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tLENY\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\t10\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t20\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tLENY\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n=\t=\nIDENT\tBETA\n*\t*Y\n(\t(\nIDENT\tI\n)\t)\nIDENT\t20\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nIDENT\tIY\n=\t=\nIDENT\tKY\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t30\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tLENY\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\t30\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t40\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tLENY\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n=\t=\nIDENT\tBETA\n*\t*Y\n(\t(\nIDENT\tIY\n)\t)\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\t40\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tRETURN\nIDENT\tIF\n(\t(\nIDENT\tLSAME\n(\t(\nIDENT\tTRANS\n,\t,\nSTRING\t'N'\n)\t)\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*        Form  y := alpha*A*x + y.\nCOMMENT\t*\nIDENT\tJX\n=\t=\nIDENT\tKX\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t60\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJX\n)\t)\nIDENT\tDO\nIDENT\t50\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n+\t+\nIDENT\tTEMP\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\t50\nIDENT\tCONTINUE\nIDENT\tJX\n=\t=\nIDENT\tJX\n+\t+\nIDENT\tINCX\nIDENT\t60\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t80\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJX\n)\t)\nIDENT\tIY\n=\t=\nIDENT\tKY\nIDENT\tDO\nIDENT\t70\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n+\t+\nIDENT\tTEMP\n*\t*A\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\t70\nIDENT\tCONTINUE\nIDENT\tJX\n=\t=\nIDENT\tJX\n+\t+\nIDENT\tINCX\nIDENT\t80\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*        Form  y := alpha*A**T*x + y.\nCOMMENT\t*\nIDENT\tJY\n=\t=\nIDENT\tKY\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t100\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP\n=\t=\nIDENT\tZERO\nIDENT\tDO\nIDENT\t90\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP\n=\t=\nIDENT\tTEMP\n+\t+\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n*\t*X\n(\t(\nIDENT\tI\n)\t)\nIDENT\t90\nIDENT\tCONTINUE\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\nIDENT\tJY\n=\t=\nIDENT\tJY\n+\t+\nIDENT\tINCY\nIDENT\t100\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t120\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP\n=\t=\nIDENT\tZERO\nIDENT\tIX\n=\t=\nIDENT\tKX\nIDENT\tDO\nIDENT\t110\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tM\nIDENT\tTEMP\n=\t=\nIDENT\tTEMP\n+\t+\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n*\t*X\n(\t(\nIDENT\tIX\n)\t)\nIDENT\tIX\n=\t=\nIDENT\tIX\n+\t+\nIDENT\tINCX\nIDENT\t110\nIDENT\tCONTINUE\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP\nIDENT\tJY\n=\t=\nIDENT\tJY\n+\t+\nIDENT\tINCY\nIDENT\t120\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     End of SGEMV .\nCOMMENT\t*\nIDENT\tEND\n"
