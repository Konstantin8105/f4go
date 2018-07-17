(string) (len=9972) "COMMENT\t> \\brief \\b SSPR\nCOMMENT\t*\nCOMMENT\t*  =========== DOCUMENTATION ===========\nCOMMENT\t*\nCOMMENT\t* Online html documentation available at\nCOMMENT\t*            http://www.netlib.org/lapack/explore-html/\nCOMMENT\t*\nCOMMENT\t*  Definition:\nCOMMENT\t*  ===========\nCOMMENT\t*\nCOMMENT\t*       SUBROUTINE SSPR(UPLO,N,ALPHA,X,INCX,AP)\nCOMMENT\t*\nCOMMENT\t*       .. Scalar Arguments ..\nCOMMENT\t*       REAL ALPHA\nCOMMENT\t*       INTEGER INCX,N\nCOMMENT\t*       CHARACTER UPLO\nCOMMENT\t*       ..\nCOMMENT\t*       .. Array Arguments ..\nCOMMENT\t*       REAL AP(*),X(*)\nCOMMENT\t*       ..\nCOMMENT\t*\nCOMMENT\t*\nCOMMENT\t*> \\par Purpose:\nCOMMENT\t*  =============\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*> SSPR    performs the symmetric rank 1 operation\nCOMMENT\t*>\nCOMMENT\t*>    A := alpha*x*x**T + A,\nCOMMENT\t*>\nCOMMENT\t*> where alpha is a real scalar, x is an n element vector and A is an\nCOMMENT\t*> n by n symmetric matrix, supplied in packed form.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Arguments:\nCOMMENT\t*  ==========\nCOMMENT\t*\nCOMMENT\t*> \\param[in] UPLO\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          UPLO is CHARACTER*1\nCOMMENT\t*>           On entry, UPLO specifies whether the upper or lower\nCOMMENT\t*>           triangular part of the matrix A is supplied in the packed\nCOMMENT\t*>           array AP as follows:\nCOMMENT\t*>\nCOMMENT\t*>              UPLO = 'U' or 'u'   The upper triangular part of A is\nCOMMENT\t*>                                  supplied in AP.\nCOMMENT\t*>\nCOMMENT\t*>              UPLO = 'L' or 'l'   The lower triangular part of A is\nCOMMENT\t*>                                  supplied in AP.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] N\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          N is INTEGER\nCOMMENT\t*>           On entry, N specifies the order of the matrix A.\nCOMMENT\t*>           N must be at least zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] ALPHA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          ALPHA is REAL\nCOMMENT\t*>           On entry, ALPHA specifies the scalar alpha.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] X\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          X is REAL array, dimension at least\nCOMMENT\t*>           ( 1 + ( n - 1 )*abs( INCX ) ).\nCOMMENT\t*>           Before entry, the incremented array X must contain the n\nCOMMENT\t*>           element vector x.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] INCX\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          INCX is INTEGER\nCOMMENT\t*>           On entry, INCX specifies the increment for the elements of\nCOMMENT\t*>           X. INCX must not be zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in,out] AP\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          AP is REAL array, dimension at least\nCOMMENT\t*>           ( ( n*( n + 1 ) )/2 ).\nCOMMENT\t*>           Before entry with  UPLO = 'U' or 'u', the array AP must\nCOMMENT\t*>           contain the upper triangular part of the symmetric matrix\nCOMMENT\t*>           packed sequentially, column by column, so that AP( 1 )\nCOMMENT\t*>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )\nCOMMENT\t*>           and a( 2, 2 ) respectively, and so on. On exit, the array\nCOMMENT\t*>           AP is overwritten by the upper triangular part of the\nCOMMENT\t*>           updated matrix.\nCOMMENT\t*>           Before entry with UPLO = 'L' or 'l', the array AP must\nCOMMENT\t*>           contain the lower triangular part of the symmetric matrix\nCOMMENT\t*>           packed sequentially, column by column, so that AP( 1 )\nCOMMENT\t*>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )\nCOMMENT\t*>           and a( 3, 1 ) respectively, and so on. On exit, the array\nCOMMENT\t*>           AP is overwritten by the lower triangular part of the\nCOMMENT\t*>           updated matrix.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Authors:\nCOMMENT\t*  ========\nCOMMENT\t*\nCOMMENT\t*> \\author Univ. of Tennessee\nCOMMENT\t*> \\author Univ. of California Berkeley\nCOMMENT\t*> \\author Univ. of Colorado Denver\nCOMMENT\t*> \\author NAG Ltd.\nCOMMENT\t*\nCOMMENT\t*> \\date December 2016\nCOMMENT\t*\nCOMMENT\t*> \\ingroup single_blas_level2\nCOMMENT\t*\nCOMMENT\t*> \\par Further Details:\nCOMMENT\t*  =====================\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*>  Level 2 Blas routine.\nCOMMENT\t*>\nCOMMENT\t*>  -- Written on 22-October-1986.\nCOMMENT\t*>     Jack Dongarra, Argonne National Lab.\nCOMMENT\t*>     Jeremy Du Croz, Nag Central Office.\nCOMMENT\t*>     Sven Hammarling, Nag Central Office.\nCOMMENT\t*>     Richard Hanson, Sandia National Labs.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*  =====================================================================\ntoken(96)\tSUBROUTINE\nIDENT\tSSPR\n(\t(\nIDENT\tUPLO\n,\t,\nIDENT\tN\n,\t,\nIDENT\tALPHA\n,\t,\nIDENT\tX\n,\t,\nIDENT\tINCX\n,\t,\nIDENT\tAP\n)\t)\nCOMMENT\t*\nCOMMENT\t*  -- Reference BLAS level2 routine (version 3.7.0) --\nCOMMENT\t*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --\nCOMMENT\t*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--\nCOMMENT\t*     December 2016\nCOMMENT\t*\nCOMMENT\t*     .. Scalar Arguments ..\nIDENT\tREAL\nIDENT\tALPHA\nIDENT\tINTEGER\nIDENT\tINCX\n,\t,\nIDENT\tN\nIDENT\tCHARACTER\nIDENT\tUPLO\nCOMMENT\t*     ..\nCOMMENT\t*     .. Array Arguments ..\nIDENT\tREAL\nIDENT\tAP\n(\t(\n*\t*)\n,\t,\nIDENT\tX\n(\t(\n*\t*)\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*  =====================================================================\nCOMMENT\t*\nCOMMENT\t*     .. Parameters ..\nIDENT\tREAL\nIDENT\tZERO\nIDENT\tPARAMETER\n(\t(\nIDENT\tZERO\n=\t=\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n)\t)\nCOMMENT\t*     ..\nCOMMENT\t*     .. Local Scalars ..\nIDENT\tREAL\nIDENT\tTEMP\nIDENT\tINTEGER\nIDENT\tI\n,\t,\nIDENT\tINFO\n,\t,\nIDENT\tIX\n,\t,\nIDENT\tJ\n,\t,\nIDENT\tJX\n,\t,\nIDENT\tK\n,\t,\nIDENT\tKK\n,\t,\nIDENT\tKX\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Functions ..\nIDENT\tLOGICAL\nIDENT\tLSAME\nIDENT\tEXTERNAL\nIDENT\tLSAME\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Subroutines ..\nIDENT\tEXTERNAL\nIDENT\tXERBLA\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*     Test the input parameters.\nCOMMENT\t*\nIDENT\tINFO\n=\t=\nIDENT\t0\nIDENT\tIF\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'U'\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'L'\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tN\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t2\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t5\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINFO\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tCALL\nIDENT\tXERBLA\n(\t(\nSTRING\t'SSPR  '\n,\t,\nIDENT\tINFO\n)\t)\nIDENT\tRETURN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Quick return if possible.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tN\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\n)\t)\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     Set the start point in X if the increment is not unity.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tLE\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tKX\n=\t=\nIDENT\t1\n-\t-\n(\t(\nIDENT\tN\n-\t-\nIDENT\t1\n)\t)\n*\t*I\nIDENT\tNCX\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t1\n)\t)\nIDENT\tTHEN\nIDENT\tKX\n=\t=\nIDENT\t1\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Start the operations. In this version the elements of the array AP\nCOMMENT\t*     are accessed sequentially with one pass through AP.\nCOMMENT\t*\nIDENT\tKK\n=\t=\nIDENT\t1\nIDENT\tIF\n(\t(\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'U'\n)\t)\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*        Form  A  when upper triangle is stored in AP.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t20\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJ\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJ\n)\t)\nIDENT\tK\n=\t=\nIDENT\tKK\nIDENT\tDO\nIDENT\t10\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tJ\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n=\t=\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n+\t+\nIDENT\tX\n(\t(\nIDENT\tI\n)\t)\n*\t*T\nIDENT\tEMP\nIDENT\tK\n=\t=\nIDENT\tK\n+\t+\nIDENT\t1\nIDENT\t10\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tKK\n=\t=\nIDENT\tKK\n+\t+\nIDENT\tJ\nIDENT\t20\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tJX\n=\t=\nIDENT\tKX\nIDENT\tDO\nIDENT\t40\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJX\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJX\n)\t)\nIDENT\tIX\n=\t=\nIDENT\tKX\nIDENT\tDO\nIDENT\t30\nIDENT\tK\n=\t=\nIDENT\tKK\n,\t,\nIDENT\tKK\n+\t+\nIDENT\tJ\n-\t-\nIDENT\t1\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n=\t=\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n+\t+\nIDENT\tX\n(\t(\nIDENT\tIX\n)\t)\n*\t*T\nIDENT\tEMP\nIDENT\tIX\n=\t=\nIDENT\tIX\n+\t+\nIDENT\tINCX\nIDENT\t30\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tJX\n=\t=\nIDENT\tJX\n+\t+\nIDENT\tINCX\nIDENT\tKK\n=\t=\nIDENT\tKK\n+\t+\nIDENT\tJ\nIDENT\t40\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*        Form  A  when lower triangle is stored in AP.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t60\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJ\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJ\n)\t)\nIDENT\tK\n=\t=\nIDENT\tKK\nIDENT\tDO\nIDENT\t50\nIDENT\tI\n=\t=\nIDENT\tJ\n,\t,\nIDENT\tN\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n=\t=\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n+\t+\nIDENT\tX\n(\t(\nIDENT\tI\n)\t)\n*\t*T\nIDENT\tEMP\nIDENT\tK\n=\t=\nIDENT\tK\n+\t+\nIDENT\t1\nIDENT\t50\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tKK\n=\t=\nIDENT\tKK\n+\t+\nIDENT\tN\n-\t-\nIDENT\tJ\n+\t+\nIDENT\t1\nIDENT\t60\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tJX\n=\t=\nIDENT\tKX\nIDENT\tDO\nIDENT\t80\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJX\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJX\n)\t)\nIDENT\tIX\n=\t=\nIDENT\tJX\nIDENT\tDO\nIDENT\t70\nIDENT\tK\n=\t=\nIDENT\tKK\n,\t,\nIDENT\tKK\n+\t+\nIDENT\tN\n-\t-\nIDENT\tJ\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n=\t=\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n+\t+\nIDENT\tX\n(\t(\nIDENT\tIX\n)\t)\n*\t*T\nIDENT\tEMP\nIDENT\tIX\n=\t=\nIDENT\tIX\n+\t+\nIDENT\tINCX\nIDENT\t70\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tJX\n=\t=\nIDENT\tJX\n+\t+\nIDENT\tINCX\nIDENT\tKK\n=\t=\nIDENT\tKK\n+\t+\nIDENT\tN\n-\t-\nIDENT\tJ\n+\t+\nIDENT\t1\nIDENT\t80\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     End of SSPR  .\nCOMMENT\t*\nIDENT\tEND\n"
