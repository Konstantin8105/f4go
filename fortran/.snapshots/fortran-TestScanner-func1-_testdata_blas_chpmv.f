(string) (len=14013) "COMMENT\t> \\brief \\b CHPMV\nCOMMENT\t*\nCOMMENT\t*  =========== DOCUMENTATION ===========\nCOMMENT\t*\nCOMMENT\t* Online html documentation available at\nCOMMENT\t*            http://www.netlib.org/lapack/explore-html/\nCOMMENT\t*\nCOMMENT\t*  Definition:\nCOMMENT\t*  ===========\nCOMMENT\t*\nCOMMENT\t*       SUBROUTINE CHPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)\nCOMMENT\t*\nCOMMENT\t*       .. Scalar Arguments ..\nCOMMENT\t*       COMPLEX ALPHA,BETA\nCOMMENT\t*       INTEGER INCX,INCY,N\nCOMMENT\t*       CHARACTER UPLO\nCOMMENT\t*       ..\nCOMMENT\t*       .. Array Arguments ..\nCOMMENT\t*       COMPLEX AP(*),X(*),Y(*)\nCOMMENT\t*       ..\nCOMMENT\t*\nCOMMENT\t*\nCOMMENT\t*> \\par Purpose:\nCOMMENT\t*  =============\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*> CHPMV  performs the matrix-vector operation\nCOMMENT\t*>\nCOMMENT\t*>    y := alpha*A*x + beta*y,\nCOMMENT\t*>\nCOMMENT\t*> where alpha and beta are scalars, x and y are n element vectors and\nCOMMENT\t*> A is an n by n hermitian matrix, supplied in packed form.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Arguments:\nCOMMENT\t*  ==========\nCOMMENT\t*\nCOMMENT\t*> \\param[in] UPLO\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          UPLO is CHARACTER*1\nCOMMENT\t*>           On entry, UPLO specifies whether the upper or lower\nCOMMENT\t*>           triangular part of the matrix A is supplied in the packed\nCOMMENT\t*>           array AP as follows:\nCOMMENT\t*>\nCOMMENT\t*>              UPLO = 'U' or 'u'   The upper triangular part of A is\nCOMMENT\t*>                                  supplied in AP.\nCOMMENT\t*>\nCOMMENT\t*>              UPLO = 'L' or 'l'   The lower triangular part of A is\nCOMMENT\t*>                                  supplied in AP.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] N\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          N is INTEGER\nCOMMENT\t*>           On entry, N specifies the order of the matrix A.\nCOMMENT\t*>           N must be at least zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] ALPHA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          ALPHA is COMPLEX\nCOMMENT\t*>           On entry, ALPHA specifies the scalar alpha.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] AP\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          AP is COMPLEX array, dimension at least\nCOMMENT\t*>           ( ( n*( n + 1 ) )/2 ).\nCOMMENT\t*>           Before entry with UPLO = 'U' or 'u', the array AP must\nCOMMENT\t*>           contain the upper triangular part of the hermitian matrix\nCOMMENT\t*>           packed sequentially, column by column, so that AP( 1 )\nCOMMENT\t*>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )\nCOMMENT\t*>           and a( 2, 2 ) respectively, and so on.\nCOMMENT\t*>           Before entry with UPLO = 'L' or 'l', the array AP must\nCOMMENT\t*>           contain the lower triangular part of the hermitian matrix\nCOMMENT\t*>           packed sequentially, column by column, so that AP( 1 )\nCOMMENT\t*>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )\nCOMMENT\t*>           and a( 3, 1 ) respectively, and so on.\nCOMMENT\t*>           Note that the imaginary parts of the diagonal elements need\nCOMMENT\t*>           not be set and are assumed to be zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] X\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          X is COMPLEX array, dimension at least\nCOMMENT\t*>           ( 1 + ( n - 1 )*abs( INCX ) ).\nCOMMENT\t*>           Before entry, the incremented array X must contain the n\nCOMMENT\t*>           element vector x.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] INCX\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          INCX is INTEGER\nCOMMENT\t*>           On entry, INCX specifies the increment for the elements of\nCOMMENT\t*>           X. INCX must not be zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] BETA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          BETA is COMPLEX\nCOMMENT\t*>           On entry, BETA specifies the scalar beta. When BETA is\nCOMMENT\t*>           supplied as zero then Y need not be set on input.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in,out] Y\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          Y is COMPLEX array, dimension at least\nCOMMENT\t*>           ( 1 + ( n - 1 )*abs( INCY ) ).\nCOMMENT\t*>           Before entry, the incremented array Y must contain the n\nCOMMENT\t*>           element vector y. On exit, Y is overwritten by the updated\nCOMMENT\t*>           vector y.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] INCY\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          INCY is INTEGER\nCOMMENT\t*>           On entry, INCY specifies the increment for the elements of\nCOMMENT\t*>           Y. INCY must not be zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Authors:\nCOMMENT\t*  ========\nCOMMENT\t*\nCOMMENT\t*> \\author Univ. of Tennessee\nCOMMENT\t*> \\author Univ. of California Berkeley\nCOMMENT\t*> \\author Univ. of Colorado Denver\nCOMMENT\t*> \\author NAG Ltd.\nCOMMENT\t*\nCOMMENT\t*> \\date December 2016\nCOMMENT\t*\nCOMMENT\t*> \\ingroup complex_blas_level2\nCOMMENT\t*\nCOMMENT\t*> \\par Further Details:\nCOMMENT\t*  =====================\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*>  Level 2 Blas routine.\nCOMMENT\t*>  The vector and matrix arguments are not referenced when N = 0, or M = 0\nCOMMENT\t*>\nCOMMENT\t*>  -- Written on 22-October-1986.\nCOMMENT\t*>     Jack Dongarra, Argonne National Lab.\nCOMMENT\t*>     Jeremy Du Croz, Nag Central Office.\nCOMMENT\t*>     Sven Hammarling, Nag Central Office.\nCOMMENT\t*>     Richard Hanson, Sandia National Labs.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*  =====================================================================\ntoken(96)\tSUBROUTINE\nIDENT\tCHPMV\n(\t(\nIDENT\tUPLO\n,\t,\nIDENT\tN\n,\t,\nIDENT\tALPHA\n,\t,\nIDENT\tAP\n,\t,\nIDENT\tX\n,\t,\nIDENT\tINCX\n,\t,\nIDENT\tBETA\n,\t,\nIDENT\tY\n,\t,\nIDENT\tINCY\n)\t)\nCOMMENT\t*\nCOMMENT\t*  -- Reference BLAS level2 routine (version 3.7.0) --\nCOMMENT\t*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --\nCOMMENT\t*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--\nCOMMENT\t*     December 2016\nCOMMENT\t*\nCOMMENT\t*     .. Scalar Arguments ..\nIDENT\tCOMPLEX\nIDENT\tALPHA\n,\t,\nIDENT\tBETA\nIDENT\tINTEGER\nIDENT\tINCX\n,\t,\nIDENT\tINCY\n,\t,\nIDENT\tN\nIDENT\tCHARACTER\nIDENT\tUPLO\nCOMMENT\t*     ..\nCOMMENT\t*     .. Array Arguments ..\nIDENT\tCOMPLEX\nIDENT\tAP\n(\t(\n*\t*)\n,\t,\nIDENT\tX\n(\t(\n*\t*)\n,\t,\nIDENT\tY\n(\t(\n*\t*)\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*  =====================================================================\nCOMMENT\t*\nCOMMENT\t*     .. Parameters ..\nIDENT\tCOMPLEX\nIDENT\tONE\nIDENT\tPARAMETER\n(\t(\nIDENT\tONE\n=\t=\n(\t(\nIDENT\t1\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n,\t,\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n)\t)\n)\t)\nIDENT\tCOMPLEX\nIDENT\tZERO\nIDENT\tPARAMETER\n(\t(\nIDENT\tZERO\n=\t=\n(\t(\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n,\t,\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tE\n+\t+\nIDENT\t0\n)\t)\n)\t)\nCOMMENT\t*     ..\nCOMMENT\t*     .. Local Scalars ..\nIDENT\tCOMPLEX\nIDENT\tTEMP1\n,\t,\nIDENT\tTEMP2\nIDENT\tINTEGER\nIDENT\tI\n,\t,\nIDENT\tINFO\n,\t,\nIDENT\tIX\n,\t,\nIDENT\tIY\n,\t,\nIDENT\tJ\n,\t,\nIDENT\tJX\n,\t,\nIDENT\tJY\n,\t,\nIDENT\tK\n,\t,\nIDENT\tKK\n,\t,\nIDENT\tKX\n,\t,\nIDENT\tKY\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Functions ..\nIDENT\tLOGICAL\nIDENT\tLSAME\nIDENT\tEXTERNAL\nIDENT\tLSAME\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Subroutines ..\nIDENT\tEXTERNAL\nIDENT\tXERBLA\nCOMMENT\t*     ..\nCOMMENT\t*     .. Intrinsic Functions ..\nIDENT\tINTRINSIC\nIDENT\tCONJG\n,\t,\nIDENT\tREAL\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*     Test the input parameters.\nCOMMENT\t*\nIDENT\tINFO\n=\t=\nIDENT\t0\nIDENT\tIF\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'U'\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'L'\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tN\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t2\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t6\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t9\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINFO\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tCALL\nIDENT\tXERBLA\n(\t(\nSTRING\t'CHPMV '\n,\t,\nIDENT\tINFO\n)\t)\nIDENT\tRETURN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Quick return if possible.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tN\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tONE\n)\t)\n)\t)\n)\t)\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     Set up the start points in  X  and  Y.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tGT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tKX\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tKX\n=\t=\nIDENT\t1\n-\t-\n(\t(\nIDENT\tN\n-\t-\nIDENT\t1\n)\t)\n*\t*I\nIDENT\tNCX\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tGT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tKY\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tKY\n=\t=\nIDENT\t1\n-\t-\n(\t(\nIDENT\tN\n-\t-\nIDENT\t1\n)\t)\n*\t*I\nIDENT\tNCY\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Start the operations. In this version the elements of the array AP\nCOMMENT\t*     are accessed sequentially with one pass through AP.\nCOMMENT\t*\nCOMMENT\t*     First form  y := beta*y.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tONE\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t10\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\t10\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t20\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n=\t=\nIDENT\tBETA\n*\t*Y\n(\t(\nIDENT\tI\n)\t)\nIDENT\t20\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nIDENT\tIY\n=\t=\nIDENT\tKY\nIDENT\tIF\n(\t(\nIDENT\tBETA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t30\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n=\t=\nIDENT\tZERO\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\t30\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t40\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n=\t=\nIDENT\tBETA\n*\t*Y\n(\t(\nIDENT\tIY\n)\t)\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\t40\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\nIDENT\tRETURN\nIDENT\tKK\n=\t=\nIDENT\t1\nIDENT\tIF\n(\t(\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'U'\n)\t)\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*        Form  y  when AP contains the upper triangle.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t60\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJ\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tZERO\nIDENT\tK\n=\t=\nIDENT\tKK\nIDENT\tDO\nIDENT\t50\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tJ\n-\t-\nIDENT\t1\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*A\nIDENT\tP\n(\t(\nIDENT\tK\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tTEMP2\n+\t+\nIDENT\tCONJG\n(\t(\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n)\t)\n*\t*X\n(\t(\nIDENT\tI\n)\t)\nIDENT\tK\n=\t=\nIDENT\tK\n+\t+\nIDENT\t1\nIDENT\t50\nIDENT\tCONTINUE\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*R\nIDENT\tEAL\n(\t(\nIDENT\tAP\n(\t(\nIDENT\tKK\n+\t+\nIDENT\tJ\n-\t-\nIDENT\t1\n)\t)\n)\t)\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP2\nIDENT\tKK\n=\t=\nIDENT\tKK\n+\t+\nIDENT\tJ\nIDENT\t60\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tJX\n=\t=\nIDENT\tKX\nIDENT\tJY\n=\t=\nIDENT\tKY\nIDENT\tDO\nIDENT\t80\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJX\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tZERO\nIDENT\tIX\n=\t=\nIDENT\tKX\nIDENT\tIY\n=\t=\nIDENT\tKY\nIDENT\tDO\nIDENT\t70\nIDENT\tK\n=\t=\nIDENT\tKK\n,\t,\nIDENT\tKK\n+\t+\nIDENT\tJ\n-\t-\nIDENT\t2\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*A\nIDENT\tP\n(\t(\nIDENT\tK\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tTEMP2\n+\t+\nIDENT\tCONJG\n(\t(\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n)\t)\n*\t*X\n(\t(\nIDENT\tIX\n)\t)\nIDENT\tIX\n=\t=\nIDENT\tIX\n+\t+\nIDENT\tINCX\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\t70\nIDENT\tCONTINUE\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*R\nIDENT\tEAL\n(\t(\nIDENT\tAP\n(\t(\nIDENT\tKK\n+\t+\nIDENT\tJ\n-\t-\nIDENT\t1\n)\t)\n)\t)\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP2\nIDENT\tJX\n=\t=\nIDENT\tJX\n+\t+\nIDENT\tINCX\nIDENT\tJY\n=\t=\nIDENT\tJY\n+\t+\nIDENT\tINCY\nIDENT\tKK\n=\t=\nIDENT\tKK\n+\t+\nIDENT\tJ\nIDENT\t80\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*        Form  y  when AP contains the lower triangle.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t100\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJ\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tZERO\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*R\nIDENT\tEAL\n(\t(\nIDENT\tAP\n(\t(\nIDENT\tKK\n)\t)\n)\t)\nIDENT\tK\n=\t=\nIDENT\tKK\n+\t+\nIDENT\t1\nIDENT\tDO\nIDENT\t90\nIDENT\tI\n=\t=\nIDENT\tJ\n+\t+\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*A\nIDENT\tP\n(\t(\nIDENT\tK\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tTEMP2\n+\t+\nIDENT\tCONJG\n(\t(\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n)\t)\n*\t*X\n(\t(\nIDENT\tI\n)\t)\nIDENT\tK\n=\t=\nIDENT\tK\n+\t+\nIDENT\t1\nIDENT\t90\nIDENT\tCONTINUE\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP2\nIDENT\tKK\n=\t=\nIDENT\tKK\n+\t+\n(\t(\nIDENT\tN\n-\t-\nIDENT\tJ\n+\t+\nIDENT\t1\n)\t)\nIDENT\t100\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tJX\n=\t=\nIDENT\tKX\nIDENT\tJY\n=\t=\nIDENT\tKY\nIDENT\tDO\nIDENT\t120\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJX\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tZERO\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*R\nIDENT\tEAL\n(\t(\nIDENT\tAP\n(\t(\nIDENT\tKK\n)\t)\n)\t)\nIDENT\tIX\n=\t=\nIDENT\tJX\nIDENT\tIY\n=\t=\nIDENT\tJY\nIDENT\tDO\nIDENT\t110\nIDENT\tK\n=\t=\nIDENT\tKK\n+\t+\nIDENT\t1\n,\t,\nIDENT\tKK\n+\t+\nIDENT\tN\n-\t-\nIDENT\tJ\nIDENT\tIX\n=\t=\nIDENT\tIX\n+\t+\nIDENT\tINCX\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n+\t+\nIDENT\tTEMP1\n*\t*A\nIDENT\tP\n(\t(\nIDENT\tK\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tTEMP2\n+\t+\nIDENT\tCONJG\n(\t(\nIDENT\tAP\n(\t(\nIDENT\tK\n)\t)\n)\t)\n*\t*X\n(\t(\nIDENT\tIX\n)\t)\nIDENT\t110\nIDENT\tCONTINUE\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n=\t=\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n+\t+\nIDENT\tALPHA\n*\t*T\nIDENT\tEMP2\nIDENT\tJX\n=\t=\nIDENT\tJX\n+\t+\nIDENT\tINCX\nIDENT\tJY\n=\t=\nIDENT\tJY\n+\t+\nIDENT\tINCY\nIDENT\tKK\n=\t=\nIDENT\tKK\n+\t+\n(\t(\nIDENT\tN\n-\t-\nIDENT\tJ\n+\t+\nIDENT\t1\n)\t)\nIDENT\t120\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     End of CHPMV .\nCOMMENT\t*\nIDENT\tEND\n"
