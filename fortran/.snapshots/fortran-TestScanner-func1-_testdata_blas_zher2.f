(string) (len=14171) "COMMENT\t> \\brief \\b ZHER2\nCOMMENT\t*\nCOMMENT\t*  =========== DOCUMENTATION ===========\nCOMMENT\t*\nCOMMENT\t* Online html documentation available at\nCOMMENT\t*            http://www.netlib.org/lapack/explore-html/\nCOMMENT\t*\nCOMMENT\t*  Definition:\nCOMMENT\t*  ===========\nCOMMENT\t*\nCOMMENT\t*       SUBROUTINE ZHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)\nCOMMENT\t*\nCOMMENT\t*       .. Scalar Arguments ..\nCOMMENT\t*       COMPLEX*16 ALPHA\nCOMMENT\t*       INTEGER INCX,INCY,LDA,N\nCOMMENT\t*       CHARACTER UPLO\nCOMMENT\t*       ..\nCOMMENT\t*       .. Array Arguments ..\nCOMMENT\t*       COMPLEX*16 A(LDA,*),X(*),Y(*)\nCOMMENT\t*       ..\nCOMMENT\t*\nCOMMENT\t*\nCOMMENT\t*> \\par Purpose:\nCOMMENT\t*  =============\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*> ZHER2  performs the hermitian rank 2 operation\nCOMMENT\t*>\nCOMMENT\t*>    A := alpha*x*y**H + conjg( alpha )*y*x**H + A,\nCOMMENT\t*>\nCOMMENT\t*> where alpha is a scalar, x and y are n element vectors and A is an n\nCOMMENT\t*> by n hermitian matrix.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Arguments:\nCOMMENT\t*  ==========\nCOMMENT\t*\nCOMMENT\t*> \\param[in] UPLO\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          UPLO is CHARACTER*1\nCOMMENT\t*>           On entry, UPLO specifies whether the upper or lower\nCOMMENT\t*>           triangular part of the array A is to be referenced as\nCOMMENT\t*>           follows:\nCOMMENT\t*>\nCOMMENT\t*>              UPLO = 'U' or 'u'   Only the upper triangular part of A\nCOMMENT\t*>                                  is to be referenced.\nCOMMENT\t*>\nCOMMENT\t*>              UPLO = 'L' or 'l'   Only the lower triangular part of A\nCOMMENT\t*>                                  is to be referenced.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] N\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          N is INTEGER\nCOMMENT\t*>           On entry, N specifies the order of the matrix A.\nCOMMENT\t*>           N must be at least zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] ALPHA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          ALPHA is COMPLEX*16\nCOMMENT\t*>           On entry, ALPHA specifies the scalar alpha.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] X\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          X is COMPLEX*16 array, dimension at least\nCOMMENT\t*>           ( 1 + ( n - 1 )*abs( INCX ) ).\nCOMMENT\t*>           Before entry, the incremented array X must contain the n\nCOMMENT\t*>           element vector x.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] INCX\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          INCX is INTEGER\nCOMMENT\t*>           On entry, INCX specifies the increment for the elements of\nCOMMENT\t*>           X. INCX must not be zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] Y\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          Y is COMPLEX*16 array, dimension at least\nCOMMENT\t*>           ( 1 + ( n - 1 )*abs( INCY ) ).\nCOMMENT\t*>           Before entry, the incremented array Y must contain the n\nCOMMENT\t*>           element vector y.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] INCY\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          INCY is INTEGER\nCOMMENT\t*>           On entry, INCY specifies the increment for the elements of\nCOMMENT\t*>           Y. INCY must not be zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in,out] A\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          A is COMPLEX*16 array, dimension ( LDA, N )\nCOMMENT\t*>           Before entry with  UPLO = 'U' or 'u', the leading n by n\nCOMMENT\t*>           upper triangular part of the array A must contain the upper\nCOMMENT\t*>           triangular part of the hermitian matrix and the strictly\nCOMMENT\t*>           lower triangular part of A is not referenced. On exit, the\nCOMMENT\t*>           upper triangular part of the array A is overwritten by the\nCOMMENT\t*>           upper triangular part of the updated matrix.\nCOMMENT\t*>           Before entry with UPLO = 'L' or 'l', the leading n by n\nCOMMENT\t*>           lower triangular part of the array A must contain the lower\nCOMMENT\t*>           triangular part of the hermitian matrix and the strictly\nCOMMENT\t*>           upper triangular part of A is not referenced. On exit, the\nCOMMENT\t*>           lower triangular part of the array A is overwritten by the\nCOMMENT\t*>           lower triangular part of the updated matrix.\nCOMMENT\t*>           Note that the imaginary parts of the diagonal elements need\nCOMMENT\t*>           not be set, they are assumed to be zero, and on exit they\nCOMMENT\t*>           are set to zero.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] LDA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          LDA is INTEGER\nCOMMENT\t*>           On entry, LDA specifies the first dimension of A as declared\nCOMMENT\t*>           in the calling (sub) program. LDA must be at least\nCOMMENT\t*>           max( 1, n ).\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Authors:\nCOMMENT\t*  ========\nCOMMENT\t*\nCOMMENT\t*> \\author Univ. of Tennessee\nCOMMENT\t*> \\author Univ. of California Berkeley\nCOMMENT\t*> \\author Univ. of Colorado Denver\nCOMMENT\t*> \\author NAG Ltd.\nCOMMENT\t*\nCOMMENT\t*> \\date December 2016\nCOMMENT\t*\nCOMMENT\t*> \\ingroup complex16_blas_level2\nCOMMENT\t*\nCOMMENT\t*> \\par Further Details:\nCOMMENT\t*  =====================\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*>  Level 2 Blas routine.\nCOMMENT\t*>\nCOMMENT\t*>  -- Written on 22-October-1986.\nCOMMENT\t*>     Jack Dongarra, Argonne National Lab.\nCOMMENT\t*>     Jeremy Du Croz, Nag Central Office.\nCOMMENT\t*>     Sven Hammarling, Nag Central Office.\nCOMMENT\t*>     Richard Hanson, Sandia National Labs.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*  =====================================================================\ntoken(96)\tSUBROUTINE\nIDENT\tZHER2\n(\t(\nIDENT\tUPLO\n,\t,\nIDENT\tN\n,\t,\nIDENT\tALPHA\n,\t,\nIDENT\tX\n,\t,\nIDENT\tINCX\n,\t,\nIDENT\tY\n,\t,\nIDENT\tINCY\n,\t,\nIDENT\tA\n,\t,\nIDENT\tLDA\n)\t)\nCOMMENT\t*\nCOMMENT\t*  -- Reference BLAS level2 routine (version 3.7.0) --\nCOMMENT\t*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --\nCOMMENT\t*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--\nCOMMENT\t*     December 2016\nCOMMENT\t*\nCOMMENT\t*     .. Scalar Arguments ..\nIDENT\tCOMPLEX\n*\t*1\nIDENT\t6\nIDENT\tALPHA\nIDENT\tINTEGER\nIDENT\tINCX\n,\t,\nIDENT\tINCY\n,\t,\nIDENT\tLDA\n,\t,\nIDENT\tN\nIDENT\tCHARACTER\nIDENT\tUPLO\nCOMMENT\t*     ..\nCOMMENT\t*     .. Array Arguments ..\nIDENT\tCOMPLEX\n*\t*1\nIDENT\t6\nIDENT\tA\n(\t(\nIDENT\tLDA\n,\t,\n*\t*)\n,\t,\nIDENT\tX\n(\t(\n*\t*)\n,\t,\nIDENT\tY\n(\t(\n*\t*)\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*  =====================================================================\nCOMMENT\t*\nCOMMENT\t*     .. Parameters ..\nIDENT\tCOMPLEX\n*\t*1\nIDENT\t6\nIDENT\tZERO\nIDENT\tPARAMETER\n(\t(\nIDENT\tZERO\n=\t=\n(\t(\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tD\n+\t+\nIDENT\t0\n,\t,\nIDENT\t0\n.\t.\nIDENT\t0\nIDENT\tD\n+\t+\nIDENT\t0\n)\t)\n)\t)\nCOMMENT\t*     ..\nCOMMENT\t*     .. Local Scalars ..\nIDENT\tCOMPLEX\n*\t*1\nIDENT\t6\nIDENT\tTEMP1\n,\t,\nIDENT\tTEMP2\nIDENT\tINTEGER\nIDENT\tI\n,\t,\nIDENT\tINFO\n,\t,\nIDENT\tIX\n,\t,\nIDENT\tIY\n,\t,\nIDENT\tJ\n,\t,\nIDENT\tJX\n,\t,\nIDENT\tJY\n,\t,\nIDENT\tKX\n,\t,\nIDENT\tKY\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Functions ..\nIDENT\tLOGICAL\nIDENT\tLSAME\nIDENT\tEXTERNAL\nIDENT\tLSAME\nCOMMENT\t*     ..\nCOMMENT\t*     .. External Subroutines ..\nIDENT\tEXTERNAL\nIDENT\tXERBLA\nCOMMENT\t*     ..\nCOMMENT\t*     .. Intrinsic Functions ..\nIDENT\tINTRINSIC\nIDENT\tDBLE\n,\t,\nIDENT\tDCONJG\n,\t,\nIDENT\tMAX\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*     Test the input parameters.\nCOMMENT\t*\nIDENT\tINFO\n=\t=\nIDENT\t0\nIDENT\tIF\n(\t(\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'U'\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n.\t.\nIDENT\tNOT\n.\t.\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'L'\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tN\n.\t.\nIDENT\tLT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t2\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t5\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t7\nIDENT\tELSE\nIDENT\tIF\n(\t(\nIDENT\tLDA\n.\t.\nIDENT\tLT\n.\t.\nIDENT\tMAX\n(\t(\nIDENT\t1\n,\t,\nIDENT\tN\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tINFO\n=\t=\nIDENT\t9\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINFO\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tCALL\nIDENT\tXERBLA\n(\t(\nSTRING\t'ZHER2 '\n,\t,\nIDENT\tINFO\n)\t)\nIDENT\tRETURN\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Quick return if possible.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tN\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tALPHA\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\tZERO\n)\t)\n)\t)\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     Set up the start points in X and Y if the increments are not both\nCOMMENT\t*     unity.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t1\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tNE\n.\t.\nIDENT\t1\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tIF\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tGT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tKX\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tKX\n=\t=\nIDENT\t1\n-\t-\n(\t(\nIDENT\tN\n-\t-\nIDENT\t1\n)\t)\n*\t*I\nIDENT\tNCX\nIDENT\tEND\nIDENT\tIF\nIDENT\tIF\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tGT\n.\t.\nIDENT\t0\n)\t)\nIDENT\tTHEN\nIDENT\tKY\n=\t=\nIDENT\t1\nIDENT\tELSE\nIDENT\tKY\n=\t=\nIDENT\t1\n-\t-\n(\t(\nIDENT\tN\n-\t-\nIDENT\t1\n)\t)\n*\t*I\nIDENT\tNCY\nIDENT\tEND\nIDENT\tIF\nIDENT\tJX\n=\t=\nIDENT\tKX\nIDENT\tJY\n=\t=\nIDENT\tKY\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nCOMMENT\t*     Start the operations. In this version the elements of A are\nCOMMENT\t*     accessed sequentially with one pass through the triangular part\nCOMMENT\t*     of A.\nCOMMENT\t*\nIDENT\tIF\n(\t(\nIDENT\tLSAME\n(\t(\nIDENT\tUPLO\n,\t,\nSTRING\t'U'\n)\t)\n)\t)\nIDENT\tTHEN\nCOMMENT\t*\nCOMMENT\t*        Form  A  when A is stored in the upper triangle.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t20\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJ\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*D\nIDENT\tCONJG\n(\t(\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tDCONJG\n(\t(\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJ\n)\t)\n)\t)\nIDENT\tDO\nIDENT\t10\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tJ\n-\t-\nIDENT\t1\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tX\n(\t(\nIDENT\tI\n)\t)\n*\t*T\nIDENT\tEMP1\n+\t+\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n*\t*T\nIDENT\tEMP2\nIDENT\t10\nIDENT\tCONTINUE\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tDBLE\n(\t(\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n)\t)\n+\t+\n+\t+\nIDENT\tDBLE\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJ\n)\t)\n*\t*T\nIDENT\tEMP1\n+\t+\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n*\t*T\nIDENT\tEMP2\n)\t)\nIDENT\tELSE\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tDBLE\n(\t(\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\t20\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t40\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJX\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*D\nIDENT\tCONJG\n(\t(\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tDCONJG\n(\t(\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJX\n)\t)\n)\t)\nIDENT\tIX\n=\t=\nIDENT\tKX\nIDENT\tIY\n=\t=\nIDENT\tKY\nIDENT\tDO\nIDENT\t30\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tJ\n-\t-\nIDENT\t1\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tX\n(\t(\nIDENT\tIX\n)\t)\n*\t*T\nIDENT\tEMP1\n+\t+\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n*\t*T\nIDENT\tEMP2\nIDENT\tIX\n=\t=\nIDENT\tIX\n+\t+\nIDENT\tINCX\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\t30\nIDENT\tCONTINUE\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tDBLE\n(\t(\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n)\t)\n+\t+\n+\t+\nIDENT\tDBLE\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJX\n)\t)\n*\t*T\nIDENT\tEMP1\n+\t+\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n*\t*T\nIDENT\tEMP2\n)\t)\nIDENT\tELSE\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tDBLE\n(\t(\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\tJX\n=\t=\nIDENT\tJX\n+\t+\nIDENT\tINCX\nIDENT\tJY\n=\t=\nIDENT\tJY\n+\t+\nIDENT\tINCY\nIDENT\t40\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tELSE\nCOMMENT\t*\nCOMMENT\t*        Form  A  when A is stored in the lower triangle.\nCOMMENT\t*\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tINCX\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\n.\t.\nIDENT\tAND\n.\t.\n(\t(\nIDENT\tINCY\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t1\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tDO\nIDENT\t60\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJ\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*D\nIDENT\tCONJG\n(\t(\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tDCONJG\n(\t(\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJ\n)\t)\n)\t)\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tDBLE\n(\t(\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n)\t)\n+\t+\n+\t+\nIDENT\tDBLE\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJ\n)\t)\n*\t*T\nIDENT\tEMP1\n+\t+\nIDENT\tY\n(\t(\nIDENT\tJ\n)\t)\n*\t*T\nIDENT\tEMP2\n)\t)\nIDENT\tDO\nIDENT\t50\nIDENT\tI\n=\t=\nIDENT\tJ\n+\t+\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tX\n(\t(\nIDENT\tI\n)\t)\n*\t*T\nIDENT\tEMP1\n+\t+\nIDENT\tY\n(\t(\nIDENT\tI\n)\t)\n*\t*T\nIDENT\tEMP2\nIDENT\t50\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tDBLE\n(\t(\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\t60\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tDO\nIDENT\t80\nIDENT\tJ\n=\t=\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIF\n(\t(\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJX\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\n.\t.\nIDENT\tOR\n.\t.\n(\t(\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n.\t.\nIDENT\tNE\n.\t.\nIDENT\tZERO\n)\t)\n)\t)\nIDENT\tTHEN\nIDENT\tTEMP1\n=\t=\nIDENT\tALPHA\n*\t*D\nIDENT\tCONJG\n(\t(\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n)\t)\nIDENT\tTEMP2\n=\t=\nIDENT\tDCONJG\n(\t(\nIDENT\tALPHA\n*\t*X\n(\t(\nIDENT\tJX\n)\t)\n)\t)\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tDBLE\n(\t(\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n)\t)\n+\t+\n+\t+\nIDENT\tDBLE\n(\t(\nIDENT\tX\n(\t(\nIDENT\tJX\n)\t)\n*\t*T\nIDENT\tEMP1\n+\t+\nIDENT\tY\n(\t(\nIDENT\tJY\n)\t)\n*\t*T\nIDENT\tEMP2\n)\t)\nIDENT\tIX\n=\t=\nIDENT\tJX\nIDENT\tIY\n=\t=\nIDENT\tJY\nIDENT\tDO\nIDENT\t70\nIDENT\tI\n=\t=\nIDENT\tJ\n+\t+\nIDENT\t1\n,\t,\nIDENT\tN\nIDENT\tIX\n=\t=\nIDENT\tIX\n+\t+\nIDENT\tINCX\nIDENT\tIY\n=\t=\nIDENT\tIY\n+\t+\nIDENT\tINCY\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tA\n(\t(\nIDENT\tI\n,\t,\nIDENT\tJ\n)\t)\n+\t+\nIDENT\tX\n(\t(\nIDENT\tIX\n)\t)\n*\t*T\nIDENT\tEMP1\n+\t+\nIDENT\tY\n(\t(\nIDENT\tIY\n)\t)\n*\t*T\nIDENT\tEMP2\nIDENT\t70\nIDENT\tCONTINUE\nIDENT\tELSE\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n=\t=\nIDENT\tDBLE\n(\t(\nIDENT\tA\n(\t(\nIDENT\tJ\n,\t,\nIDENT\tJ\n)\t)\n)\t)\nIDENT\tEND\nIDENT\tIF\nIDENT\tJX\n=\t=\nIDENT\tJX\n+\t+\nIDENT\tINCX\nIDENT\tJY\n=\t=\nIDENT\tJY\n+\t+\nIDENT\tINCY\nIDENT\t80\nIDENT\tCONTINUE\nIDENT\tEND\nIDENT\tIF\nIDENT\tEND\nIDENT\tIF\nCOMMENT\t*\nIDENT\tRETURN\nCOMMENT\t*\nCOMMENT\t*     End of ZHER2 .\nCOMMENT\t*\nIDENT\tEND\n"
