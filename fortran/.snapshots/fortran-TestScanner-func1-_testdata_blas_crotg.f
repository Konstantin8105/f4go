(string) (len=3081) "COMMENT\t> \\brief \\b CROTG\nCOMMENT\t*\nCOMMENT\t*  =========== DOCUMENTATION ===========\nCOMMENT\t*\nCOMMENT\t* Online html documentation available at\nCOMMENT\t*            http://www.netlib.org/lapack/explore-html/\nCOMMENT\t*\nCOMMENT\t*  Definition:\nCOMMENT\t*  ===========\nCOMMENT\t*\nCOMMENT\t*       SUBROUTINE CROTG(CA,CB,C,S)\nCOMMENT\t*\nCOMMENT\t*       .. Scalar Arguments ..\nCOMMENT\t*       COMPLEX CA,CB,S\nCOMMENT\t*       REAL C\nCOMMENT\t*       ..\nCOMMENT\t*\nCOMMENT\t*\nCOMMENT\t*> \\par Purpose:\nCOMMENT\t*  =============\nCOMMENT\t*>\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>\nCOMMENT\t*> CROTG determines a complex Givens rotation.\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Arguments:\nCOMMENT\t*  ==========\nCOMMENT\t*\nCOMMENT\t*> \\param[in] CA\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          CA is COMPLEX\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[in] CB\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          CB is COMPLEX\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[out] C\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          C is REAL\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*>\nCOMMENT\t*> \\param[out] S\nCOMMENT\t*> \\verbatim\nCOMMENT\t*>          S is COMPLEX\nCOMMENT\t*> \\endverbatim\nCOMMENT\t*\nCOMMENT\t*  Authors:\nCOMMENT\t*  ========\nCOMMENT\t*\nCOMMENT\t*> \\author Univ. of Tennessee\nCOMMENT\t*> \\author Univ. of California Berkeley\nCOMMENT\t*> \\author Univ. of Colorado Denver\nCOMMENT\t*> \\author NAG Ltd.\nCOMMENT\t*\nCOMMENT\t*> \\date November 2017\nCOMMENT\t*\nCOMMENT\t*> \\ingroup complex_blas_level1\nCOMMENT\t*\nCOMMENT\t*  =====================================================================\ntoken(96)\tSUBROUTINE\nIDENT\tCROTG\n(\t(\nIDENT\tCA\n,\t,\nIDENT\tCB\n,\t,\nIDENT\tC\n,\t,\nIDENT\tS\n)\t)\nCOMMENT\t*\nCOMMENT\t*  -- Reference BLAS level1 routine (version 3.8.0) --\nCOMMENT\t*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --\nCOMMENT\t*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--\nCOMMENT\t*     November 2017\nCOMMENT\t*\nCOMMENT\t*     .. Scalar Arguments ..\nIDENT\tCOMPLEX\nIDENT\tCA\n,\t,\nIDENT\tCB\n,\t,\nIDENT\tS\nIDENT\tREAL\nIDENT\tC\nCOMMENT\t*     ..\nCOMMENT\t*\nCOMMENT\t*  =====================================================================\nCOMMENT\t*\nCOMMENT\t*     .. Local Scalars ..\nIDENT\tCOMPLEX\nIDENT\tALPHA\nIDENT\tREAL\nIDENT\tNORM\n,\t,\nIDENT\tSCALE\nCOMMENT\t*     ..\nCOMMENT\t*     .. Intrinsic Functions ..\nIDENT\tINTRINSIC\nIDENT\tCABS\n,\t,\nIDENT\tCONJG\n,\t,\nIDENT\tSQRT\nCOMMENT\t*     ..\nIDENT\tIF\n(\t(\nIDENT\tCABS\n(\t(\nIDENT\tCA\n)\t)\n.\t.\nIDENT\tEQ\n.\t.\nIDENT\t0\n.\t.\n)\t)\nIDENT\tTHEN\nIDENT\tC\n=\t=\nIDENT\t0\n.\t.\nIDENT\tS\n=\t=\n(\t(\nIDENT\t1\n.\t.\n,\t,\nIDENT\t0\n.\t.\n)\t)\nIDENT\tCA\n=\t=\nIDENT\tCB\nIDENT\tELSE\nIDENT\tSCALE\n=\t=\nIDENT\tCABS\n(\t(\nIDENT\tCA\n)\t)\n+\t+\nIDENT\tCABS\n(\t(\nIDENT\tCB\n)\t)\nIDENT\tNORM\n=\t=\nIDENT\tSCALE\n*\t*S\nIDENT\tQRT\n(\t(\n(\t(\nIDENT\tCABS\n(\t(\nIDENT\tCA\n/\t/\nIDENT\tSCALE\n)\t)\n)\t)\n*\t**\nIDENT\t2\n+\t+\n(\t(\nIDENT\tCABS\n(\t(\nIDENT\tCB\n/\t/\nIDENT\tSCALE\n)\t)\n)\t)\n*\t**\nIDENT\t2\n)\t)\nIDENT\tALPHA\n=\t=\nIDENT\tCA\n/\t/\nIDENT\tCABS\n(\t(\nIDENT\tCA\n)\t)\nIDENT\tC\n=\t=\nIDENT\tCABS\n(\t(\nIDENT\tCA\n)\t)\n/\t/\nIDENT\tNORM\nIDENT\tS\n=\t=\nIDENT\tALPHA\n*\t*C\nIDENT\tONJG\n(\t(\nIDENT\tCB\n)\t)\n/\t/\nIDENT\tNORM\nIDENT\tCA\n=\t=\nIDENT\tALPHA\n*\t*N\nIDENT\tORM\nIDENT\tEND\nIDENT\tIF\nIDENT\tRETURN\nIDENT\tEND\n"