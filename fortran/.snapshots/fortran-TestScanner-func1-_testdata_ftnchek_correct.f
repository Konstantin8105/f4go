(string) (len=1642) "COMMENT\t\tAUTHORS: MIKE MYERS & LUCIA SPAGNUOLO\nCOMMENT\tC\tDATE:    MAY 8, 1989\nCOMMENT\tC\tVariables:\nCOMMENT\tC\t\tSCORE -> an array of test scores\nCOMMENT\tC\t\tSUM ->   sum of the test scores\nCOMMENT\tC\t\tCOUNT -> counter of scores read in\nCOMMENT\tC\t\tI ->     loop counter              \nIDENT\tREAL\nIDENT\tFUNCTION\nIDENT\tCOMPAV\n(\t(\nIDENT\tSCORE\n,\t,\nIDENT\tCOUNT\n)\t)\nIDENT\tINTEGER\nIDENT\tSUM\n,\t,\nIDENT\tCOUNT\n,\t,\nIDENT\tI\n,\t,\nIDENT\tSCORE\n(\t(\nIDENT\t5\n)\t)\nIDENT\tSUM\n=\t=\nIDENT\t0\nIDENT\tDO\nIDENT\t30\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tCOUNT\nIDENT\tSUM\n=\t=\nIDENT\tSUM\n+\t+\nIDENT\tSCORE\n(\t(\nIDENT\tI\n)\t)\nIDENT\t30\nIDENT\tCONTINUE\nIDENT\tCOMPAV\n=\t=\nIDENT\tFLOAT\n(\t(\nIDENT\tSUM\n)\t)\n/\t/\nIDENT\tFLOAT\n(\t(\nIDENT\tCOUNT\n)\t)\nIDENT\tEND\nIDENT\tPROGRAM\nIDENT\tAVENUM\nCOMMENT\tC\nCOMMENT\tC\t\t\tMAIN PROGRAM\nCOMMENT\tC\nCOMMENT\tC\tAUTHOR:   LOIS BIGBIE\nCOMMENT\tC\tDATE:\t  MAY 15, 1990\nCOMMENT\tC\nCOMMENT\tC\tVariables:\nCOMMENT\tC\t\tMAXNOS -> maximum number of input values\nCOMMENT\tC\t\tNUMS    -> an array of numbers\nCOMMENT\tC\t\tCOUNT   -> exact number of input values\nCOMMENT\tC\t\tAVG     -> average returned by COMPAV\nCOMMENT\tC\t\tI       -> loop counter\nCOMMENT\tC\t\nIDENT\tINTEGER\nIDENT\tMAXNOS\nIDENT\tPARAMETER\n(\t(\nIDENT\tMAXNOS\n=\t=\nIDENT\t5\n)\t)\nIDENT\tINTEGER\nIDENT\tI\n,\t,\nIDENT\tNUMS\n(\t(\nIDENT\tMAXNOS\n)\t)\n,\t,\nIDENT\tCOUNT\nIDENT\tREAL\nIDENT\tAVG\nIDENT\tCOUNT\n=\t=\nIDENT\t0\nIDENT\tDO\nIDENT\t80\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tMAXNOS\nIDENT\tREAD\n(\t(\nIDENT\t5\n,\t,\n*\t*,\nIDENT\tEND\n=\t=\nIDENT\t100\n)\t)\nIDENT\tNUMS\n(\t(\nIDENT\tI\n)\t)\nIDENT\tCOUNT\n=\t=\nIDENT\tCOUNT\n+\t+\nIDENT\t1\nIDENT\t80\nIDENT\tCONTINUE\nIDENT\t100\nIDENT\tAVG\n=\t=\nIDENT\tCOMPAV\n(\t(\nIDENT\tNUMS\n,\t,\nIDENT\tCOUNT\n)\t)\nIDENT\tWRITE\n(\t(\nIDENT\t6\n,\t,\n*\t*)\nSTRING\t'AVERAGE ='\n,\t,\nIDENT\tAVG\nIDENT\tEND\n"
