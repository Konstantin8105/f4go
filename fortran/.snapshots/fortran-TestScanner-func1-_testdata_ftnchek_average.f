(string) (len=1649) "COMMENT\t       AUTHORS: MIKE MYERS AND LUCIA SPAGNUOLO\nCOMMENT\tC       DATE:    MAY 8, 1989\nCOMMENT\tC       Variables:\nCOMMENT\tC               SCORE -> an array of test scores\nCOMMENT\tC               SUM ->   sum of the test scores\nCOMMENT\tC               COUNT -> counter of scores read in\nCOMMENT\tC               I ->     loop counter\nIDENT\tREAL\nIDENT\tFUNCTION\nIDENT\tCOMPAV\n(\t(\nIDENT\tSCORE\n,\t,\nIDENT\tCOUNT\n)\t)\nIDENT\tINTEGER\nIDENT\tSUM\n,\t,\nIDENT\tCOUNT\n,\t,\nIDENT\tJ\n,\t,\nIDENT\tSCORE\n(\t(\nIDENT\t5\n)\t)\nIDENT\tDO\nIDENT\t30\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tCOUNT\nIDENT\tSUM\n=\t=\nIDENT\tSUM\n+\t+\nIDENT\tSCORE\n(\t(\nIDENT\tI\n)\t)\nIDENT\t30\nIDENT\tCONTINUE\nIDENT\tCOMPAV\n=\t=\nIDENT\tSUM\n/\t/\nIDENT\tCOUNT\nIDENT\tEND\nIDENT\tPROGRAM\nIDENT\tAVENUM\nCOMMENT\tC\nCOMMENT\tC                       MAIN PROGRAM\nCOMMENT\tC\nCOMMENT\tC       AUTHOR:   LOIS BIGBIE\nCOMMENT\tC       DATE:     MAY 15, 1990\nCOMMENT\tC\nCOMMENT\tC       Variables:\nCOMMENT\tC               MAXNOS -> maximum number of input values\nCOMMENT\tC               NUMS    -> an array of numbers\nCOMMENT\tC               COUNT   -> exact number of input values\nCOMMENT\tC               AVG     -> average returned by COMPAV\nCOMMENT\tC               I       -> loop counter\nCOMMENT\tC\nIDENT\tPARAMETER\n(\t(\nIDENT\tMAXNOS\n=\t=\nIDENT\t5\n)\t)\nIDENT\tINTEGER\nIDENT\tI\n,\t,\nIDENT\tCOUNT\nIDENT\tREAL\nIDENT\tNUMS\n(\t(\nIDENT\tMAXNOS\n)\t)\n,\t,\nIDENT\tAVG\nIDENT\tCOUNT\n=\t=\nIDENT\t0\nIDENT\tDO\nIDENT\t80\nIDENT\tI\n=\t=\nIDENT\t1\n,\t,\nIDENT\tMAXNOS\nIDENT\tREAD\n(\t(\nIDENT\t5\n,\t,\n*\t*,\nIDENT\tEND\n=\t=\nIDENT\t100\n)\t)\nIDENT\tNUMS\n(\t(\nIDENT\tI\n)\t)\nIDENT\tCOUNT\n=\t=\nIDENT\tCOUNT\n+\t+\nIDENT\t1\nIDENT\t80\nIDENT\tCONTINUE\nIDENT\t100\nIDENT\tAVG\n=\t=\nIDENT\tCOMPAV\n(\t(\nIDENT\tNUMS\n,\t,\nIDENT\tCOUNT\n)\t)\nIDENT\tEND\n"
