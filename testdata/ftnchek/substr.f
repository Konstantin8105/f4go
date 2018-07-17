C This program tickled a bug in gcc under DEC Unix for Alpha
       PROGRAM X
       CHARACTER*80 LINE
       INTEGER LSTART, LEND
       PARAMETER (LSTART=2,LEND=11)
       READ (5,'(A)') LINE(1:9)
       PRINT *,LINE(9:10)
       READ (5,'(A)') LINE(LSTART:LEND)
       PRINT *,LINE(2:LEND)
       END
