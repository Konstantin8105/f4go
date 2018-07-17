C	AUTHORS: MIKE MYERS & LUCIA SPAGNUOLO
C	DATE:    MAY 8, 1989
                   
C	Variables:
C		SCORE -> an array of test scores
C		SUM ->   sum of the test scores
C		COUNT -> counter of scores read in
C		I ->     loop counter              

	REAL FUNCTION COMPAV(SCORE,COUNT)
            INTEGER SUM,COUNT,I,SCORE(5)
    
	    SUM = 0
	    DO 30 I = 1,COUNT      
		SUM = SUM + SCORE(I)
30	    CONTINUE
	    COMPAV = FLOAT(SUM)/FLOAT(COUNT)
        END


	PROGRAM AVENUM
C
C			MAIN PROGRAM
C
C	AUTHOR:   LOIS BIGBIE
C	DATE:	  MAY 15, 1990
C
C	Variables:
C		MAXNOS -> maximum number of input values
C		NUMS    -> an array of numbers
C		COUNT   -> exact number of input values
C		AVG     -> average returned by COMPAV
C		I       -> loop counter
C	

	    INTEGER MAXNOS
	    PARAMETER(MAXNOS=5)
	    INTEGER I, NUMS(MAXNOS), COUNT
	    REAL AVG
	    COUNT = 0
	    DO 80 I = 1,MAXNOS
		READ (5,*,END=100) NUMS(I)
		COUNT = COUNT + 1
80	    CONTINUE
100	    AVG = COMPAV(NUMS, COUNT)
	    WRITE(6,*) 'AVERAGE =',AVG
	END
