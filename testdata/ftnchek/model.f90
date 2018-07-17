!       AUTHORS: MIKE MYERS AND LUCIA SPAGNUOLO
!       DATE:    MAY 8, 1989

!       Variables:
!               SCORE -> an array of test scores
!               SUM ->   sum of the test scores
!               COUNT -> counter of scores read in
!               I ->     loop counter

REAL FUNCTION COMPAV(SCORE,COUNT)
  INTEGER SUM,COUNT,J,SCORE(5)

  DO I = 1,COUNT
     SUM = SUM + SCORE(I)
  END DO
  COMPAV = SUM/COUNT
END FUNCTION COMPAV


PROGRAM AVENUM
!
!                       MAIN PROGRAM
!
!       AUTHOR:   LOIS BIGBIE
!       DATE:     MAY 15, 1990
!
!       Variables:
!               MAXNOS -> maximum number of input values
!               NUMS    -> an array of numbers
!               COUNT   -> exact number of input values
!               AVG     -> average returned by COMPAV
!               I       -> loop counter
!

  PARAMETER(MAXNOS=5)
  INTEGER I, COUNT
  REAL NUMS(MAXNOS), AVG
  COUNT = 0
  DO I = 1,MAXNOS
     READ (5,*,END=100) NUMS(I) ; COUNT = COUNT + 1

  END DO
100 AVG = COMPAV(NUMS, COUNT)
END PROGRAM AVENUM
