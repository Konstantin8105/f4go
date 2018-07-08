       FUNCTION func_name(a, b)
          INTEGER :: func_name
          INTEGER :: a
          REAL    :: b
          func_name = (2*a)+b
          RETURN
       END FUNCTION
   
       PROGRAM cows
          IMPLICIT NONE
          INTEGER :: func_name
          PRINT *,func_name(2, 1.3)
       END PROGRAM
