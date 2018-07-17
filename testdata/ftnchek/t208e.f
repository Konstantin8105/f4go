      DOUBLE PRECISION     D(10,20,30), E(40,50), F(60)
      REAL                 R, S(70,80)
      CHARACTER            C
      CHARACTER*81         LINE
      LOGICAL		   LOGVAR
      COMPLEX		   Z, ZZ
      DOUBLE COMPLEX	   ZZZ, ZZZZ
      INTEGER              I, J, K
      J = M
      END
      SUBROUTINE SUB 
      REAL X, Y, Z
      COMMON /SHARE/ A, B, C
      END
      REAL FUNCTION FUN
      REAL X, Y, Z      
      COMMON /SHARE/ A, B, C
      FUN = 0.0
      END
      SUBROUTINE BAR
      REAL PI
      PARAMETER (PI = 3.14159265)
      INTEGER NPI
      PARAMETER (NPI = 9)
      INTRINSIC SQRT, SIN, COS
      EXTERNAL MYSQRT, MYSUB
      REAL MYSQRT
      END

      
