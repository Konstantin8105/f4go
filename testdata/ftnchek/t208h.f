      subroutine foo(x)
      real r1mach
*     ftnchek types sqrt() as generic here; I think it should be REAL.
*     This results in loss of a declaration "REAL SQRT"
      x = sqrt(r1mach(3))
      end
