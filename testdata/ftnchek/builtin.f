C checks makedcl generation of "Built-in" section

      double precision dsqrt, x
      intrinsic sqrt, dsqrt
      x = dsqrt(2.0d0)
      y = sqrt(2.0)
      print *, x, y
      end
