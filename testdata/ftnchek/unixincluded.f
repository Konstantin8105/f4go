# 1 "unixinclude.f"
      integer n

# 1 "./unixdefs.h"
      integer x
      logical a
      common n
# 3 "unixinclude.f"
      call foo(x)
      a = x .eq. n
      end
      subroutine foo(m)
      common n
      n = 1
      m = 2
      end
