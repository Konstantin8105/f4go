C  Test of parsing and checking quad precision variables & constants.
C  No intrinsic functions at this time.
      real r
      double precision d
      real*8 r8
      real*16 q
      complex*32 cq
      q = 1.234Q5
      q = 1.234Q+05
      q = 1.234Q+345
      q = 1.234Q-345
      d = 1.234Q+05
      r = 1.234Q+05
      d = q
      q = d
      r = q
      q = r
      cq = q
      q = qsqrt(q)
      cq = cqsqrt(cq)
      r = qsqrt(q)
      q = qsqrt(r)
      q = qsqrt(d)
      r8 = qsqrt(q)
      q = qprod(d,r8)
      q = qmax1(1.234Q5,q,1.234Q-5)
      end
