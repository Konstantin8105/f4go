c-----------------------------------------------------------------------
c     ftnchek test file: t208w.f, Mon Mar 13 14:01:03 1995
c-----------------------------------------------------------------------

      double precision function s(x)
      intrinsic dsin
      external extsub
      double precision x, y

      integer a, b, c
      common /one/ a, b, c

      integer d, e, f
      common /two/ d, e, f

      integer g, h, i
      real u, v, w
      common /three/ g, h, i, u, v, w

      call subext(extsub, x)

      y = dfloat(a + b + c + d + e + f)

      s = dsqrt(x + y)

      end

c-----------------------------------------------------------------------

      parameter(l2ng=6,ng=2**l2ng,nb =ng+2,ngm1=ng-1)
      parameter(nbp1=nb+1)
c
      double precision f, p, d, w1, w2
      common/force /f(0:nbp1,0:nbp1,2)
      common/pres  /p(0:nbp1,0:nbp1)
c
      dimension d (0:nbp1,0:nbp1)
      dimension w1(0:nbp1,0:nbp1,1:2)
      dimension w2(0:nbp1,0:nbp1,1:2)
c
      equivalence(f,w1,w2)
      equivalence(d,p)
c
      call foo(f,p)
      end

c-----------------------------------------------------------------------

      double precision x, y 
      common /one/ x(10)
      equivalence (x,y)
      end

c-----------------------------------------------------------------------

      double precision x, y(1), z(2)
      common /one/ x(10)
      equivalence (x(3), y(1), z(2))
      call subrtn (x, y, z)
      end
