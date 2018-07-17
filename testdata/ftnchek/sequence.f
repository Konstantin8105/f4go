C tests message about statement out of order.
      data x /1.0/
      integer a,b,c
      common a,b,c
      sqr(y)=y*y
      double precision d
      read(*,*) a,b,c,d
      write(*,*) a,b,c,d,sqr(x)
      end
