      integer c(10,10)
      data c /1/
      call suba(c)
      end
      subroutine suba(c)
      integer c(100)
      print *,c
      end
