      character c(10,10)
      data c /'1'/
      call suba(c)
      end
      subroutine suba(c)
      character*2 c(100)
      print *,c
      end
