      character c(10,10)
      data c /'1'/
      call suba(c)
      end
      subroutine suba(c)
      character*(*) c(2000)
      print *,c
      end
