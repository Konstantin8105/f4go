      double precision function f(a,b)
      double precision a,b
      f = a+b
      return
      end
      program main
      real*8 f
      real*8 x,y
      x = 1.0
      y = 2.0d0
      write(*,*) f(x,y)
      end

