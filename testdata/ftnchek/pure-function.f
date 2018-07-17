      real function notpure(a,b,c)
      real a,b,c
      common /shared/ x,y,z
      b = a + c
      z = x + y
      notpure = a*x + b*y + c*z
      end

      
