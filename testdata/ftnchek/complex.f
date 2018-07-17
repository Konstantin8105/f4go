      complex cc,cd
      parameter (cc=(1.2,3.4))
      data cd,de / ( +23e5 , 0 ), (-5 ,  +77)/
      a = (12,32)
      a = (5,-3e5)
      write(11,7) a
 7    format(1x,2f10.2)
      print 22, (1,2) ,a,(1,2,i=1,2)
 22   format(1x,2e10.3,2f10.2,4i4)
      call abc(123,(456,789))
      end
      subroutine abc(x,z)
      integer x
      complex z
      print *,z+x
      end
