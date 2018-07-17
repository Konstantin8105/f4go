C  Tests flagging of deleted features in Fortran 95
      integer i,j,label,format
      real x
      assign 5 to label
      assign 90 to format
      j = 1
 5    continue
      do 10 x=1,5
         print format,x,j*x**2
 10   continue
      j = j+1
      if( j .eq. 2 ) goto label
      i = j*2
      assign 91 to format
      write(*,format) i,j
      write(*,91) i,j
 90   format(1x,2f6.0)
 91   format(1x,2hi=,i5,3h j=,i5)
      end

