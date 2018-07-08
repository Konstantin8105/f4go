      program som
      implicit none
c
      integer n, i, sum
      double precision r
c
      sum = 0
      n   = 5
 
      do i = 1, n
        sum = sum + i
        r = sum / n
        write (*,*) 'Sum = ', sum, ' and the average = ', r
      enddo
c   
      end
