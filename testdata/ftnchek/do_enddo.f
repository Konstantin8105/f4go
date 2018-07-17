c  VAX-compatible fortran: underscores & do-enddo
      subroutine abc
      sum = 0.0
      do i=1,100
         sum = sum + i
      enddo
      print *,sum  !@#$*&
     # ,i
      other_sum = 1.0
      do while(other_sum .lt. 2000)
         other_sum = other_sum * 2.0
      end do
! here we have a nonstandard comment
      print *,other_sum
      dowhile ( x .ne. (1,2))
         x = 3.0
      end do
      do 100 while (.true.)
         read(*,*) sum
         if( sum .eq. 0.0 ) exit
 100  continue
      do 200, while (.false.)
         write(*,*) 'Cannot happen'
 200  end do
      end
      include 'average.f'
