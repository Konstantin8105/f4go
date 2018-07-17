C From:  USER NOTES ON FORTRAN PROGRAMMING (UNFP) 
C  http://www.ibiblio.org/pub/languages/fortran/ch2-16.html
Copyright (C) 1996-1998 to the contributors.  All rights are reserved.
      program cyrptr 
      integer           i
      real              array1(10),   array2(5), 
     &                  pointee1(10), pointee2(5), pointee3(*)
      pointer           (ptr1, pointee1),
     &                  (ptr2, pointee2),
     &                  (ptr3, pointee3)
      data              array1   /0,1,2,3,4,5,6,7,8,9/,
     &                  array2   /5,5,5,5,5/
c     ------------------------------------------------------------------
      write(*,*) 
      write(*,'(1x,a,10f6.1)') 'array1=   ', array1
      write(*,'(1x,a,10f6.1)') 'array2=   ', array2
c     ------------------------------------------------------------------
      write(*,*) 
      ptr1 = loc(array1)
      ptr2 = loc(array1)
      ptr3 = loc(array1)
      write(*,'(1x,a,10f6.1)') 'pointee1= ', pointee1
      write(*,'(1x,a,10f6.1)') 'pointee2= ', pointee2
      write(*,'(1x,a,10f6.1)') 'pointee3= ', (pointee3(i), i = 1, 10)
c     ------------------------------------------------------------------
      write(*,*) 
      ptr1 = loc(array2)
      ptr2 = loc(array2)
      ptr3 = loc(array2)
      write(*,'(1x,a,10f6.1)') 'pointee1= ', pointee1
      write(*,'(1x,a,10f6.1)') 'pointee2= ', pointee2
      write(*,'(1x,a,10f6.1)') 'pointee3= ', (pointee3(i), i = 1, 5)
c     ------------------------------------------------------------------
      end
c The result of this program on a VMS machine was:
c
c array1=      0.0   1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0
c array2=      5.0   5.0   5.0   5.0   5.0
c
c pointee1=    0.0   1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0
c pointee2=    0.0   1.0   2.0   3.0   4.0
c pointee3=    0.0   1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0
c
c pointee1=    5.0   5.0   5.0   5.0   5.0   0.0   0.0   0.0   0.0   0.0
c pointee2=    5.0   5.0   5.0   5.0   5.0
c pointee3=    5.0   5.0   5.0   5.0   5.0
c
c
