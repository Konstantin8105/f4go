C  Test program for acceptance of new style relational operators.  These
C  are F90 standard, not F77.
      real x, y

      write(*,*) 'Enter two numbers'
      read(*,*) x,y

      if( x > y ) then
         write(*,*) x,' is greater than',y
      endif
      if( x == y ) then
         write(*,*) x, ' is equal to ',y
      endif
      if( x /= y ) then
         write(*,*) x, ' is not equal to',y
      endif
      if( x >= y ) then
         write(*,*) x,' is greater or equal to than',y
      endif
      if( x < y ) then
         write(*,*) x, ' is less than ',y
      endif
      if( x <= y ) then
         write(*,*) x, ' is less than or equal to ',y
      endif
      end

