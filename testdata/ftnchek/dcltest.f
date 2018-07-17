      program test1
c
      character  STRING1*6
      parameter( STRING1 = 'abc123' )

      character  STRING2*(6)
      parameter( STRING2 = 'xyz789' )
c
      write(*,*) STRING1,STRING2
c
      end
