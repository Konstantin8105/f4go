C  Testing of recognition of CYCLE and EXIT statements.
C  Also tests that no-path warning is generated.
      do i=1,100
         if( mod(i,2) .eq. 0 ) then
            print *, i
            cycle
            print *, i*i ! no path to this statement
         endif
         if( i .gt. 10 ) exit
         print *, i*i  ! there is a path to this statement
      end do
      end

