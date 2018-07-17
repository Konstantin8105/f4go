
      SubRoutine EnterGraphicsMode
!
!       Enter ReGIS, clear screen
! 
      character*1 esc
        esc=char(27)
        write(*,*)esc,'P0ps(e)'
        Return
      End
