

      SubRoutine ExitGraphicsMode

C  Go out of graphics mode back to
C  text mode.

      character*1 esc
        esc=char(27)
        write(*,*)esc,'\',esc,'[15H'
        Return
      End
