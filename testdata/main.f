        program main
            integer i
            i = 12
            i = i + 12
            i = i - 20
            i = i *2
            write (*, FMT = 120) i
            CALL realCheck()
            CAll real_test()
            STOp
*
  120 Format (' output ', I1 , ' integer')
        END

        subroutine real_test()
        end

        subroutine realCheck()
            REAL r
            Real real_1
            r = -9.Q+4
            r = r / 5
            r = r + 45**2
            real_1 = r/5
            write (*, FMT = 125) r, real_1
            STOP
*
  125 Format (' output ', F15.2 ,' , ', F14.2 , ' reals')
        end
