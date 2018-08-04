C -----------------------------------------------------
C Tests 
C -----------------------------------------------------

        program main
            ! begin of tests
            write(*,*) "test_operations"
            call test_operations()

            write(*,*) "test_pow"
            call test_pow()

            write(*,*) "real_test_name"
            call real_test_name()

            write(*,*) "test_subroutine"
            call test_subroutine()
            ! end of tests
        END

C -----------------------------------------------------
C -----------------------------------------------------
C -----------------------------------------------------
C -----------------------------------------------------

        subroutine real_test_name()
        end

C -----------------------------------------------------

        subroutine test_operations()
            integer i
            i = 12
            i = i + 12
            i = i - 20
            i = i *2
            write (*, FMT = 120) i
            return
  120 Format (' output ', I1 , ' integer')
        end

C -----------------------------------------------------

        subroutine test_pow()
            REAL r,p
            Real real_1
            r = -9.Q+4
            p = 1.45
            r = r / 5
            r = r + 45**2
            r = r + p**p
            r = (r + p)**(p+1)
            real_1 = r/5
            write (*, FMT = 125) r, real_1
            return
  125 Format (' output ', F15.2 ,' , ', F14.2 , ' reals')
        end

C -----------------------------------------------------

        subroutine test_subroutine()
            REAL a,b
            a = 5
            b = 6
            write (*, FMT = 131) a
            write (*, FMT = 132) b
            CALL ab(a,b)
            write (*, FMT = 133) a
            write (*, FMT = 134) b
            return
  131 Format (' outpu1 ', F12.5 , ' real ')
  132 Format (' outpu2 ', F12.5 , ' real ')
  133 Format (' outpu3 ', F12.5 , ' real ')
  134 Format (' outpu4 ', F12.5 , ' real ')
        end

        subroutine ab(a,b)
            REAL a,b
            a = a + 5.12
            b = b + 8.4
            return
        end

C -----------------------------------------------------
