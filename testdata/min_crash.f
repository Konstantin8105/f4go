C -----------------------------------------------------
C Tests 
C -----------------------------------------------------

        program MAIN
            ! begin of tests
            call testName("test_operations")
            call test_operations()

            call testName("test_pow")
            call test_pow()

            call testName("real_test_name")
            call real_test_name()

            call testName("test_subroutine")
            call test_subroutine()

            call testName("test_if")
            call test_if()

        END


        subroutine testName(name)
            character(*) name
            write (*,FMT=45) name
            return
  45        FORMAT ('========== Test : ',A20,' ==========')
        end

        subroutine fail(name)
            character(*) name
            write (*,FMT=46) name
            STOP
  46        FORMAT ('***** FAIL : ',A)
        end

C -----------------------------------------------------
C -----------------------------------------------------
C -----------------------------------------------------
C -----------------------------------------------------

        subroutine real_test_name()
        end subroutine ! this is also test

C -----------------------------------------------------

        subroutine test_operations()
            integer i
            i = 12
            i = i + 12
            i = i - 20
            i = i *2
            write (*, FMT = 120) i ! TEST COMMENT
            write (*, '(I1,A2,I1)') i,'YY',i
            write (*, '()') ! TEST COMMENT
            return
  120 Format (' output ', I1 , ' integer I''C values ')
        end

C -----------------------------------------------------

        subroutine test_pow()
            REAL r,p
            Real real_1
            REAL*8 H(1)
            COMPLEX *16 C
            Intrinsic REAL
            C = (2.0,2.0)
C calculation
            H(1) = 3
            H(1) = H(1) ** H(1)
            r = -9.Q+4
            p = 1.45
            r = r / 5
            r = r + 1.4**2.1
            r = -(r + p**p)
            r = (r + p)**(p-0.6)
            real_1 = r/5
            real_1 = real_1 + 1.222**REAL(C)
            write (*, FMT = 125) r, real_1, H(1)
            return
  125 Format ('POW: ', F15.2 ,' , ', F14.2, ' , ' ,  F14.2)
        end

C -----------------------------------------------------

        subroutine test_subroutine()
            REAL a,b
            a = 5
            b = 6
            write (*||FMT = 131) 1,a ! ERRRRROORORORORORORO
            write (*, FMT = 131) 2,b
            CALL ab(a,b)
            write (*, FMT = 131) 3,a
            write (*, FMT = 131) 4,b
            return
  131 Format (' output ', I2, ' ', F12.5 , ' real ')
        end

        subroutine ab(a,b)
            REAL a,b
            a = a + 5.12
            b = b + 8.4
            return
        end

C -----------------------------------------------------

        recursive subroutine test_if()
            integer i, J
            logical l
            l = .false.
            i = 5
            IF ( i .EQ. 5) write(*,*) "Operation  .EQ.    is Ok"
            IF ( i .GE. 5) write(*,*) "Operation  .GE.    is Ok"
            IF ( i .LE. 5) write(*,*) "Operation  .LE.    is Ok"
            IF ( i .GE. 4) write(*,*) "Operation  .GE.    is Ok"
            IF ( i .LE. 3) write(*,*) "Operation  .GE.    is Ok"
            IF ( i .NE. 3) write(*,*) "Operation  .NE.    is Ok"
            IF ( .true.  ) write(*,*) "Operation  .TRUE.  is Ok"
            IF (.NOT. l  ) write(*,*) "Operation .NOT. .false. is Ok"
            IF (.NOT. l  ) write(*,*) "Operation .NOT. .FALSE. is Ok"
            l = .TRUE.
            IF ( l       ) write(*,*) "Operation  .TRUE.  is Ok"

            IF (i .GE. 100) THEN
                STOP 
            ELSEIF (i .EQ. 5) THEN ! TEST COMMENT
                WRITE(*,*) "ELSEIF is Ok"
            END IF ! TEST COMMENT

            DO 100 J = 1,2
                IF (.NOT.l) THEN ! TEST COMMENT
                ELSE 
                    IF (J.GE.0) THEN 
                        WRITE(*,*) "Ok"
                    END IF
                END IF
  100       CONTINUE

            CALL ZD()

            RETURN
        end

        SUBROUTINE ZD()
			INTEGER IFORM
            sdf sdfsdf sdfsdf
            DO 100 IFORM = 1, 2
      			IF ( IFORM .NE. 0 ) THEN
                    WRITE(*,'(I1)')IFORM
                END IF
  100       CONTINUE
        RETURN
        END
