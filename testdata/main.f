C -----------------------------------------------------
C Tests 
C -----------------------------------------------------

        program main
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

            call testName("test_do")
            call test_do()

            call testName("test_do_while")
            call test_do_while()

            call testName("test_array")
            call test_array()

            call testName("test_goto")
            call test_goto()

            call testName("test_function")
            call test_function()

            call testName("test_data")
            call test_data()

            call testName("test_matrix")
            call test_matrix()

            call testName("test_types")
            call test_types()

            call testName("test_concat")
            call test_concat()

            ! call testName("test_complex")
            ! call test_complex()

            ! end of tests
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
            write (*, FMT = 120) i
            write (*, '(I1,A2,I1)') i,'YY',i
            write (*, '()')
            return
  120 Format (' output ', I1 , ' integer')
        end

C -----------------------------------------------------

        subroutine test_pow()
            ! initialization
            REAL r,p
            Real real_1
            REAL*8 H(1)
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
            write (*, FMT = 125) r, real_1, H(1)
            return
  125 Format ('POW: ', F15.2 ,' , ', F14.2, ' , ' ,  F14.2)
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

        recursive subroutine test_if()
            integer i
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
                ! stop the program
            ELSEIF (i .EQ. 5) THEN
                WRITE(*,*) "ELSEIF is Ok"
            END IF
        end

C -----------------------------------------------------

  !       subroutine test_complex()
  !           complex c1,c2
  !           c1 = (1.1221,2.2)
  !           c2 = (3.23,5.666)
  !           c1 = c1 + c2
  !           write(*, fmt = 300) c1
  !           return
  ! 300 Format (F15.4)
  !       end

C -----------------------------------------------------

        subroutine test_do()
            integer IR, JR, HR, iterator
            integer ab_min
            JR = 1
            iterator = 1
            Do IR = 1,10,3
                write (*,FMT=142) IR
            end do
            Do IR = 1,3
                write (*,FMT=146) IR
            end do
            DO 143 IR = 1,3
                write (*,FMT=147) IR
  143 Continue
  144 Continue
            Do IR = 1,3
                write (*,FMT=148) IR
            end do

            if (ab_min(3,14) .EQ. 14) THEN
                call fail("test_do 1")
            end if

            Do IR = 1,ab_min(3,13)
                write (*,FMT=149) IR
            enddo
            DO 145, HR = 1,2
                DO 145 JR = 1,2
                    write(*,FMT=150) HR, JR
  145 Continue
            write (*,fmt=151)iterator
            iterator = iterator + 1
            IF ( iterator .LE. 3) THEN
                write(*,*) "iterator is less or equal 3"
                GO TO 144
            END IF
            return
  142 FORMAT ('Do with inc ', I2)
  146 FORMAT ('Do ', I2)
  147 FORMAT ('Do with continue ', I2)
  148 FORMAT ('Do with end do ', I2)
  149 FORMAT ('Do with enddo ', I2)
  150 FORMAT ('Double DO ', I2, I2)
  151 FORMAT (' iterator = ', I2)
         end

         integer function ab_min(a,b)
             integer a,b
             if (a .LE. b) then
                 ab_min = a
             else 
                 ab_min = b
             end if 
             return
         end function


C -----------------------------------------------------

        subroutine test_do_while()
            integer iterator
            iterator = 1
            Do while (iterator .Le. 3)
                write (*,FMT=180) iterator
                iterator = iterator + 1
            end do
            return
  180 FORMAT ('Do while ', I2)
        end 

C -----------------------------------------------------

        subroutine test_array()
            integer iterator(3),ir, summator
            external summator
            do ir = 1,3
                iterator(ir) = ir
            end do
            do ir = 1,3
                write(*,fmt = 210) iterator(ir)
            end do
            if (summator(iterator) .NE. 6) THEN
                call fail("test_array 1")
            end if
            return
  210 FORMAT ('vector ', I2)
        end 

        integer function summator(s)
            integer s(*), ir, sum
            sum = 0 
            do ir = 1,3
                sum = sum + s(ir)
            end do
            summator = sum
            return
        end function

C -----------------------------------------------------
        subroutine test_goto()
            integer t,m
            t = 0
            m = 0
  230       t = t + 1
  240       t = t + 5
            m = m + 1
            write(*, FMT = 251) t
            if ( m .GE. 5) THEN
                go to 250
            end if
            write(*, FMT = 252) m
            goto (230,240), m
  250       return
  251 FORMAT ('goto check t = ', I2)
  252 FORMAT ('goto check m = ', I2)
        end

C -----------------------------------------------------

        subroutine test_function()
            integer ai
            character bi*32
            logical l, function_changer
            external function_changer
            ai = 12
            bi = "rrr"
            write(*,fmt = 270) ai,bi
            l = function_changer(ai,bi)
            if ( l .NEQV. .TRUE.) THEN 
                call fail("test function in logical")
            end if
            write(6,fmt = 271) ai,bi
            return
  270 FORMAT('test function integer = ', I9, ' array = ' , A3)
  271 FORMAT('test function integer = ', I9, ' array = ' , A3)
        end subroutine

        logical function function_changer(a,b)
            integer a
            character b*32
            a = 34
            b = "www"
            function_changer = .TRUE.
            return
        end

C -----------------------------------------------------

        subroutine test_data
            INTEGER LOC12( 4 ), LOC21( 4 )
            INTEGER IPIVOT( 2, 2 )
            real*4  v
            integer r

            data v , r / 23.23 , 25 /
            DATA    LOC12 / 3, 4, 1, 2 / , LOC21 / 2, 1, 4, 3 /
            DATA    IPIVOT / 1, 2, 3, 4 /

            if ( r .NE. 25) then 
                call fail("test_data 1")
            end if
            if(.NOT. ( 23.0 .LE. v .AND. v .LE. 23.5 )) then
                call fail("test_data 2")
            end if 
            if ( LOC12(1) .NE. 3) call fail("test_data 3")
            if ( LOC12(2) .NE. 4) call fail("test_data 4")
            if ( LOC12(3) .NE. 1) call fail("test_data 5")
            if ( LOC12(4) .NE. 2) call fail("test_data 6")

            if ( LOC21(1) .NE. 2) call fail("test_data 7")
            if ( LOC21(2) .NE. 1) call fail("test_data 8")
            if ( LOC21(3) .NE. 4) call fail("test_data 9")
            if ( LOC21(4) .NE. 3) call fail("test_data 10")

            if (IPIVOT(1,1) .NE. 1) call fail("test_data matrix 1")
            if (IPIVOT(2,1) .NE. 2) call fail("test_data matrix 2")
            if (IPIVOT(1,2) .NE. 3) call fail("test_data matrix 3")
            if (IPIVOT(2,2) .NE. 4) call fail("test_data matrix 4")
        end subroutine

C -----------------------------------------------------

        subroutine test_matrix
            integer M(3,2),I,J
            do I = 1,3
                do J = 1,2
                    M(I,J) = I*8+J+(I-J)*5
                end do
            end do
            do I = 1,3
                do J = 1,2
                    write(*,fmt=330) I, J, M(I,J)
                end do
            end do
            call matrix_changer(M,3,2)
            do I = 1,3
                do J = 1,2
                    write(*,fmt=331) I, J, M(I,J)
                end do
            end do
            return
  330 format('Matrix (',I1,',',I1,') = ', I2 )
  331 format('Matrix*(',I1,',',I1,') = ', I2 )
        end

        subroutine matrix_changer(M,IN,JN)
            integer M(IN,JN), IN, JN, I, J
            do I = 1,IN
                do J = 1,JN
                    M(I,J) = M(I,J) + 2*(I+J)
                end do
            end do
            return
        end subroutine

C -----------------------------------------------------

        subroutine test_types
            REAL    R1
            REAL *4 R4
            REAL *8 R8
            DOUBLE precision DP
            INTEGER    I1
            INTEGER *2 I2
            INTEGER *4 I4
            INTEGER *8 I8
            R1 = 45.1
            R4 = 45.1
            R8 = 45.1
            DP = 45.1
            I1 = 12
            I2 = 12
            I4 = 12
            I8 = 12
            if ( 45.0 .LE. R1 .AND. R1 .LE. 45.2) THEN
                write(*,*)'R1 ... ok'
            end if
            if ( 45.0 .LE. R4 .AND. R4 .LE. 45.2) THEN
                write(*,*)'R4 ... ok'
            end if
            if ( 45.0 .LE. R8 .AND. R8 .LE. 45.2) THEN
                write(*,*)'R8 ... ok'
            end if
            if ( 45.0 .LE. DP .AND. DP .LE. 45.2) THEN
                write(*,*)'DP ... ok'
            end if
            if ( I1 .Eq. 12) write(*,*)'I1 ... ok'
            if ( I2 .Eq. 12) write(*,*)'I2 ... ok'
            if ( I4 .Eq. 12) write(*,*)'I4 ... ok'
            if ( I8 .Eq. 12) write(*,*)'I8 ... ok'
            return
        end subroutine

C -----------------------------------------------------

        subroutine test_concat
            CHARACTER*1 a,b
            CHARACTER*2 c
            a = 'a'
            b = 'b'
            c = a // b
            write(*,*)a
            write(*,*)b
            write(*,*)c
        end

C -----------------------------------------------------


