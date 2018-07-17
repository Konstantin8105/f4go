      program blocks
! program with various errors in balancing block structures
        real x
        integer n
        x = bar(1.0)
        if( x .gt. 0 ) then
           call baz(x)
           n = int(x)
           select case (n)
              case(1)
                 call baz(2.0)
              case(2:10)
                 call baz(3.0)
              case(-1,11:100,300)
                 call baz(4.0)
              case default
                 call baz(5.0)
           end select
        else
           k = 0
           loop1: do i=1,100
              loop2:    do j=1,10
                 write(*,*) i,j,i*j
                 if( i .eq. 50 ) exit loopdedo   ! wrong construct name
                 k = k + j
                 if( k .eq. 50) cycle loop2
              end do loop2
              write(*,*) k*i**2
           enddo foop1
           case2: select case (n)
              case(1) case2
                 call baz(2.0)
              case(2:10) caseZ
                 call baz(3.0)
              case(-1,11:100,300) case2
                 call baz(4.0)
              case default case2
!  do-loops with shared terminator
                 do 100 i=1,10
                    do 100 j=1,10
                       print *, i, j, i*j
 100             continue
                 do 200 i=1,10
                    do 250 j=1,10
                       print *, i, j, i*j
 200                continue                  ! terminators out of order
 250             end do
           end select case2
        end if
        if( x .eq. 0) exit                    ! no enclosing DO
        else if( x .lt. 0 ) then              ! else has no matching then
           print *, 'Hello'
        end select                            ! should be end if
      end program blocks
      function bar(c)
        real a, b, c ,d
        read *, a
! This block is from section 8.1.2.3 of the F90 standard, except for
! removing space between some keywords
        if ( a .gt. 0 ) then
           b = c/a
           if (b .gt. 0) then
              d = 1.0
           endif
        elseif (c .gt. 0) then
           b = a/c
           d = -1.0
        else
           b = abs (max (a, c))
           d = 0
        endif
        bar = d*b
      end subroutine
      subroutine baz(c)
        real a, b, c ,d
        read *, a
! Same as above but with spaces restored and construct names added
        first_if: if ( a .gt. 0 ) then
           b = c/a
           second_if: if (b .gt. 0) then
              d = 1.0
           end if second_if
        else if (c .gt. 0) then firstif
           b = a/c
           d = -1.0
        else first_if
           b = abs (max (a, c))
           d = 0
        end if
        print *, a, b, c, d
      end subroutine bazz
      
