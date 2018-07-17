program blocks
! program to check parsing of block structures
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
           if( i .eq. 50 ) exit loop1
           k = k + j
           if( k .eq. 50) cycle loop2
        end do loop2
        write(*,*) k*i**2
     enddo loop1
     case2: select case (n)
        case(1) case2
           call baz(2.0)
        case(2:10) case2
           call baz(3.0)
        case(-1,11:100,300) case2
           call baz(4.0)
        case default case2
           do i=1,10
              if( i == 7 ) goto 200
              call baz(5.0)
200        end do
     end select case2
  end if
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
end function bar
subroutine baz(c)
  real a, b, c ,d
  read *, a
! Same as above but with spaces restored and construct names added
  first_if: if ( a .gt. 0 ) then
     b = c/a
     second_if: if (b .gt. 0) then
        d = 1.0
     end if second_if
  else if (c .gt. 0) then first_if
     b = a/c
     d = -1.0
  else first_if
     b = abs (max (a, c))
     d = 0
  end if first_if
  print *, a, b, c, d
end subroutine baz

