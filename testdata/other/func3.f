        pure function square(x)
         real, intent(in) :: x
         real :: square
         square = x * x
        end function

        program main
         real :: a, b, square
         a = 2.0
         b = square(a)
         ! After invoking the square(.) pure function, we can be sure that 
         ! besides assigning the output value of square(a) to b,
         ! nothing else has been changed.
        end program main
