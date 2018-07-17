! Example of allocating and using an array via a pointer

program pointer_alloc
  real, pointer, dimension(:, :) :: A
  integer :: M, N
  integer :: alloc_err, dealloc_err

  read *, M, N
  allocate ( A(M, N) , stat=alloc_err) 
  if( alloc_err /= 0 ) then
     print *, "Error allocating real array of size", &
          M, "x", N
  else
     read *, A
     print *, A
     deallocate (A, stat = dealloc_err)
     if( dealloc_err /= 0 ) then
        print *, "Error deallocating array"
     else
        print *, "Array deallocated"
     end if
  end if

  if( associated(A) ) then
     print *, "Pointer is still associated"
     nullify(A)
 end if
end program pointer_alloc
