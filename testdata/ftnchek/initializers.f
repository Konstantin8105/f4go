C  Testing syntax recognition and assignment type checking of
C  standard and non-standard initializing declarations.

C  Here we use modern standard F90 initializers
      subroutine f90
      integer :: m, n = 100
C ftnchek does not recognize the following one yet
      integer, dimension(5) :: a = (/ 1, 2, 3, 4, 5 /)
      real :: pi = 3.14159265358979d0
      character :: c = 32   ! type mismatch
      integer :: q = 'hello'   ! type mismatch
      print *, 'F90 initializers:'
      print *, m, n, a   ! m used before set
      print *, pi, c, q
      end
C  Here we use archaic but standard F77 separate type decls and data stmts
      subroutine f77
      integer m, n, a(5)
      real pi
      character c
      integer q
      data n / 100 /
      data a /  1, 2, 3, 4, 5 /
      data pi / 3.14159265358979d0 /
      data c / 32 /   ! type mismatch
      data q / 'hello' /   ! type mismatch
      print *, 'F77 initializers:'
      print *, m, n, a   ! m used before set
      print *, pi, c, q
      end
C  This one uses "bastard" initializer form.
      subroutine bastard
      integer m, n / 100 /, a(5) / 1, 2, 3, 4, 5 /
      real pi / 3.14159265358979d0 /
      character c / 32 /   ! type mismatch
      integer q / 'hello' /   ! type mismatch
      print *, 'bastard initializers:'
      print *, m, n, a   ! m used before set
      print *, pi, c, q
      end
C  Main program just suppresses "never invoked" warnings.
      program main
      call f90
      call f77
      call bastard
      end

