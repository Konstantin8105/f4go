      program main
      call test(100)
      end
      subroutine test(n)
      integer n
C the following local array cannot have variable bounds.
      integer auto(n)
      auto(1) = 5
      auto(2) = auto(1)*2
      end

