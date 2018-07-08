      program hello_world2
      implicit none
c
      call hello
      call hello

      end

      subroutine hello
      implicit none
      character*32 text
c
      text = 'Hello\nR\nWorld '
      write (*,*) text
c
      end
