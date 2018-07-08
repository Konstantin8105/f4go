      program hello_world3
      implicit none
      character*32 text
c
      text = 'Hello World'
c
      call printtext(text)

      end

      subroutine printtext(text)
      implicit none
      character*32 text
c
      write (*,*) text
c
      end
