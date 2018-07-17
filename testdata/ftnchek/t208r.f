      subroutine foo (argchr,maxrow,maxcol)
      character*(*) argchr(maxrow,maxcol,*)
      character one, two*2, three*3, twob*2, oneagain
      parameter (maxy =
     x          54321)
      parameter (maxz =
     x          12345 + maxy)
      character*(*) s
      parameter (s = 'hello')
      integer             MAXTOK
      parameter           (MAXTOK = 10)
      character*(MAXTOK)  toklst
      end
