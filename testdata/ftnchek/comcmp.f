      character *8 c8,c7*7
      character c1
      common /chars/ c1,c8,c7
      integer iplain
      integer *4 ifour
      integer*2 itwo
      common /ints/ iplain,ifour,itwo
      common /bk1/ a,b,c,d,e,f,g
      c1 = 'A'
      call foo
      end
      subroutine foo
      character *8 c8,c7*7
      character c1
      common /chars/ c7,c8,c1
      integer iplain
      integer *4 ifour
      integer*2 itwo
      common /ints/ iplain,itwo,ifour
      common /bk1/ i,j,k,l,m,n,o
      c1 = 'A'
      call bar
      end
      subroutine bar
      character *8 c8,c7*7
      character c1
      common /chars/ c7,c8
      integer iplain
      integer *4 ifour
      common /ints/ ifour,iplain
      common /bk1/ a(3),j,x,y,z
      c1 = 'A'
      c7 = c1//c8
      end

