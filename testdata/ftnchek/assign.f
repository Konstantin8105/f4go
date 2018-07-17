c  for testing propagation of type sizes
      subroutine sizeprop(cadj,i,x)
      character cadj*(*)
      character c1, c5*5, c10*10
      c1 = c5
      c5 = c1
      c5 = cadj
      cadj = c5
      c5 = 'hello'
      c5 = 5Hhello
      c5 = 11Hhello there
      end
      integer i,j,n
      integer*4 m
      parameter (n=6)
      parameter (m=7)
      real x,y
      integer*2 i2
      integer*4 i4
      logical L
      logical*2 L2
      logical*4 L4
      double precision d
      real*4 r4
      real*8 r8
      complex c
      real*16 r16
      double complex z
      complex*16 c16
      complex*32 c32
      i = m
      i = n
      i = 1
      i = i2
      i = i4
      i4 = i
      i4 = i2
      i2 = i4
      i = 4habcd
      i = 8habcdefgh
      x = r4
      x = r8
      x = c
      d = x
      x = d
      z = d
      d = z
      r16 = d
      r16 = r8
      d = r4
      d = r8
      d = r16
      c = x
      c = z
      i = L4
      call sizeprop('hello',i2,r16)
      end
