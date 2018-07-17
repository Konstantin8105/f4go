      subroutine foo (a,b,c,d)
      real a(1), b(*), c(21,*), d(2)
      parameter (s = ichar(')'))
      parameter (t =
*  comment ( ( (
*  more comments
     x     ichar(')'))
      parameter (u = ! this is a comment
     x        25.0)
      parameter (v =
*  comment ( ( (
*  more comments
     x     2.18281828D+00)

      end
