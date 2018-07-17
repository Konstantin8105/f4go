      subroutine foo(d)
      parameter (n=1,m=2,k=3)
      character *17 xx
      integer a(1:m*n,-n+2), b(n*(m+k)*2,k+1)
      integer d(*,*)
      common /xx/ xx(101)
      character*1 str, str2*(n), str3*(*), str4*(m+n*k)
      external str
      dimension str2(k*m)
      character *10 c(len('won''t'))
      a(n,1) = a(1,m)
      c(2)(a(ichar('A'//'hello'), ichar(char(40))+3)
     $     :ichar(str("won""t",40) (1:1))) = 'x'
      end
