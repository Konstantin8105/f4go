      implicit none
      integer M, numprimes
      integer MAXPRIMES

      parameter (MAXPRIMES=10000)
      integer primes(MAXPRIMES)
      integer makeprimes

      write(*,*) 'Enter ceiling on primes'
      read(*,*) M

C Get list of primes up to M
      numprimes = makeprimes(primes,M,MAXPRIMES)

C Print the primes
      call printarray('Primes',primes,numprimes)

      end

      subroutine printarray(heading,a,n)
      implicit none
      character *(*) heading
      integer i, n, a(1)
      write(*,*) heading
      do i=1,n
         write(*,'(1x,i4,1x,i5)') i, a(i)
      end do
      end

      integer function makeprimes(p,n,max)
C computes array of primes less than or equal to n.
C note: max = limit of number of primes, must be at least 1.
      implicit none
      integer n, max, p(max)
      integer i, numprimes
      logical divisible
C put the even prime into the list
      numprimes = 1
      p(numprimes) = 2
C loop thru odd integers, testing if divisible
      do i=3,n,2
         if( .not. divisible(i,p,numprimes) ) then
            numprimes = numprimes+1
            if( numprimes .gt. max ) then
               write(*,*) 'Ran out of space at p=',i
               stop
            else
               p(numprimes) = i
            end if
         end if
      end do
C return number of primes found
      makeprimes = numprimes
      return
      end
      logical function divisible(n,p,nump)
      implicit none
      integer n, nump, p(1)
      integer i
      do i=1,nump
         if( mod(n,p(i)) .eq. 0 ) then
            divisible = .true.
            return
         end if
      end do
      divisible = .false.
      return
      end
