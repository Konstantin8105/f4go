      character *80 argmt
C  Test recognition of standard intrinsic
      x = sqrt(y)
C  Test mismatch of argument types
      z = sqrt(i)
      i = max(x,i)
C  Test recognition of extra intrinsics
      i = ior(i,8)
      i = not(i)
C  Test recognition of unix intrinsics
      call getarg(1,argmt)
C Test the different forms of rand and iargc: zero and one arg
      i = iargc()
      i = iargc(0)
      x = rand()
      x = rand(0)
C Test recognition of vms intrinsics
      x = ran(0)
      end

