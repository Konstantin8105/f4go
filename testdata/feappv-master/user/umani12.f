!$Id:$
      subroutine umani12

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2021: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Dummy user mesh manipulation set routine

!      Inputs:

!      Outputs:
!         none   - Users are responsible for generating outputs
!                  through common blocks, etc.  See programmer
!                  manual for example.
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include 'umac1.h'

      logical      :: pcomp

      if (pcomp(uct,'ma12',4)) then
!         uct = 'name'
      else

      end if

      end subroutine umani12
