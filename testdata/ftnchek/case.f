
      program testcase
      integer ivar1
      character*8 cvar1
      character*1 cvar2
      logical lvar1
      real rvar1
      ivar1 = 3
      lvar1 = .true.

      select case (ivar1)
          cvar1 = 'no path!'
      case (:-1)
          cvar1 = 'path'
      case (10/5 - 2)
      case (1)
      case (2:3, 5:7, 9)
      case (10:)
      case default
          cvar2 = 'd'
      end select

      select case (lvar1)
      case (.false.)
      case (.true.:)
      case (:.false.)
      case (1.eq.0)
      end select

      cvar1 = 'hamster'
      select case (cvar1)
      case (:'cat')
      case ('dog':'fer' // 'ret')
      case ('gerbil')
      case ('horse':)
      case default
      end select

      select case (rvar1)
      case (-1.0)
      case (2:5.0)
      case (ivar1)
      case (:.false.)
      end select
      stop
      end
