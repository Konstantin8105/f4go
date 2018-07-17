C  Exerciser for recognition and flagging of various I/O keywords.
      program iokeywds
      call vms_isms
      call direct_access
      call f90_stuff
      end
      subroutine vms_isms
      character*512 line(4)
C  This one uses some VMS-isms
      open(unit=1,carriagecontrol='none',recordtype='fixed',
     $  recl=2048,status='new',name='file1')
      open(unit=2,carriagecontrol='none',recl=512,status='old',err=1)
      goto 2
1     stop 'Please $ASSIGN/USER output FOR001'
2     do i=1,4
       read(2,'(a)',err=3) line(i)
      end do
      write(1,'(a)') line(1)//line(2)//line(3)//line(4)
      goto 2
3     continue
      close(unit=1)
      close(unit=2)
      end
      subroutine direct_access
      character inline*79

      open(unit=1,file='test',access='direct',form='formatted',
     1     status='new', recl=80,err=909)
*
*       Read a line from standard input, write it to the new file,
*       and echo it to standard output.
*
      do j = 1,100
            read(5,10,end=910)inline
            write(1,20,rec=j)inline
            write(5,20)inline
      enddo

 10   format(79a)
 20   format(1x79a)
      GoTo 910
 909  print*,'Error opening file.'
 910  continue
      close(unit=1)
      close(unit=5)
      end
      subroutine f90_stuff
      integer unit,inputsize,val,i1
      character *20 c1,c2
      unit = 3
      open(unit=unit,file='data.dat',status='old',action='read',
     $     access='sequential',form='formatted',err=910)
      read(unit=unit,fmt='(i10)',advance='no',size=inputsize,
     $     err=920) val
      inquire(unit=unit,recl=i1,access=c1,position=c2)
      write(unit=*,fmt='(1x,a7)',advance='yes') 'Success'
      close(unit)
      return
 910  print *,'Error opening file.'
      return
 920  print *,'Error reading file.'
      return
      end
