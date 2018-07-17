      read(unit=5,fmt=1900) x
 1900 format(f10.0)
      if(x) 100,250,89
 89   assign 100 to icky
      goto icky
 100  write(6,*) 'hello'
      do 123 i=1,123
         read(fmt=1900,unit=5,end=200,err=909) x
 123  continue
 302  write(*,909) x
 909  format(1x,f10.3)
 200  continue
      print 9, x
 9    format(' Just checking')
      read 10, x
      type 11, sqrt(2.0)
 11   format(' The sqrt of 2 is ', f15.5)
      write(*,*) 'I''m outta here'
      end
