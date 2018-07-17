	program yacctest
c
c  Tests the Yacc fortran parser.
*  Should pass the current version.
C  It is also a legal, though not especially meaningful, program
*
	implicit integer (u-z)
	dimension xyz(3),abc
c  The next line has some whitespace, but nothing else.  Note continuation!!
             		  
     $ 		(9,9,100)
22	equivalence (a,b) , (xyz,abc,def)
	intrinsic sqrt,log
c
	common com1,com2 /blk1/ com3,com4(5),com6, /blk2/ w123x

	common comm1,comm2 /blk3/ comm3,comm4(5),comm6 /blk4/ cc1

	implicit double precision (d), real(r)
	integer program(3,3), pause(20)
         character *8 bb,bb13*3
	character *4, cc(9),cc1
	parameter (Pi = 3.14159265358979D0)

	logical stop, save
	equivalence (xyz(1),qq), (bb(1:3),bb13)
     x	,	(cc1,bb)
	real a123(9,9), ifa
	integer xray
	external xray
	double precision dbl1,dbl2(3)
	logical test
	complex compl1,compl2
	test = .true.
	data ( (a123(i,j),i=1,9),j=1,9) ,ifa / 82*0.0 /
	data cc / 9*'help' /, bb /'wouldn''t'/ com4 / -1e7,+5.2
     $,					4habcd, .1, 1. /
	if(dbl1) 10,34567,5432
34567	stop
10	stop 123
5432	pause
	end = 123 + pause(1)
	read(iunit,*) (((abc(i,j,k),k=1,100),j=1,9),i=1,9),xyz(2)
	read iunit , a,b,xyz(1)
	write(iunit,*) 'hello there',a
	print *, cc(1),a+b*Pi
	print 5, (cc(i),i=1,4)
	if( stop .and. save ) then
		cc(2) = 'abcd'
		www = 4.7 + (Pi * 2)**8
	else if(abc(1,1,1) .eq. xyz(2)) then
		open(unit=4,file='foo.bar',err=5432,status='old')
		rewind 1+3
		close(4)
		open(3,file='buzzsaw',form='unformatted',status='new')
		backspace (unit=3)
		close(unit=3)
	endif
	test = .false.
	dbl1 = 1.2345
	write (6,900) dbl1
900	format(f10.2)
	if(test) goto 10
	pause 456
	goto 34567
	end

c comments in here don't hurt a bit

	integer function xray(beta,gamma)
	character *(*) gamma
*

	logical beta
	if( beta ) xray = 1.0D0
	end

	subroutine home(x,*)
	common /blk1/ com3,com4(5),com6
	save /blk1/,xray
	if((x-1.0)**(sqrt(2.0))/(5/4+com6) .gt. com3) then
		xray = 1.234
		return
	else
		xray = -1.234 ** (+1.7e10)
	endif
	com4(com2+com6) = com4(xray)
	return 1
	end
