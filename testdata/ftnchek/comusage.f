	  Program Main
          common a,b,c
          print *,a,b,c
	  Call Input
	  Call Comp
	  Call Output
	  End

	  Subroutine Input
	  Common /INP/ x1,x2,x3,x4
	  Real x1,x2,x3
	  Common/LAST/ y1,y2,y3
	  Write(*,*) 'Enter 3 Values'
	  Read (*,*) x1,x2,x3
	  y2 = y3
	  Return
	  End

	  Subroutine Comp
	  Common /INP/ A,B,C
          Common /ZAP/ t1,t2,t3
	  Real A,B,C
	  Common /OUTP/ AVG
	  Real AVG
	  Avg = (A+B+C)/3
	  Return
	  End

	  Subroutine Output
	  Common /OUTP/ Averag
	  Real Averag
	  Common /LAST/ n1,n2,n3
	  Write (*,*) 'Average =',Averag
	  n1 = 8
	  Return
	  End
