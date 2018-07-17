	common /abc/ x,y,z
	abc = 5
	x = abc
	end
	subroutine sub1
	common /abc/ x,y,z
	real abc(5)
	x = abc(1)
	call sub2(x,y)
	call sub2(1.0,2)
	end
	subroutine sub2(a,b)
	common /abc/ x,i,z
	x = abc(1)
	end
	function abc(n)
	abc = n*n
	end
