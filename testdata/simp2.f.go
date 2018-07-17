package main

func cows() {
	{
		var n int
		n = 306
		var n2 int
		n2 = n*2/4 + n
		n2 = n*2 + n2
		n2 = n2 + n + -2
		{
			var tempVarF4GO_38 int
			var tempVarF4GO_60 int
			tempVarF4GO_60 = n
			tempVarF4GO_38 = n
			n2 = _gfortran_pow_i4_i4(tempVarF4GO_60, n+1) + n2 + _gfortran_pow_i4_i4(tempVarF4GO_38, n2-n)
		}
		n2 = (n/2+n2)*(n2+n) + 42
		n2 = (n/2 + n2 + -21) * 2
		var n3 int
		n3 = n + n2
	}
}
func main() int {
	{
		var options.0 [288]int
		_gfortran_set_args(argc, argv)
		_gfortran_set_options(9, options.0[0])
		cows()
		return f4goUndefinedmodify_expr
	}
}
