package main

func printtext() {
	{
		{
			var dt_parm.0 __st_parameter_dt
			dt_parm.0.common.filename = "testdata/hw3.f "[1]
			dt_parm.0.common.line = 15
			dt_parm.0.common.flags = 128
			dt_parm.0.common.unit = 6
			_gfortran_st_write(dt_parm.0)
			_gfortran_transfer_character_write(dt_parm.0, text, 32)
			_gfortran_st_write_done(dt_parm.0)
		}
	}
}
func hello_world3() {
	{
		var text [256]byte
		__builtin_memmove(text, "Hello World"[1], 11)
		__builtin_memset(f4goUndefinedpointer_plus_expr, 32, 21)
		printtext(text, 32)
	}
}
func main() int {
	{
		var options.1 [288]int
		_gfortran_set_args(argc, argv)
		_gfortran_set_options(9, options.1[0])
		hello_world3()
		return f4goUndefinedmodify_expr
	}
}
