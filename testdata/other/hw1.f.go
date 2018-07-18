package main

func hello_world() {
	{
		var text [256]byte
		__builtin_memmove(text, "Hello\nA\n World"[1], 16)
		__builtin_memset(f4goUndefinedpointer_plus_expr, 32, 16)
		{
			var dt_parm.0 __st_parameter_dt
			dt_parm.0.common.filename = "testdata/hw1.f "[1]
			dt_parm.0.common.line = 7
			dt_parm.0.common.flags = 128
			dt_parm.0.common.unit = 6
			_gfortran_st_write(dt_parm.0)
			_gfortran_transfer_character_write(dt_parm.0, text, 32)
			_gfortran_st_write_done(dt_parm.0)
		}
	}
}
func main() int {
	{
		var options.1 [288]int
		_gfortran_set_args(argc, argv)
		_gfortran_set_options(9, options.1[0])
		hello_world()
		return f4goUndefinedmodify_expr
	}
}
