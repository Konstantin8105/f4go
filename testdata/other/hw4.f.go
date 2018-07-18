package main

func MAIN__() {
	{
		{
			var dt_parm.0 __st_parameter_dt
			dt_parm.0.common.filename = "testdata/hw4.f "[1]
			dt_parm.0.common.line = 2
			dt_parm.0.common.flags = 128
			dt_parm.0.common.unit = 6
			_gfortran_st_write(dt_parm.0)
			_gfortran_transfer_character_write(dt_parm.0, "Hello, world!"[1], 13)
			_gfortran_st_write_done(dt_parm.0)
		}
	}
}
func main() int {
	{
		var options.1 [288]int
		_gfortran_set_args(argc, argv)
		_gfortran_set_options(9, options.1[0])
		MAIN__()
		return f4goUndefinedmodify_expr
	}
}
