package main

func square_cube() {
	{
		{
			var tempVarF4GO_32 int
			var tempVarF4GO_45 int
			tempVarF4GO_45 = *i
			tempVarF4GO_32 = tempVarF4GO_45 * tempVarF4GO_45
			*isquare = tempVarF4GO_32
		}
		{
			var tempVarF4GO_34 int
			var tempVarF4GO_49 int
			tempVarF4GO_49 = *i
			var tempVarF4GO_51 int
			tempVarF4GO_51 = tempVarF4GO_49 * tempVarF4GO_49
			tempVarF4GO_34 = tempVarF4GO_49 * tempVarF4GO_51
			*icube = tempVarF4GO_34
		}
	}
}
func xx() {
	{
		var i int
		i = 4
		square_cube(i, isq, icub)
		{
			var dt_parm.0 __st_parameter_dt
			dt_parm.0.common.filename = "testdata/sub1.f "[1]
			dt_parm.0.common.line = 13
			dt_parm.0.common.flags = 128
			dt_parm.0.common.unit = 6
			_gfortran_st_write(dt_parm.0)
			_gfortran_transfer_character_write(dt_parm.0, "i,i^2,i^3="[1], 10)
			_gfortran_transfer_integer_write(dt_parm.0, i, 4)
			_gfortran_transfer_integer_write(dt_parm.0, isq, 4)
			_gfortran_transfer_integer_write(dt_parm.0, icub, 4)
			_gfortran_st_write_done(dt_parm.0)
		}
		{
			var dt_parm.1 __st_parameter_dt
			dt_parm.1.common.filename = "testdata/sub1.f "[1]
			dt_parm.1.common.line = 14
			dt_parm.1.common.flags = 128
			dt_parm.1.common.unit = 6
			_gfortran_st_write(dt_parm.1)
			_gfortran_transfer_character_write(dt_parm.1, "i,i^2,i^3="[1], 10)
			_gfortran_transfer_integer_write(dt_parm.1, i, 4)
			_gfortran_transfer_integer_write(dt_parm.1, isq, 4)
			_gfortran_transfer_integer_write(dt_parm.1, icub, 4)
			_gfortran_st_write_done(dt_parm.1)
		}
	}
}
func main() int {
	{
		var options.2 [288]int
		_gfortran_set_args(argc, argv)
		_gfortran_set_options(9, options.2[0])
		xx()
		return f4goUndefinedmodify_expr
	}
}
