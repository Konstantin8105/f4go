# f4go

Transpiling fortran code to golang code

AST from `gfortran`. Example:
```bash
	gfortran -fdump-tree-original-raw=ast.tree hello.f
```

Present result:
```fortran
        function func(i) result(j)
           integer, intent(in) :: i ! input
           integer             :: j ! output
           j = i**2 + i**3
        end function func
      
      program xfunc
         implicit none
         integer :: i
         integer :: func
         i = 3
         print*,"sum of the square and cube of",i," is",func(i)
      end program xfunc
```

Go code:

```golang
package main

func func() int {
	{
		var j int
		{
			var tempVarF4GO_34 int
			var tempVarF4GO_44 int
			tempVarF4GO_44 = f4goUndefinedindirect_ref
			var tempVarF4GO_46 int
			tempVarF4GO_46 = tempVarF4GO_44 * tempVarF4GO_44
			var tempVarF4GO_48 int
			tempVarF4GO_48 = f4goUndefinedindirect_ref
			var tempVarF4GO_50 int
			tempVarF4GO_50 = tempVarF4GO_48 * tempVarF4GO_48
			tempVarF4GO_34 = tempVarF4GO_48 * tempVarF4GO_50
			j = tempVarF4GO_46 + tempVarF4GO_34
		}
		return f4goUndefinedmodify_expr
	}
}
func xfunc() {
	{
		var i int
		i = 3
		{
			var dt_parm.0 __st_parameter_dt
			dt_parm.0.common.filename = "testdata/func1.f "[1]
			dt_parm.0.common.line = 12
			dt_parm.0.common.flags = 128
			dt_parm.0.common.unit = 6
			_gfortran_st_write(dt_parm.0)
			_gfortran_transfer_character_write(dt_parm.0, "sum of the square and cube of"[1], 29)
			_gfortran_transfer_integer_write(dt_parm.0, i, 4)
			_gfortran_transfer_character_write(dt_parm.0, " is"[1], 3)
			{
				var tempVarF4GO_67 int
				tempVarF4GO_67 = func(i)
				_gfortran_transfer_integer_write(dt_parm.0, tempVarF4GO_67, 4)
			}
			_gfortran_st_write_done(dt_parm.0)
		}
	}
}
func main() int {
	{
		var options.1 [288]int
		_gfortran_set_args(argc, argv)
		_gfortran_set_options(9, options.1[0])
		xfunc()
		return f4goUndefinedmodify_expr
	}
}
```
