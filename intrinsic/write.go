package intrinsic

import "os"

var units map[int]*os.File

func init() {
	units = map[int]*os.File{}
	units[6] = os.Stdout
}

func WRITE(unit int, fmt string, a ...interface{}) {
	fmt.Fprintf(units[unit], fmt, a)
}
