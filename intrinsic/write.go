package intrinsic

import (
	"fmt"
	"os"
)

var units map[int]*os.File

func init() {
	units = map[int]*os.File{}
	units[6] = os.Stdout
}

func WRITE(unit int, format []byte, a ...interface{}) {
	fmt.Fprintf(units[unit], string(format), a...)
}
