package intrinsic

import (
	"fmt"
	"os"
	"reflect"
)

var units map[int]*os.File

func init() {
	units = map[int]*os.File{}
	units[6] = os.Stdout
}

func WRITE(unit int, format []byte, a ...interface{}) {
	fmt.Fprintf(units[unit], string(format), a...)
}

func OPEN(unit int, file []byte) {
	f, err := os.Open(string(file))
	if err != nil {
		panic(err)
	}
	units[unit] = f
}

func CLOSE(unit int) {
	delete(units, unit)
}

func READ(unit int, format []byte, a ...interface{}) {
	_, err := fmt.Fscanf(units[unit], string(format), a...)
	if err != nil {
		for i := range a {
			fmt.Println(">", i, "\t", reflect.TypeOf(a[i]))
		}
		panic(err)
	}
}
