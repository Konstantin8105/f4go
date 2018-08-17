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
	if format[len(format)-1] == '\n' {
		format = format[:len(format)-1]
	}
	_, err := fmt.Fscanf(units[unit], string(format), a...)
	if err != nil {
		var types string
		for i := range a {
			types += fmt.Sprintf("|%s|", reflect.TypeOf(a[i]))
		}
		panic(fmt.Errorf("READ error for format `%s` : %v\nValues = %v",
			string(format), err, types))
	}
}
