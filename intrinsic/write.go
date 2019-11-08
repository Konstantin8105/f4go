package intrinsic

import (
	"bytes"
	"fmt"
	"os"
	"reflect"
)

var units map[int]*os.File

func init() {
	units = map[int]*os.File{}
	units[6] = os.Stdout
}

func REWIND(unit int) {
	units[unit].Seek(0, 0)
}

func WRITE(unit int, format []byte, a ...interface{}) {

	for i := range a {
		if str, ok := a[i].([]byte); ok {
			a[i] = string(str)
		}
	}

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

func READ(unit *int, format []byte, a ...interface{}) {

	format = bytes.TrimSpace(bytes.ToLower(format))

	// Change from %15.4f to %15f
	for i := 1; i < 20; i++ {
		for j := 1; j <= i; j++ {
			format = bytes.Replace(format,
				[]byte(fmt.Sprintf("%c%d.%df", '%', i, j)),
				[]byte(fmt.Sprintf("%cf", '%')),
				-1)
		}
	}

	ft := string(format)
	_, err := fmt.Fscanf(units[*unit], ft, a...)
	if err != nil {
		var types string
		for i := range a {
			types += fmt.Sprintf("|%s|", reflect.TypeOf(a[i]))
		}
		panic(fmt.Errorf("READ error for format `%s` : %v\nValues = %v",
			ft, err, types))
	}
}
