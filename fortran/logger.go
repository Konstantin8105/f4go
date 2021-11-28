package fortran

import (
	"fmt"
	"io"
	"os"
	"strings"
)

var (
	Verbose int
	writer  io.Writer = os.Stdout
)

func Debugf(format string, a ...interface{}) {
	if Verbose >= 3 {
		logf(format, a...)
	}
}

func Infof(format string, a ...interface{}) {
	if Verbose >= 2 {
		logf(format, a...)
	}
}

func Logf(format string, a ...interface{}) {
	if Verbose >= 1 {
		logf(format, a...)
	}
}

func Errorf(format string, a ...interface{}) {
	logf(format, a...)
}

// Ensures log prints with newline at end
func logf(format string, a ...interface{}) {
	if strings.HasSuffix(format, "\n") {
		fmt.Fprintf(writer, format, a...)
	} else {
		fmt.Fprintf(writer, format+"\n", a...)
	}
}
