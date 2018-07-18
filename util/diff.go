package util

import (
	"fmt"
	"io/ioutil"
	"math"
	"os"
	"strconv"
	"strings"
)

func IsDiff(filename string, actual string) (out string, err error) {
	if _, err = os.Stat(filename); err == nil {
		// file is exist
		var expect []byte
		expect, err = ioutil.ReadFile(filename)
		if err != nil {
			return
		}

		out = ShowDiff(string(expect), actual)
		return
	}

	// file is not exist
	err = ioutil.WriteFile(filename, []byte(actual), 0644)
	return
}

// ShowDiff will print two strings vertically next to each other so that line
// differences are easier to read.
func ShowDiff(a, b string) (out string) {
	aLines := strings.Split(a, "\n")
	bLines := strings.Split(b, "\n")
	maxLines := int(math.Max(float64(len(aLines)), float64(len(bLines))))

	for lineNumber := 0; lineNumber < maxLines; lineNumber++ {
		aLine := ""
		bLine := ""

		// Replace NULL characters with a dot. Otherwise the strings will look
		// exactly the same but have different length (and therfore not be
		// equal).
		if lineNumber < len(aLines) {
			aLine = strconv.Quote(aLines[lineNumber])
		}
		if lineNumber < len(bLines) {
			bLine = strconv.Quote(bLines[lineNumber])
		}

		diffFlag := " "
		if aLine != bLine {
			diffFlag = "*"
		} else {
			continue
		}
		out += fmt.Sprintf("%s %3d %-40s%s\n", diffFlag, lineNumber+1, aLine, bLine)
		break
	}

	if out != "" {
		out = "\n" + out
	}

	return out
}
