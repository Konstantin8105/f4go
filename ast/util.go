package ast

import (
	"fmt"
	"strconv"
	"strings"
)

func findVal(pattern string, line *string) (out string) {
	if len(pattern) != 5 {
		panic(fmt.Errorf("Pattern must have 5 letters: `%s`", pattern))
	}
	if pattern[4] != ':' {
		panic(fmt.Errorf("Pattern must end on letter `:` : `%s`", pattern))
	}
	index := strings.Index(*line, pattern)
	if index < 0 {
		return ""
	}
	defer func() {
		out = strings.TrimSpace(out)
	}()
	// Example:
	// pattern : `algn:`
	// begin   : `algn: `
	// ----------------^
	begin := index + 5
	var ignoreSpace bool = true
	for i := begin; i < len(*line); i++ {
		if (*line)[i] != ' ' && ignoreSpace {
			ignoreSpace = false
			continue
		}
		if (*line)[i] == ' ' && ignoreSpace {
			continue
		}
		if (*line)[i] == ' ' {
			return (*line)[begin:i]
		}
	}
	return (*line)[begin:]
}

func IsLink(s string) (index int, ok bool) {
	if len(s) < 2 {
		return
	}
	if s[0] != '@' {
		return
	}
	var err error
	index, err = strconv.Atoi(s[1:])
	if err != nil {
		return
	}
	return index, true
}
