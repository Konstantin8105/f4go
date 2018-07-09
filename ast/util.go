package ast

import (
	"regexp"
	"strconv"
	"strings"
	"sync"
)

func groupsFromRegex(rx, line string) map[string]string {
	// We remove tabs and newlines from the regex. This is purely cosmetic,
	// as the regex input can be quite long and it's nice for the caller to
	// be able to format it in a more readable way.
	fullRegexp := strings.Replace(strings.Replace(rx, "\n", "", -1), "\t", "", -1)
	rx = fullRegexp

	re := GetRegex(rx)

	match := re.FindStringSubmatch(line)
	if len(match) == 0 {
		panic("could not match regexp with string\n" + rx + "\n" + line + "\n")
	}

	result := make(map[string]string)
	for i, name := range re.SubexpNames() {
		if i != 0 {
			result[name] = match[i]
		}
	}

	return result
}

// cachedRegex - structure for saving regexp`s
type cachedRegex struct {
	sync.RWMutex
	m map[string]*regexp.Regexp
}

// Global variable
var cr = cachedRegex{m: map[string]*regexp.Regexp{}}

// GetRegex return regexp
// added for minimaze regexp compilation
func GetRegex(rx string) *regexp.Regexp {
	cr.RLock()
	v, ok := cr.m[rx]
	cr.RUnlock()
	if ok {
		return v
	}
	// if regexp is not in map
	cr.Lock()
	cr.m[rx] = regexp.MustCompile(rx)
	cr.Unlock()
	return GetRegex(rx)
}

func IsLink(s string) (index int, ok bool) {
	if len(s) < 2 {
		return
	}
	var err error
	index, err = strconv.Atoi(s[1:])
	if err != nil {
		return
	}
	return index, true
}
