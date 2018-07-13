package ast

import (
	"fmt"
	"reflect"
	"testing"
)

func TestStringCst(t *testing.T) {
	tcs := []struct {
		line     string
		expected String_cst
	}{
		{
			line: "type: @94     strg: Hello, world!  lngt: 13     ",
			expected: String_cst{
				Type: "@94",
				Strg: "Hello, world!",
				Lngt: "13",
			},
		},
	}

	for index, tc := range tcs {
		t.Run(fmt.Sprintf("%d", index), func(t *testing.T) {
			actual := parse_string_cst(tc.line)
			if !reflect.DeepEqual(actual, tc.expected) {
				t.Fatalf("Not equal:\nact:%s\nexp:%s", actual, tc.expected)
			}
		})
	}

}
