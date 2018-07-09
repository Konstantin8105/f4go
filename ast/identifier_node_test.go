package ast

import (
	"fmt"
	"reflect"
	"testing"
)

func TestIdentifierNode(t *testing.T) {
	tcs := []struct {
		line     string
		expected identifier_node
	}{
		{
			line: "@26     identifier_node  strg: integer(kind=4)         lngt: 15   ",
			expected: identifier_node{
				link: "@26",
				strg: "integer(kind=4)",
				lngt: "15",
			},
		},
	}

	for index, tc := range tcs {
		t.Run(fmt.Sprintf("%d", index), func(t *testing.T) {
			actual := parse_identifier_node(tc.line)
			if !reflect.DeepEqual(actual, tc.expected) {
				t.Fatalf("Not equal:\nact:%s\nexp:%s", actual, tc.expected)
			}
		})
	}

}
