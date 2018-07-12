package ast

type FunctionLine struct {
	Line string
}

func (f FunctionLine) GenNodeName() string {
	return f.Line
}
