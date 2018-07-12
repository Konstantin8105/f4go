package ast

type Array_ref struct {
	Op0  string
	Op1  string
	Type string
}

func (a Array_ref) GenNodeName() string {
	return "array_ref"
}
func parse_array_ref(line string) (n Node) {
	return Array_ref{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
