package ast

type Indirect_ref struct {
	Op0  string
	Type string
}

func (a Indirect_ref) GenNodeName() string {
	return "indirect_ref"
}
func parse_indirect_ref(line string) (n Node) {
	return Indirect_ref{
		Op0:  findVal("op 0:", &line),
		Type: findVal("type:", &line),
	}
}
