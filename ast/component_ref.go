package ast

type Component_ref struct {
	Op0  string
	Op1  string
	Type string
}

func (a Component_ref) GenNodeName() string {
	return "component_ref"
}
func parse_component_ref(line string) (n Node) {
	return Component_ref{
		Op0:  findVal("op 0:", &line),
		Op1:  findVal("op 1:", &line),
		Type: findVal("type:", &line),
	}
}
