package ast

type Boolean_type struct {
	Algn string
	Name string
	Size string
}

func (a Boolean_type) GenNodeName() string {
	return "boolean_type"
}
func parse_boolean_type(line string) (n Node) {
	return Boolean_type{
		Algn: findVal("algn:", &line),
		Name: findVal("name:", &line),
		Size: findVal("size:", &line),
	}
}
