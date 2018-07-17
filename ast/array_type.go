package ast

type Array_type struct {
	Algn string
	Domn string
	Elts string
	Name string
	Size string
}

func (a Array_type) GenNodeName() string {
	return "array_type"
}
func parse_array_type(line string) (n Node) {
	return Array_type{
		Algn: findVal("algn:", &line),
		Domn: findVal("domn:", &line),
		Elts: findVal("elts:", &line),
		Name: findVal("name:", &line),
		Size: findVal("size:", &line),
	}
}
