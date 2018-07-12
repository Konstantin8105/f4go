package ast

type Real_type struct {
	Algn string
	Name string
	Prec string
	Size string
}

func (a Real_type) GenNodeName() string {
	return "real_type"
}
func parse_real_type(line string) (n Node) {
	return Real_type{
		Algn: findVal("algn:", &line),
		Name: findVal("name:", &line),
		Prec: findVal("prec:", &line),
		Size: findVal("size:", &line),
	}
}
