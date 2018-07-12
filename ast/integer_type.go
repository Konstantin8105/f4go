package ast

type Integer_type struct {
	Algn string
	Max  string
	Min  string
	Name string
	Prec string
	Sign string
	Size string
}

func (a Integer_type) GenNodeName() string {
	return "integer_type"
}
func parse_integer_type(line string) (n Node) {
	return Integer_type{
		Algn: findVal("algn:", &line),
		Max:  findVal("max :", &line),
		Min:  findVal("min :", &line),
		Name: findVal("name:", &line),
		Prec: findVal("prec:", &line),
		Sign: findVal("sign:", &line),
		Size: findVal("size:", &line),
	}
}
