package ast

type Real_cst struct {
	Type string
	Valu string
}

func (a Real_cst) GenNodeName() string {
	return "real_cst"
}
func parse_real_cst(line string) (n Node) {
	return Real_cst{
		Type: findVal("type:", &line),
		Valu: findVal("valu:", &line),
	}
}
