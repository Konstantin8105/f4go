package ast

type Pointer_type struct {
	Algn string
	Ptd  string
	Size string
}

func (a Pointer_type) GenNodeName() string {
	return "pointer_type"
}
func parse_pointer_type(line string) (n Node) {
	return Pointer_type{
		Algn: findVal("algn:", &line),
		Ptd:  findVal("ptd :", &line),
		Size: findVal("size:", &line),
	}
}
