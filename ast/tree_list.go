package ast

type Tree_list struct {
	Chan string
	Valu string
}

func (a Tree_list) GenNodeName() string {
	return "tree_list"
}
func parse_tree_list(line string) (n Node) {
	return Tree_list{
		Chan: findVal("chan:", &line),
		Valu: findVal("valu:", &line),
	}
}
