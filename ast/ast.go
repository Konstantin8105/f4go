package ast

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func ParseAST(treeFile string) (err error) {
	file, err := os.Open(treeFile)
	if err != nil {
		return
	}
	defer file.Close()

	var block []string

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line[0] == ' ' {
			block[len(block)-1] += " " + strings.TrimSpace(line)
			continue
		}
		if line[0] == '@' {
			block = append(block, line)
			continue
		}
		err = parseBlock(block)
		if err != nil {
			return
		}
		block = []string{line}
	}

	if err := scanner.Err(); err != nil {
		return err
	}

	return
}

func parseBlock(block []string) (err error) {
	if len(block) == 0 {
		return
	}
	fmt.Println("---------------------------------")

	for i, l := range block {

		if i == 0 {
			fmt.Println("NAME : ", l)
			continue
		}

		var n interface{}
		n, err = parse(l)
		if err != nil {
			return
		}
		fmt.Printf("\n")
		fmt.Println(l)
		fmt.Println("n: ", n)

		/*
			rr := strings.Split(strings.TrimSpace(l), " ")
			for _, t := range rr {
				t := strings.TrimSpace(t)
				if t == "" {
					continue
				}
				fmt.Printf("|%s|\n", t)
			}
		*/

	}

	return
}

func parse(line string) (n interface{}, err error) {
	if line[0] != '@' {
		err = fmt.Errorf("Not @")
		return
	}
	var index int

	// go to space
	for {
		if line[index] == ' ' {
			break
		}
		index++
	}
	// n.link = line[:index]

	// go to not space
	for {
		if line[index] != ' ' {
			break
		}
		index++
	}
	begin := index
	// go to space
	for {
		if line[index] == ' ' {
			break
		}
		index++
	}
	// n.name = line[begin:index]
	//
	// ll := strings.Split(line[index+1:], ":")
	// for _, r := range ll {
	// 	rr := strings.Split(strings.TrimSpace(r), " ")
	// 	for _, t := range rr {
	// 		t := strings.TrimSpace(t)
	// 		if t == "" {
	// 			continue
	// 		}
	// 		fmt.Println(t)
	// 	}
	// }

	switch line[begin:index] {
	case "identifier_node":
		n = parse_identifier_node(line[index:])
	default:
		fmt.Println("Undefine")
	}

	// for {
	// 	begin = index
	// 	// go to :
	// 	for {
	// 		if index == len(line) {
	// 			return
	// 		}
	// 		if line[index] == ':' {
	// 			break
	// 		}
	// 		index++
	// 	}
	// 	if index-begin == 0 {
	// 		index++
	// 		continue
	// 	}
	// 	var p property
	// 	p.key = line[begin:index]
	// 	p.value = line[index:]
	// 	fmt.Println("---- ", p.key, " ----- ", p.value)
	//
	// 	n.properties = append(n.properties, p)
	// }

	return
}
