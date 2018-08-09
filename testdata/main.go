package main

import "fmt"
import "math"
//C -----------------------------------------------------
//C Tests
//C -----------------------------------------------------
func main() {
	//! begin of tests
	testName([]byte("test_operations"))
	test_operations()
	testName([]byte("test_pow"))
	test_pow()
	testName([]byte("real_test_name"))
	real_test_name()
	testName([]byte("test_subroutine"))
	test_subroutine()
	testName([]byte("test_if"))
	test_if()
	testName([]byte("test_do"))
	test_do()
	testName([]byte("test_do_while"))
	test_do_while()
	testName([]byte("test_array"))
	test_array()
	testName([]byte("test_goto"))
	test_goto()
	testName([]byte("test_function"))
	test_function()
	testName([]byte("test_data"))
	test_data()
	testName([]byte("test_matrix"))
	test_matrix()
	testName([]byte("test_types"))
	test_types()
	//! call testName("test_complex")
	//! call test_complex()
	//! end of tests
}

func testName(name []byte) {
	fmt.Printf("========== Test : %20s ==========\n", name)
	return
}

func fail(name []byte) {
	fmt.Printf("***** FAIL : %s\n", name)
	return
}
//C -----------------------------------------------------
//C -----------------------------------------------------
//C -----------------------------------------------------
//C -----------------------------------------------------
func real_test_name() {
}
//C -----------------------------------------------------
func test_operations() {
	var i int
	i = 12
	i = i + 12
	i = i - 20
	i = i * 2
	fmt.Printf(" output %1d integer\n", i)
	return
}
//C -----------------------------------------------------
func test_pow() {
	var r float64
	var p float64
	var real_1 float64
	H := make([]float64, 1)
	//! initialization
	//C calculation
	H[1-1] = 3
	H[1-1] = math.Pow(H[1-1], H[1-1])
	r = -9.e+4
	p = 1.45
	r = r / 5
	r = r + math.Pow(1.4, 2.1)
	r = -(r + math.Pow(p, p))
	r = math.Pow((r + p), (p - 0.6))
	real_1 = r / 5
	fmt.Printf("POW: %15.2f , %14.2f , %14.2f\n", r, real_1, H[1-1])
	return
}
//C -----------------------------------------------------
func test_subroutine() {
	var a float64
	var b float64
	a = 5
	b = 6
	fmt.Printf(" outpu1 %12.5f real \n", a)
	fmt.Printf(" outpu2 %12.5f real \n", b)
	ab(&(a), &(b))
	fmt.Printf(" outpu3 %12.5f real \n", a)
	fmt.Printf(" outpu4 %12.5f real \n", b)
	return
}

func ab(a *float64, b *float64) {
	*a = *a + 5.12
	*b = *b + 8.4
	return
}
//C -----------------------------------------------------
func test_if() {
	var i int
	i = 5
	if i == 5 {
		fmt.Printf(" %v\n", "Operation .EQ. is Ok")
	}
	if i >= 5 {
		fmt.Printf(" %v\n", "Operation .GE. is Ok")
	}
	if i <= 5 {
		fmt.Printf(" %v\n", "Operation .LE. is Ok")
	}
	if i >= 4 {
		fmt.Printf(" %v\n", "Operation .GE. is Ok")
	}
	if i <= 3 {
		fmt.Printf(" %v\n", "Operation .GE. is Ok")
	}
	if i != 3 {
		fmt.Printf(" %v\n", "Operation .NE. is Ok")
	}
	if i >= 100 {
		return
		//! stop the program
	} else if i == 5 {
		fmt.Printf(" %v\n", "ELSEIF is Ok")
	}
}
//C -----------------------------------------------------
//!       subroutine test_complex()
//!           complex c1,c2
//!           c1 = (1.1221,2.2)
//!           c2 = (3.23,5.666)
//!           c1 = c1 + c2
//!           write(*, fmt = 300) c1
//!           return
//! 300 Format (F15.4)
//!       end
//C -----------------------------------------------------
func test_do() {
	var IR int
	var JR int
	var HR int
	var iterator int
	JR = 1
	iterator = 1
	for IR = 1; IR <= 10; IR += 3 {
		fmt.Printf("Do with inc %2d\n", IR)
	}
	for IR = 1; IR <= 3; IR++ {
		fmt.Printf("Do %2d\n", IR)
	}
	for IR = 1; IR <= 3; IR++ {
		fmt.Printf("Do with continue %2d\n", IR)
	}
//Label143:
	;
Label144:
	;
	for IR = 1; IR <= 3; IR++ {
		fmt.Printf("Do with end do %2d\n", IR)
	}
	if ab_min(func()*int{y:=3;return &y}(), func()*int{y:=14;return &y}()) == 14 {
		fail([]byte("test_do 1"))
	}
	for IR = 1; IR <= ab_min(func()*int{y:=3;return &y}(), func()*int{y:=13;return &y}()); IR++ {
		fmt.Printf("Do with enddo %2d\n", IR)
	}
	for HR = 1; HR <= 2; HR++ {
		for JR = 1; JR <= 2; JR++ {
			fmt.Printf("Double DO %2d%2d\n", HR, JR)
		}
	}
//Label145:
	;
	fmt.Printf(" iterator = %2d\n", iterator)
	iterator = iterator + 1
	if iterator <= 3 {
		fmt.Printf(" %v\n", "iterator is less or equal 3")
		goto Label144
	}
	return
}

func ab_min(a *int, b *int) (ab_min_RES int) {
	if *a <= *b {
		ab_min_RES = *a
	} else {
		ab_min_RES = *b
	}
	return
}
//C -----------------------------------------------------
func test_do_while() {
	var iterator int
	iterator = 1
	for iterator <= 3 {
		fmt.Printf("Do while %2d\n", iterator)
		iterator = iterator + 1
	}
	return
}
//C -----------------------------------------------------
func test_array() {
	iterator := make([]int, 3)
	var ir int
	for ir = 1; ir <= 3; ir++ {
		iterator[ir-1] = ir
	}
	for ir = 1; ir <= 3; ir++ {
		fmt.Printf("vector %2d\n", iterator[ir-1])
	}
	if summator(iterator) != 6 {
		fail([]byte("test_array 1"))
	}
	return
}

func summator(s []int) (summator_RES int) {
	var ir int
	var sum int
	sum = 0
	for ir = 1; ir <= 3; ir++ {
		sum = sum + s[ir-1]
	}
	summator_RES = sum
	return
}
//C -----------------------------------------------------
func test_goto() {
	var t int
	var m int
	t = 0
	m = 0
Label230:
	;
	t = t + 1
Label240:
	;
	t = t + 5
	m = m + 1
	fmt.Printf("goto check t = %2d\n", t)
	if m >= 5 {
		goto Label250
	}
	fmt.Printf("goto check m = %2d\n", m)
	switch m {
	case 1:
		goto Label230
	case 2:
		goto Label240
	}
Label250:
	;
	return
}
//C -----------------------------------------------------
func test_function() {
	var ai int
	var bi string
	var l bool
	ai = 12
	bi = "rrr"
	fmt.Printf("test function integer = %9d array = %3s\n", ai, bi)
	l = function_changer(&(ai), &(bi))
	if l != true {
		fail([]byte("test function in logical"))
	}
	fmt.Printf("test function integer = %9d array = %3s\n", ai, bi)
	return
}

func function_changer(a *int, b *string) (function_changer_RES bool) {
	*a = 34
	*b = "www"
	function_changer_RES = true
	return
}
//C -----------------------------------------------------
func test_data() {
	var v float32
	var r int
	LOC12 := make([]int, 4)
	LOC21 := make([]int, 4)
	LOC12[0] = 3
	LOC12[1] = 4
	LOC12[2] = 1
	LOC12[3] = 2
	LOC21[0] = 2
	LOC21[1] = 1
	LOC21[2] = 4
	LOC21[3] = 3
	v = 23.23
	r = 25
	if r != 25 {
		fail([]byte("test_data 1"))
	}
	if !(23.0 <= v && v <= 23.5) {
		fail([]byte("test_data 2"))
	}
	if LOC12[1-1] != 3 {
		fail([]byte("test_data 3"))
	}
	if LOC12[2-1] != 4 {
		fail([]byte("test_data 4"))
	}
	if LOC12[3-1] != 1 {
		fail([]byte("test_data 5"))
	}
	if LOC12[4-1] != 2 {
		fail([]byte("test_data 6"))
	}
	if LOC21[1-1] != 2 {
		fail([]byte("test_data 7"))
	}
	if LOC21[2-1] != 1 {
		fail([]byte("test_data 8"))
	}
	if LOC21[3-1] != 4 {
		fail([]byte("test_data 9"))
	}
	if LOC21[4-1] != 3 {
		fail([]byte("test_data 10"))
	}
}
//C -----------------------------------------------------
func test_matrix() {
	M := make([][]int, 3)
	for u := 0; u < 3; u++ {
		M[u] = make([]int, 2)
	}
	var I int
	var J int
	for I = 1; I <= 3; I++ {
		for J = 1; J <= 2; J++ {
			M[I-1][J-1] = I*8 + J + (I-J)*5
		}
	}
	for I = 1; I <= 3; I++ {
		for J = 1; J <= 2; J++ {
			fmt.Printf("Matrix (%1d,%1d) = %2d\n", I, J, M[I-1][J-1])
		}
	}
	matrix_changer(M, func()*int{y:=3;return &y}(), func()*int{y:=2;return &y}())
	for I = 1; I <= 3; I++ {
		for J = 1; J <= 2; J++ {
			fmt.Printf("Matrix*(%1d,%1d) = %2d\n", I, J, M[I-1][J-1])
		}
	}
	return
}

func matrix_changer(M [][]int, IN *int, JN *int) {
	var I int
	var J int
	for I = 1; I <= *IN; I++ {
		for J = 1; J <= *JN; J++ {
			M[I-1][J-1] = M[I-1][J-1] + 2*(I+J)
		}
	}
	return
}
//C -----------------------------------------------------
func test_types() {
	var I8 int64
	var R1 float64
	var R4 float32
	var R8 float64
	var DP float64
	var I1 int
	var I2 int16
	var I4 int32
	R1 = 45.1
	R4 = 45.1
	R8 = 45.1
	DP = 45.1
	I1 = 12
	I2 = 12
	I4 = 12
	I8 = 12
	if 45.0 <= R1 && R1 <= 45.2 {
		fmt.Printf(" %v\n", "R1 ... ok")
	}
	if 45.0 <= R4 && R4 <= 45.2 {
		fmt.Printf(" %v\n", "R4 ... ok")
	}
	if 45.0 <= R8 && R8 <= 45.2 {
		fmt.Printf(" %v\n", "R8 ... ok")
	}
	if 45.0 <= DP && DP <= 45.2 {
		fmt.Printf(" %v\n", "DP ... ok")
	}
	if I1 == 12 {
		fmt.Printf(" %v\n", "I1 ... ok")
	}
	if I2 == 12 {
		fmt.Printf(" %v\n", "I2 ... ok")
	}
	if I4 == 12 {
		fmt.Printf(" %v\n", "I4 ... ok")
	}
	if I8 == 12 {
		fmt.Printf(" %v\n", "I8 ... ok")
	}
	return
}
