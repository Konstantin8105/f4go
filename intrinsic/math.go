package intrinsic

import (
	"fmt"
	"math"
	"math/cmplx"
)

func MIN(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func MAX(a, b float64) float64 {
	if a > b {
		return a
	}
	return b
}

func CONJG(c complex64) complex64 {
	return complex64(cmplx.Conj(complex128(c)))
}

func DCONJG(c complex128) complex128 {
	return cmplx.Conj(c)
}

func DBLE(a interface{}) float64 {
	switch a.(type) {
	case int:
		return float64(a.(int))
	case int32:
		return float64(a.(int32))
	case int64:
		return float64(a.(int64))
	case float32:
		return float64(a.(float32))
	case complex64:
		return float64(real(a.(complex64)))
	case complex128:
		return float64(real(a.(complex128)))
	case float64:
		return a.(float64)
	}
	panic(fmt.Errorf("Cannot find type : %T", a))
}

func ABS(a float64) float64 {
	return math.Abs(a)
}

func CABS(a complex128) float64 {
	return cmplx.Abs(a)
}

func SIGN(a float64) float64 {
	if a < 0.0 {
		return -1
	}
	return 1
}

func MOD(a, b int) int {
	return a % b
}
