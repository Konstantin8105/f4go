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

func MAX(a, b interface{}) float64 {
	cast := func(w interface{}) float64 {
		switch v := w.(type) {
		case float64:
			return v
		case *float64:
			return *v
		case float32:
			return float64(v)
		case *float32:
			return float64(*v)
		case int:
			return float64(v)
		case *int:
			return float64(*v)
		default:
			panic(fmt.Errorf("cannot cast: %#v", w))
		}
	}

	A := cast(a)
	B := cast(b)

	if A > B {
		return A
	}
	return B
}

func EPSILON(f float64) float64 {
	return math.Pow(2, -23)
}

func CONJG(c complex128) complex128 {
	return complex128(cmplx.Conj(complex128(c)))
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
