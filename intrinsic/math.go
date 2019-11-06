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

func castToFloat64(w interface{}) float64 {
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
		// 	case complex128:
		// 		r := real(v)
		// 		i := imag(v)
		// 		return float64(math.Mod(r, 2) - math.Mod(i, 2))
		// 	case *complex128:
		// 		r := real(*v)
		// 		i := imag(*v)
		// 		return float64(math.Mod(r, 2) - math.Mod(i, 2))
	default:
		panic(fmt.Errorf("cannot cast: %#v", w))
	}
}

func SQRT(a interface{}) float64 {
	A := castToFloat64(a)
	return math.Sqrt(A)
}

func MAX(a, b interface{}) float64 {
	A := castToFloat64(a)
	B := castToFloat64(b)
	if A > B {
		return A
	}
	return B
}

func EPSILON(f float64) float64 {
	return math.Pow(2, -23)
}

func CONJG(c complex128) *complex128 {
	v := complex128(cmplx.Conj(complex128(c)))
	return &v
}

func DCONJG(c complex128) *complex128 {
	v := cmplx.Conj(c)
	return &v
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
