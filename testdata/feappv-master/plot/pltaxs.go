package main

// !$Id:$
func PLTAXS(XI *int, NDM *int, CT *int) {
	M := new(int)
	N := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Draw vectors on screen for direction of coord. axes

	//!      Inputs:

	//!         xi(*)     - Location on origin for axes

	//!         ndm       - Spatial dimension of mesh

	//!         ct        - Size factor for plot

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: CT , FAC1 , FAC2 , FAC3
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: DD ( 3 , 3 ) , XX ( 3 , 5 ) , XI ( NDM )
	//!     Perspective projecton of axes

	PZERO(DD, func() *int { y := 9; return &y }())
	for (*M) = 1; (*M) <= (*(NDM)); (*M)++ {
		DD(M, M) = (*CT)
	}
	//!     Compute plot location for axes

	for (*M) = 1; (*M) <= (*(NDM)); (*M)++ {
		PZERO(XX, func() *int { y := 15; return &y }())
		for (*N) = 1; (*N) <= (*(NDM)); (*N)++ {
			(*FAC1) = (*DD(N, M))
			XX(N, func() *int { y := 1; return &y }()) = (*XI(N))
			XX(N, func() *int { y := 2; return &y }()) = XX(N, func() *int { y := 1; return &y }()) + (*FAC1)
			XX(N, func() *int { y := 5; return &y }()) = (*XX(N, func() *int { y := 2; return &y }()))
		}
		(*FAC1) = DD(func() *int { y := 1; return &y }(), M) * 0.1e0
		(*FAC2) = DD(func() *int { y := 2; return &y }(), M) * 0.1e0
		(*FAC3) = DD(func() *int { y := 3; return &y }(), M) * 0.1e0
		XX(func() *int { y := 1; return &y }(), func() *int { y := 3; return &y }()) = XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }()) - 3.e0*(*FAC1) - (*FAC2) - (*FAC3)
		XX(func() *int { y := 2; return &y }(), func() *int { y := 3; return &y }()) = XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }()) - 3.e0*(*FAC2) + (*FAC1) + (*FAC3)
		XX(func() *int { y := 3; return &y }(), func() *int { y := 3; return &y }()) = XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }()) - 3.e0*(*FAC3) + (*FAC1) + (*FAC2)
		XX(func() *int { y := 1; return &y }(), func() *int { y := 4; return &y }()) = XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }()) - 3.e0*(*FAC1) + (*FAC2) + (*FAC3)
		XX(func() *int { y := 2; return &y }(), func() *int { y := 4; return &y }()) = XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }()) - 3.e0*(*FAC2) - (*FAC1) - (*FAC3)
		XX(func() *int { y := 3; return &y }(), func() *int { y := 4; return &y }()) = XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }()) - 3.e0*(*FAC3) - (*FAC1) - (*FAC2)
		//!       Plot vector

		PLOTL(XX(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }()), XX(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }()), XX(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }()), func() *int { y := 3; return &y }())
		for (*N) = 2; (*N) <= 5; (*N)++ {
			PLOTL(XX(func() *int { y := 1; return &y }(), N), XX(func() *int { y := 2; return &y }(), N), XX(func() *int { y := 3; return &y }(), N), func() *int { y := 2; return &y }())
		}
		PLOTL(XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }()), XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }()), XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }()), func() *int { y := 3; return &y }())
		//!       Add label

		PLABL(M)
	}
}
