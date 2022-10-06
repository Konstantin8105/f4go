package main

type MEMORY struct {
	PDATA5 struct {
		EXORD *int
		EPORD *int
	}
	PDATA6 struct {
		INORD *int
		IPORD *int
		IPU   *int
	}
}

var COMMON MEMORY

// !$Id:$
func PLQUD8(IEL *int) {
	EXORD := new(int)
	EPORD := new(int)
	COMMON.PDATA5.EPORD = func() *[][]float64 {
		arr := make([][]float64, 50)
		for u := 0; u < 50; u++ {
			arr[u] = make([]float64, 50)
		}
		return &arr
	}()
	COMMON.PDATA5.EXORD = func() *[]int {
		arr := make([]int, 50)
		return &arr
	}()
	INORD := new(int)
	IPORD := new(int)
	IPU := new(int)
	COMMON.PDATA6.IPU = func() *[][]float64 {
		arr := make([][]float64, 4)
		for u := 0; u < 4; u++ {
			arr[u] = make([]float64, 200)
		}
		return &arr
	}()
	COMMON.PDATA6.IPORD = func() *[][]float64 {
		arr := make([][]float64, 200)
		for u := 0; u < 200; u++ {
			arr[u] = make([]float64, 50)
		}
		return &arr
	}()
	COMMON.PDATA6.INORD = func() *[]int {
		arr := make([]int, 50)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: 2-D Plot Sequence for 4-node quadrilateral elements

	//!      Inputs:

	//!         iel       - Element type number

	//!      Outputs:

	//!         none      - Plot output to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	EXORD = COMMON.PDATA5.EXORD
	EPORD = COMMON.PDATA5.EPORD
	INORD = COMMON.PDATA6.INORD
	IPORD = COMMON.PDATA6.IPORD
	IPU = COMMON.PDATA6.IPU
	//!     Set number of points

	if (*(IEL)) > 0 {
		INORD((IEL)) = 9
		//!       Set plot sequence

		IPORD(func() *int { y := 1; return &y }(), (IEL)) = 1
		IPORD(func() *int { y := 2; return &y }(), (IEL)) = 5
		IPORD(func() *int { y := 3; return &y }(), (IEL)) = 2
		IPORD(func() *int { y := 4; return &y }(), (IEL)) = 6
		IPORD(func() *int { y := 5; return &y }(), (IEL)) = 3
		IPORD(func() *int { y := 6; return &y }(), (IEL)) = 7
		IPORD(func() *int { y := 7; return &y }(), (IEL)) = 4
		IPORD(func() *int { y := 8; return &y }(), (IEL)) = 8
		IPORD(func() *int { y := 9; return &y }(), (IEL)) = 1
	} else if (*(IEL)) < 0 {
		EXORD(-(*(IEL))) = 9
		//!       Set plot sequence

		EPORD(func() *int { y := 1; return &y }(), -(*(IEL))) = 1
		EPORD(func() *int { y := 2; return &y }(), -(*(IEL))) = 5
		EPORD(func() *int { y := 3; return &y }(), -(*(IEL))) = 2
		EPORD(func() *int { y := 4; return &y }(), -(*(IEL))) = 6
		EPORD(func() *int { y := 5; return &y }(), -(*(IEL))) = 3
		EPORD(func() *int { y := 6; return &y }(), -(*(IEL))) = 7
		EPORD(func() *int { y := 7; return &y }(), -(*(IEL))) = 4
		EPORD(func() *int { y := 8; return &y }(), -(*(IEL))) = 8
		EPORD(func() *int { y := 9; return &y }(), -(*(IEL))) = 1
	}
}
