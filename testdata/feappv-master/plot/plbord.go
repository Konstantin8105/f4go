package main

type MEMORY struct {
	PDATA2 struct {
		ICLEAR *int
		IDEV   *int
		IDX    *int
		IDY    *int
		IPB    *int
		IFRM   *int
		ICOLR  *int
		ILNO   *int
	}
}

var COMMON MEMORY

// !$Id:$
func PLBORD(ICL *int) {
	ICLEAR := new(int)
	IDEV := new(int)
	IDX := new(int)
	IDY := new(int)
	IPB := new(int)
	IFRM := new(int)
	ICOLR := new(int)
	ILNO := new(int)
	COMMON.PDATA2.ILNO = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA2.ICOLR = new(float64)
	COMMON.PDATA2.IFRM = new(float64)
	COMMON.PDATA2.IPB = new(float64)
	COMMON.PDATA2.IDY = new(float64)
	COMMON.PDATA2.IDX = new(float64)
	COMMON.PDATA2.IDEV = new(float64)
	COMMON.PDATA2.ICLEAR = new(int)
	ICOL := new(int)
	ILN := new(int)
	ILNS := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Plot border for plots

	//!      Inputs:

	//!         icl       - Color for border

	//!      Outputs:

	//!         none      - Plot output to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	ICLEAR = COMMON.PDATA2.ICLEAR
	IDEV = COMMON.PDATA2.IDEV
	IDX = COMMON.PDATA2.IDX
	IDY = COMMON.PDATA2.IDY
	IPB = COMMON.PDATA2.IPB
	IFRM = COMMON.PDATA2.IFRM
	ICOLR = COMMON.PDATA2.ICOLR
	ILNO = COMMON.PDATA2.ILNO
	//!     Draw a border around mesh

	(*ICOL) = (*(ICL))
	PPPCOL(ICOL, func() *int { y := 0; return &y }())
	//!     Set line to solid for border

	ILNS(func() *int { y := 1; return &y }()) = (*ILNO(func() *int { y := 1; return &y }()))
	ILNS(func() *int { y := 2; return &y }()) = (*ILNO(func() *int { y := 2; return &y }()))
	ILN(func() *int { y := 1; return &y }()) = 0
	ILN(func() *int { y := 2; return &y }()) = 3
	PLLINE(ILN)
	PPBOX(func() *float64 { y := 0.0003e0; return &y }(), func() *float64 { y := 0.0100e0; return &y }(), func() *float64 { y := 1.279e0; return &y }(), func() *float64 { y := 0.9697e0; return &y }(), func() *int { y := 3; return &y }())
	//!     Place box lines on figure

	DPLOT(func() *float64 { y := 0.9700e0; return &y }(), func() *float64 { y := 0.0100e0; return &y }(), func() *int { y := 3; return &y }())
	DPLOT(func() *float64 { y := 0.9700e0; return &y }(), func() *float64 { y := 0.9796e0; return &y }(), func() *int { y := 2; return &y }())
	DPLOT(func() *float64 { y := 0.9700e0; return &y }(), func() *float64 { y := 0.8250e0; return &y }(), func() *int { y := 3; return &y }())
	DPLOT(func() *float64 { y := 1.2793e0; return &y }(), func() *float64 { y := 0.8250e0; return &y }(), func() *int { y := 2; return &y }())
	DPLOT(func() *float64 { y := 0.9700e0; return &y }(), func() *float64 { y := 0.0960e0; return &y }(), func() *int { y := 3; return &y }())
	DPLOT(func() *float64 { y := 1.2793e0; return &y }(), func() *float64 { y := 0.0960e0; return &y }(), func() *int { y := 2; return &y }())
	//!     Place lines for perspective view

	DPLOT(func() *float64 { y := 1.12465e0; return &y }(), func() *float64 { y := 0.8250e0; return &y }(), func() *int { y := 3; return &y }())
	DPLOT(func() *float64 { y := 1.12465e0; return &y }(), func() *float64 { y := 0.9796e0; return &y }(), func() *int { y := 2; return &y }())
	//!     Restore line type

	PLLINE(ILNS)
	ILNO(func() *int { y := 1; return &y }()) = (*ILNS(func() *int { y := 1; return &y }()))
	ILNO(func() *int { y := 2; return &y }()) = (*ILNS(func() *int { y := 2; return &y }()))
}
