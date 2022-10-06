package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	PLCLIP struct {
		IENTER *int
		IEXIT  *int
		FWIN   *bool
		CLCHK  *bool
		FWOFF  *bool
		PSOFF  *bool
		CMIN   *float64
		CMAX   *float64
		XC     *float64
		YC     *float64
	}
	PDATA1 struct {
		SCALEF *float64
		SCALEG *float64
		S0     *float64
		DX     *float64
		SX     *float64
		FACT   *float64
	}
	PPERS struct {
		E     *float64
		EOLD  *float64
		TG    *float64
		VOLD  *float64
		Q     *float64
		XLBDA *float64
		ENORM *float64
		KPERS *int
	}
	PVIEW struct {
		CS    *float64
		ZVIEW *float64
		IVIEW *int
		LVIEW *bool
	}
}

var COMMON MEMORY

// !$Id:$
func PLOTL(XX1 *int, XX2 *int, XX3 *int, IPEN *int) {
	CMIN := new(float64)
	CMAX := new(float64)
	XC := new(float64)
	YC := new(float64)
	IENTER := new(int)
	IEXIT := new(int)
	FWIN := new(bool)
	CLCHK := new(bool)
	FWOFF := new(bool)
	PSOFF := new(bool)
	COMMON.PLCLIP.YC = func() *[]float64 {
		arr := make([]float64, 4)
		return &arr
	}()
	COMMON.PLCLIP.XC = func() *[]float64 {
		arr := make([]float64, 4)
		return &arr
	}()
	COMMON.PLCLIP.CMAX(3) = new(float64)
	COMMON.PLCLIP.CMIN(3) = new(int)
	COMMON.PLCLIP.PSOFF = new(float64)
	COMMON.PLCLIP.FWOFF = new(float64)
	COMMON.PLCLIP.CLCHK = new(float64)
	COMMON.PLCLIP.FWIN = new(float64)
	COMMON.PLCLIP.IEXIT = new(float64)
	COMMON.PLCLIP.IENTER = new(int)
	SCALEF := new(float64)
	SCALEG := new(float64)
	S0 := new(float64)
	DX := new(float64)
	SX := new(float64)
	FACT := new(float64)
	COMMON.PDATA1.FACT = new(float64)
	COMMON.PDATA1.SX = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA1.DX(2) = new(float64)
	COMMON.PDATA1.S0 = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA1.SCALEG = new(float64)
	COMMON.PDATA1.SCALEF = new(int)
	E := new(float64)
	EOLD := new(float64)
	TG := new(float64)
	VOLD := new(float64)
	Q := new(float64)
	XLBDA := new(float64)
	ENORM := new(float64)
	KPERS := new(int)
	COMMON.PPERS.KPERS = new(float64)
	COMMON.PPERS.ENORM = new(float64)
	COMMON.PPERS.XLBDA = func() *[][]float64 {
		arr := make([][]float64, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([]float64, 3)
		}
		return &arr
	}()
	COMMON.PPERS.Q = func() *[][]float64 {
		arr := make([][]float64, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([]float64, 3)
		}
		return &arr
	}()
	COMMON.PPERS.VOLD = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PPERS.TG = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PPERS.EOLD = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PPERS.E = func() *[]int {
		arr := make([]int, 3)
		return &arr
	}()
	CS := new(float64)
	ZVIEW := new(float64)
	IVIEW := new(int)
	LVIEW := new(bool)
	COMMON.PVIEW.LVIEW = new(float64)
	COMMON.PVIEW.IVIEW = new(float64)
	COMMON.PVIEW.ZVIEW = new(float64)
	COMMON.PVIEW.CS = new(int)
	ERRV := new(bool)
	NTYP := new(int)
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

	//!      Purpose: Line drawing command: Converts mesh coordinates to

	//!               screen coordinate values

	//!      Inputs:

	//!         xx1       - X-coordinate for plot

	//!         xx2       - Y-coordinate for plot

	//!         xx3       - Z-coordinate for plot

	//!         ipen      - Pen position

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	CMIN = COMMON.PLCLIP.CMIN
	CMAX = COMMON.PLCLIP.CMAX
	XC = COMMON.PLCLIP.XC
	YC = COMMON.PLCLIP.YC
	IENTER = COMMON.PLCLIP.IENTER
	IEXIT = COMMON.PLCLIP.IEXIT
	FWIN = COMMON.PLCLIP.FWIN
	CLCHK = COMMON.PLCLIP.CLCHK
	FWOFF = COMMON.PLCLIP.FWOFF
	PSOFF = COMMON.PLCLIP.PSOFF
	SCALEF = COMMON.PDATA1.SCALEF
	SCALEG = COMMON.PDATA1.SCALEG
	S0 = COMMON.PDATA1.S0
	DX = COMMON.PDATA1.DX
	SX = COMMON.PDATA1.SX
	FACT = COMMON.PDATA1.FACT
	E = COMMON.PPERS.E
	EOLD = COMMON.PPERS.EOLD
	TG = COMMON.PPERS.TG
	VOLD = COMMON.PPERS.VOLD
	Q = COMMON.PPERS.Q
	XLBDA = COMMON.PPERS.XLBDA
	ENORM = COMMON.PPERS.ENORM
	KPERS = COMMON.PPERS.KPERS
	CS = COMMON.PVIEW.CS
	ZVIEW = COMMON.PVIEW.ZVIEW
	IVIEW = COMMON.PVIEW.IVIEW
	LVIEW = COMMON.PVIEW.LVIEW
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: XX1 , XX2 , XX3 , X1 , X2 , X3 , S1 , S2
	(*NTYP) = 1
	//!     Recover plot coordinates

	(*X1) = (*XX1)
	(*X2) = (*XX2)
	(*X3) = (*XX3)
	//!     Perform perspective tranformation

	if (*KPERS) == 1 {
		XG(func() *int { y := 1; return &y }()) = (*X1)
		XG(func() *int { y := 2; return &y }()) = (*X2)
		XG(func() *int { y := 3; return &y }()) = (*X3)
		PERSPJ(XG, NTYP, func() *int { y := 1; return &y }(), ERRV)
		if *LVIEW {
			return
		}
		(*X1) = (*XG(func() *int { y := 1; return &y }()))
		(*X2) = (*XG(func() *int { y := 2; return &y }()))
		(*X3) = (*XG(func() *int { y := 3; return &y }()))
	}
	//!     Compute the normal coordinates

	(*S1) = (*SCALEF)*((*X1)+(*X1)-SX(func() *int { y := 1; return &y }())) + S0(func() *int { y := 1; return &y }())
	(*S2) = (*SCALEF)*((*X2)+(*X2)-SX(func() *int { y := 2; return &y }())) + S0(func() *int { y := 2; return &y }())
	(*S1) = (intrinsic.MAX(func() *float64 { y := 0.0e0; return &y }(), intrinsic.MIN(func() *float64 { y := 1.45e0; return &y }(), (*S1))))
	(*S2) = (intrinsic.MAX(func() *float64 { y := 0.0e0; return &y }(), intrinsic.MIN(func() *float64 { y := 1.00e0; return &y }(), (*S2))))
	(*CLCHK) = true
	DPLOT(S1, S2, (IPEN))
	(*CLCHK) = false
}
