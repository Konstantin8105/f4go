package main

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
}

var COMMON MEMORY

// !$Id:$
func LCLIP(IX *int, NEN *int, X *int, NDM *int) (LCLIP_RETURN *bool) {
	LCLIP_RETURN = new(bool)
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
	I := new(int)
	N := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Clip plot at specified planes

	//!      Inputs:

	//!         ix(*)     - List of nodes on element

	//!         nen       - Number of nodes on element

	//!         x(ndm,*)  - Nodal coordinates of element

	//!         ndm       - Spatial dimension of mesh

	//!      Outputs:

	//!         lclip     - Flag, true if element is within clip region

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
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X ( NDM , * ) , X0 ( 3 )
	for (*I) = 1; (*I) <= (*(NDM)); (*I)++ {
		X0(I) = 0.0e0
		for (*N) = 1; (*N) <= (*(NEN)); (*N)++ {
			if (*(IX)(N)) > 0 {
				X0(I) = X0(I) + X(I, (IX)(N))
			}
		}
		X0(I) = X0(I) / (*(NEN))
	}
	if (*(NDM)) == 1 {
		(*(LCLIP_RETURN)) = (X0(func() *int { y := 1; return &y }()) >= CMIN(func() *int { y := 1; return &y }())) && (X0(func() *int { y := 1; return &y }()) <= CMAX(func() *int { y := 1; return &y }()))
	} else if (*(NDM)) == 2 {
		(*(LCLIP_RETURN)) = (X0(func() *int { y := 1; return &y }()) >= CMIN(func() *int { y := 1; return &y }())) && (X0(func() *int { y := 1; return &y }()) <= CMAX(func() *int { y := 1; return &y }())) && (X0(func() *int { y := 2; return &y }()) >= CMIN(func() *int { y := 2; return &y }())) && (X0(func() *int { y := 2; return &y }()) <= CMAX(func() *int { y := 2; return &y }()))
	} else if (*(NDM)) == 3 {
		(*(LCLIP_RETURN)) = (X0(func() *int { y := 1; return &y }()) >= CMIN(func() *int { y := 1; return &y }())) && (X0(func() *int { y := 1; return &y }()) <= CMAX(func() *int { y := 1; return &y }())) && (X0(func() *int { y := 2; return &y }()) >= CMIN(func() *int { y := 2; return &y }())) && (X0(func() *int { y := 2; return &y }()) <= CMAX(func() *int { y := 2; return &y }())) && (X0(func() *int { y := 3; return &y }()) >= CMIN(func() *int { y := 3; return &y }())) && (X0(func() *int { y := 3; return &y }()) <= CMAX(func() *int { y := 3; return &y }()))
	} else {
		(*(LCLIP_RETURN)) = false
	}
}
