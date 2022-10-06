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
func PFEAP(XL *int, YL *int, SIZ *int, COLOR *int, BORDER *int) {
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
	I := new(int)
	ICO1 := new(int)
	ICO2 := new(int)
	IS := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	IFX := new(int)
	IFY := new(int)
	IEX := new(int)
	IEY := new(int)
	ILN := new(int)
	IAX := new(int)
	IAY := new(int)
	IPX := new(int)
	IPY := new(int)
	IXL := new(int)
	IVX := new(int)
	IVY := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Put FEAPpv logo on plots

	//!      Inputs:

	//!         xl,yl     - Location to place logo

	//!         siz       - Size of logo

	//!         color     - Color for plot

	//!         border    - Border type: <2 = fill; >1 = line

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
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: XL , YL , DIXL , SIZ , SIZE , SIZESM
	//F4GO: NOT IMPLEMENTED :data IFX / 0 , 10 , 45 , 43 , 18 , 16 , 26 , 24 , 14 , 10 , 0 /
	//F4GO: NOT IMPLEMENTED :data IFY / 0 , 50 , 50 , 40 , 40 , 30 , 30 , 20 , 20 , 0 , 0 /
	//F4GO: NOT IMPLEMENTED :data IEX / 0 , 10 , 45 , 43 , 18 , 16 , 26 , 24 , 14 , 12 , 37 , 35 , 0 /
	//F4GO: NOT IMPLEMENTED :data IEY / 0 , 50 , 50 , 40 , 40 , 30 , 30 , 20 , 20 , 10 , 10 , 0 , 0 /
	//F4GO: NOT IMPLEMENTED :data IAX / 0 , 20 , 30 , 40 , 30 , 23 , 18 , 26 , 28 , 14 , 10 , 0 /
	//F4GO: NOT IMPLEMENTED :data IAY / 0 , 50 , 50 , 0 , 0 , 33 , 20 , 20 , 10 , 10 , 0 , 0 /
	//F4GO: NOT IMPLEMENTED :data IPX / 0 , 10 , 30 , 40 , 37 , 29 , 13 , 15 , 24 , 26 , 28 , 26 , 18 , 10 , 0 /
	//F4GO: NOT IMPLEMENTED :data IPY / 0 , 50 , 50 , 40 , 24 , 15 , 15 , 25 , 25 , 28 , 38 , 40 , 40 , 0 , 0 /
	//F4GO: NOT IMPLEMENTED :data IVX / 7 , 0 , 10 , 13 , 24 , 34 , 17 , 7 /
	//F4GO: NOT IMPLEMENTED :data IVY / 0 , 50 , 50 , 17 , 50 , 50 , 0 , 0 /
	//F4GO: NOT IMPLEMENTED :data IXL / 30 , 70 , 110 , 155 , 466 , 510 /
	//!     Save line type

	(*ICO1) = (*ILNO(func() *int { y := 1; return &y }()))
	(*ICO2) = (*ILNO(func() *int { y := 2; return &y }()))
	ILN(func() *int { y := 1; return &y }()) = 0
	ILN(func() *int { y := 2; return &y }()) = 1
	PLLINE(ILN)
	//!     Plot FEAP letters

	(*SIZE) = 200.0 / (*SIZ)
	if (*(BORDER)) <= 1 {
		(*IS) = 1
	} else {
		(*IS) = 3
	}
	//!     F

	PPPCOL((COLOR), func() *int { y := 1; return &y }())
	(*DIXL) = (*XL)*(*SIZE) + IXL(func() *int { y := 1; return &y }())
	DPLOT((IFX(func() *int { y := 1; return &y }())+(*DIXL))/(*SIZE), IFY(func() *int { y := 1; return &y }())/(*SIZE)+(*YL), IS)
	for (*I) = 2; (*I) <= 11; (*I)++ {
		DPLOT((IFX(I)+(*DIXL))/(*SIZE), IFY(I)/(*SIZE)+(*YL), func() *int { y := 2; return &y }())
	}
	if (*IS) == 1 {
		CLPAN()
	}
	//!     E

	(*DIXL) = (*XL)*(*SIZE) + IXL(func() *int { y := 2; return &y }())
	DPLOT((IEX(func() *int { y := 1; return &y }())+(*DIXL))/(*SIZE), IEY(func() *int { y := 1; return &y }())/(*SIZE)+(*YL), IS)
	for (*I) = 2; (*I) <= 13; (*I)++ {
		DPLOT((IEX(I)+(*DIXL))/(*SIZE), IEY(I)/(*SIZE)+(*YL), func() *int { y := 2; return &y }())
	}
	if (*IS) == 1 {
		CLPAN()
	}
	//!     A

	(*DIXL) = (*XL)*(*SIZE) + IXL(func() *int { y := 3; return &y }())
	DPLOT((IAX(func() *int { y := 1; return &y }())+(*DIXL))/(*SIZE), IAY(func() *int { y := 1; return &y }())/(*SIZE)+(*YL), IS)
	for (*I) = 2; (*I) <= 12; (*I)++ {
		DPLOT((IAX(I)+(*DIXL))/(*SIZE), IAY(I)/(*SIZE)+(*YL), func() *int { y := 2; return &y }())
	}
	if (*IS) == 1 {
		CLPAN()
	}
	//!     P

	PPPCOL((COLOR), func() *int { y := 1; return &y }())
	(*DIXL) = (*XL)*(*SIZE) + IXL(func() *int { y := 4; return &y }())
	DPLOT((IPX(func() *int { y := 1; return &y }())+(*DIXL))/(*SIZE), IPY(func() *int { y := 1; return &y }())/(*SIZE)+(*YL), IS)
	for (*I) = 2; (*I) <= 15; (*I)++ {
		DPLOT((IPX(I)+(*DIXL))/(*SIZE), IPY(I)/(*SIZE)+(*YL), func() *int { y := 2; return &y }())
	}
	if (*IS) == 1 {
		CLPAN()
	}
	//!     P

	PPPCOL((COLOR), func() *int { y := 1; return &y }())
	(*SIZESM) = (*SIZE) * 2.5e0
	(*DIXL) = (*XL)*(*SIZESM) + IXL(func() *int { y := 5; return &y }())
	DPLOT((IPX(func() *int { y := 1; return &y }())+(*DIXL))/(*SIZESM), IPY(func() *int { y := 1; return &y }())/(*SIZESM)+(*YL), IS)
	for (*I) = 2; (*I) <= 15; (*I)++ {
		DPLOT((IPX(I)+(*DIXL))/(*SIZESM), IPY(I)/(*SIZESM)+(*YL), func() *int { y := 2; return &y }())
	}
	if (*IS) == 1 {
		CLPAN()
	}
	//!     V

	PPPCOL((COLOR), func() *int { y := 1; return &y }())
	(*DIXL) = (*XL)*(*SIZESM) + IXL(func() *int { y := 6; return &y }())
	DPLOT((IVX(func() *int { y := 1; return &y }())+(*DIXL))/(*SIZESM), IVY(func() *int { y := 1; return &y }())/(*SIZESM)+(*YL), IS)
	for (*I) = 2; (*I) <= 8; (*I)++ {
		DPLOT((IVX(I)+(*DIXL))/(*SIZESM), IVY(I)/(*SIZESM)+(*YL), func() *int { y := 2; return &y }())
	}
	if (*IS) == 1 {
		CLPAN()
	}
	//!     Restore line type

	ILNO(func() *int { y := 1; return &y }()) = (*ICO1)
	ILNO(func() *int { y := 2; return &y }()) = (*ICO2)
}
