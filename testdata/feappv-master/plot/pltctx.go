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
	PDATXT struct {
		DTEXT *float64
		XSYC  *float64
		JX1   *int
		JY1   *int
	}
}

var COMMON MEMORY

// !$Id:$
func PLTCTX(VC *int, IC *int, IV *int, NC *int, MC *int) {
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
	DTEXT := new(float64)
	XSYC := new(float64)
	JX1 := new(int)
	JY1 := new(int)
	COMMON.PDATXT.JY1 = new(float64)
	COMMON.PDATXT.JX1 = new(float64)
	COMMON.PDATXT.XSYC = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATXT.DTEXT = new(int)
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	I := new(int)
	IVI := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	STRS := new(float64)
	SLAB := new(float64)
	YFR := new(float64)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Place contour line description on right hand side

	//!               of plot region.

	//!      Inputs:

	//!         vc(*)     - Values of contours to plotted

	//!         ic        - Component to plot

	//!         iv        - Color indicator for line

	//!         nc        - Number of contours

	//!         mc        - Plot type: 1 = stress;       2 = displacement;

	//!                                3 = velocity;     4 = acceleration;

	//!                                5 = prin. stress; 6 = streamline.

	//!                                7 = contact variables

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	ICLEAR = COMMON.PDATA2.ICLEAR
	IDEV = COMMON.PDATA2.IDEV
	IDX = COMMON.PDATA2.IDX
	IDY = COMMON.PDATA2.IDY
	IPB = COMMON.PDATA2.IPB
	IFRM = COMMON.PDATA2.IFRM
	ICOLR = COMMON.PDATA2.ICOLR
	ILNO = COMMON.PDATA2.ILNO
	DTEXT = COMMON.PDATXT.DTEXT
	XSYC = COMMON.PDATXT.XSYC
	JX1 = COMMON.PDATXT.JX1
	JY1 = COMMON.PDATXT.JY1
	//F4GO: NOT IMPLEMENTED :real ( KIND = 4 ) :: XPH , YPH
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: XDV , YCOR , XHEAD , YCOI
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: VC ( * ) , YFR ( 4 )
	//F4GO: NOT IMPLEMENTED :data STRS / " S T R E S S " , " DISPLACEMENT" , "  VELOCITY" , " ACCELERATION" , " PRIN. STRESS" , "  STREAMLINE " , " CONTACT VAR." /
	//F4GO: NOT IMPLEMENTED :data SLAB / "  1 " , "  2 " , "  3 " , " Ang" , " I_1" , " J_2" , " J_3" /
	//F4GO: NOT IMPLEMENTED :data YFR / 0.90e0 , 0.70e0 , 0.50e0 , 0.30e0 /
	(*XDV) = 20.e0
	if (*IFRM) != 0 {
		(*XDV) = 40.e0
	}
	(*YCOR) = 0.75e0
	if (*IFRM) != 0 {
		(*YCOR) = (*YFR(IFRM))
	}
	PPPCOL(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }())
	DPLOT(func() *float64 { y := 1.00e0; return &y }(), YCOR, func() *int { y := 3; return &y }())
	// Unused by f4go :  write ( CI , "(i2)" ) IC
	fmt.Println("WRITE SOMETHING")
	//!     X11 and DOS devices

	if (*(MC)) == 5 {
		// Unused by f4go :  write ( YY , "(a13,a4)" ) STRS ( MC ) , SLAB ( MIN ( 7 , IC ) )
		fmt.Println("WRITE SOMETHING")
	} else {
		// Unused by f4go :  write ( YY , "(a13,i2)" ) STRS ( MC ) , IC
		fmt.Println("WRITE SOMETHING")
	}
	(*XPH) = 1.0 / 1.28
	(*YPH) = (*real((*YCOR) / 1.28e0))
	(*XHEAD) = 1.00e0
	(*DTEXT) = 0.06e0
	PPPCOL(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }())
	TPLOT(XHEAD, YCOR, YY, func() *int { y := 17; return &y }(), func() *int { y := 1; return &y }())
	for (*I) = 1; (*I) <= (*(NC)); (*I)++ {
		(*IVI) = (*(IV)) + (*I)
		PPPCOL(IVI, func() *int { y := 1; return &y }())
		DPLOT(func() *float64 { y := 1.02e0; return &y }(), (*YCOR)-(*IVI)/(*XDV), func() *int { y := 3; return &y }())
		// Unused by f4go :  write ( YY , "(i3,1p1e11.3)" ) IVI , VC ( I )
		fmt.Println("WRITE SOMETHING")
		//!       PHIGS or X11 devices

		(*XHEAD) = 1.00e0
		(*YCOI) = (*YCOR) - (*IVI)/(*XDV)
		(*XPH) = 1.02 / 1.28
		(*YPH) = (*real((*YCOI) / 1.28e0))
		TPLOT(XHEAD, YCOI, YY, func() *int { y := 14; return &y }(), func() *int { y := 1; return &y }())
	}
}
