package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	PSDAT5 struct {
		XOLD  *int
		YOLD  *int
		DOLD  *int
		LWOLD *int
	}
	PSDAT6 struct {
		XLL   *int
		YLL   *int
		XUR   *int
		YUR   *int
		PSCAL *float64
	}
	PLFLAG struct {
		PLOUT  *bool
		BORDFL *bool
		SCREFL *bool
		EVERON *bool
		BLK    *bool
		CLIP   *bool
		LSTRK  *bool
		LFILL  *bool
	}
	PLPOST struct {
		NXTCHR *int
		BUFFER *byte
	}
	PSDAT2 struct {
		CLIN  *[ ]byte
		CVAR  *[ ]byte
		COLV  *[ ]byte
		OCLIN *[ ]byte
		OCVAR *[ ]byte
		OCOLV *[ ]byte
	}
}

var COMMON MEMORY
//!$Id:$
func FPTPLT(XP *int, YP *int, TX *int, NN *int, NCTR *int) {
	BORDFL := new(bool)
	SCREFL := new(bool)
	EVERON := new(bool)
	BLK := new(bool)
	CLIP := new(bool)
	LSTRK := new(bool)
	LFILL := new(bool)
	COMMON.PLFLAG.LFILL = new(float64)
	COMMON.PLFLAG.LSTRK = new(float64)
	COMMON.PLFLAG.CLIP = new(float64)
	COMMON.PLFLAG.BLK = new(float64)
	COMMON.PLFLAG.EVERON = new(float64)
	COMMON.PLFLAG.SCREFL = new(float64)
	COMMON.PLFLAG.BORDFL = new(int)
	PLOUT := new(bool)
	COMMON.PLFLAG.PLOUT = new(int)
	IBUFSZ := new(int)
	NXTCHR := new(int)
	BUFFER := new(byte)
	COMMON.PLPOST.BUFFER = func() *[]float64 {
		arr := make([]float64, 80)
		return &arr
	}()
	COMMON.PLPOST.NXTCHR = new(int)
	CLIN := func() *[]byte {
		arr := make([]byte, 3)
		return &arr
	}()
	CVAR := func() *[]byte {
		arr := make([]byte, 4)
		return &arr
	}()
	COLV := func() *[]byte {
		arr := make([]byte, 2)
		return &arr
	}()
	OCLIN := func() *[]byte {
		arr := make([]byte, 3)
		return &arr
	}()
	OCVAR := func() *[]byte {
		arr := make([]byte, 4)
		return &arr
	}()
	OCOLV := func() *[]byte {
		arr := make([]byte, 2)
		return &arr
	}()
	COMMON.PSDAT2.OCOLV = new(float64)
	COMMON.PSDAT2.OCVAR = new(float64)
	COMMON.PSDAT2.OCLIN = new(float64)
	COMMON.PSDAT2.COLV = new(float64)
	COMMON.PSDAT2.CVAR = new(float64)
	COMMON.PSDAT2.CLIN = new(int)
	XOLD := new(int)
	YOLD := new(int)
	DOLD := new(int)
	LWOLD := new(int)
	COMMON.PSDAT5.LWOLD = new(float64)
	COMMON.PSDAT5.DOLD = new(float64)
	COMMON.PSDAT5.YOLD = new(float64)
	COMMON.PSDAT5.XOLD = new(int)
	XLL := new(int)
	YLL := new(int)
	XUR := new(int)
	YUR := new(int)
	PSCAL := new(float64)
	COMMON.PSDAT6.PSCAL = new(float64)
	COMMON.PSDAT6.YUR = new(float64)
	COMMON.PSDAT6.XUR = new(float64)
	COMMON.PSDAT6.YLL = new(float64)
	COMMON.PSDAT6.XLL = new(int)
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	J := new(int)
	X := new(int)
	Y := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Subroutine to place text into PostScript file

	//!      Inputs:

	//!         xp(2)     - X-location to start placing text

	//!         yp(2)     - Y-location to start placing text

	//!         tx        - Character array qith text

	//!         nn        - Number of characters in text

	//!         nctr      - Text counter value

	//!      Outputs:

	//!         none      - Output written to PostScript file

	//!-----[--.----+----.----+----.-----------------------------------------]

	BORDFL = COMMON.PLFLAG.BORDFL
	SCREFL = COMMON.PLFLAG.SCREFL
	EVERON = COMMON.PLFLAG.EVERON
	BLK = COMMON.PLFLAG.BLK
	CLIP = COMMON.PLFLAG.CLIP
	LSTRK = COMMON.PLFLAG.LSTRK
	LFILL = COMMON.PLFLAG.LFILL
	PLOUT = COMMON.PLFLAG.PLOUT
	(*IBUFSZ) = 80
	NXTCHR = COMMON.PLPOST.NXTCHR
	BUFFER = COMMON.PLPOST.BUFFER
	CLIN = COMMON.PSDAT2.CLIN
	CVAR = COMMON.PSDAT2.CVAR
	COLV = COMMON.PSDAT2.COLV
	OCLIN = COMMON.PSDAT2.OCLIN
	OCVAR = COMMON.PSDAT2.OCVAR
	OCOLV = COMMON.PSDAT2.OCOLV
	XOLD = COMMON.PSDAT5.XOLD
	YOLD = COMMON.PSDAT5.YOLD
	DOLD = COMMON.PSDAT5.DOLD
	LWOLD = COMMON.PSDAT5.LWOLD
	XLL = COMMON.PSDAT6.XLL
	YLL = COMMON.PSDAT6.YLL
	XUR = COMMON.PSDAT6.XUR
	YUR = COMMON.PSDAT6.YUR
	PSCAL = COMMON.PSDAT6.PSCAL
	//F4GO: NOT IMPLEMENTED :real ( KIND = 4 ) :: XP ( * ) , YP ( * )
	//!     Close out a stroke if necessary

	if *LSTRK {
		FPPSIN(func()*byte{y:=byte('s');return &y}())
	}
	FPPSDU()
	//!       Place text on plot

	FPPSIN(func()*byte{y:=byte('H');return &y}())
	FPPSDU()
	//!       Position text

	(*X) = (*NINT(5400.0*XP(func()*int{y:=2;return &y}()) + 360.0))
	(*Y) = (*NINT(5400.0*YP(func()*int{y:=2;return &y}()) + 360.0))
	(*XLL) = (intrinsic.MIN((*X), (*XLL)))
	(*YLL) = (intrinsic.MIN((*Y), (*YLL)))
	(*XUR) = intrinsic.MAX((*X), (*XUR)) + NINT(2.5*FLOAT((NN)))
	(*YUR) = intrinsic.MAX((*Y), (*YUR)) + 7
	// Unused by f4go :  write ( COORD , "(i4,1x,i4,1x)" ) X , Y
	fmt.Println("WRITE SOMETHING")
	FPPSIN(append(append([]byte{}, (*COORD)), *func()*[]byte{y:=[]byte("m ");return &y}()))
	FPPSDU()
	if (*(NCTR)) == 1 {
		FPPSIN(func()*byte{y:=byte('(');return &y}())
		for (*J) = 1; (*J) <= (*(NN)); (*J)++ {
			FPPSIN(TX(J))
		}
		FPPSIN(func()*[]byte{y:=[]byte(") w");return &y}())
		FPPSDU()
	}
	FPPSIN(func()*byte{y:=byte('(');return &y}())
	for (*J) = 1; (*J) <= (*(NN)); (*J)++ {
		FPPSIN(TX(J))
	}
	FPPSIN ( ") " append ( append ( [ ] byte { } , append ( append ( [ ] byte { } , ) , ( * CLIN ) ) ) , "show" ) )
	FPPSDU()
	(*OCLIN) = ' '
	(*LSTRK) = false
	(*LFILL) = false
	(*XOLD) = -9980
	(*YOLD) = -9980
}
