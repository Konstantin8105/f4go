package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
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
	PRMPTD struct {
		PROMPT *bool
		DEFALT *bool
		PSCOLR *bool
		PSREVS *bool
	}
	PSDAT2 struct {
		CLIN  *[ ]byte
		CVAR  *[ ]byte
		COLV  *[ ]byte
		OCLIN *[ ]byte
		OCVAR *[ ]byte
		OCOLV *[ ]byte
	}
	PSDAT5 struct {
		XOLD  *int
		YOLD  *int
		DOLD  *int
		LWOLD *int
	}
}

var COMMON MEMORY
//!$Id:$
func FPPSPL(NUM *int, XP *int, YP *int) {
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
	PROMPT := new(bool)
	DEFALT := new(bool)
	PSCOLR := new(bool)
	PSREVS := new(bool)
	COMMON.PRMPTD.PSREVS = new(float64)
	COMMON.PRMPTD.PSCOLR = new(float64)
	COMMON.PRMPTD.DEFALT = new(float64)
	COMMON.PRMPTD.PROMPT = new(int)
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
	I := new(int)
	NUMC := new(int)
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

	//!      Purpose: Save a set of plot points for PostScript panel fills

	//!      Inputs:

	//!         num       - Number of points

	//!         xp(*)     - X-coordinates for plots

	//!         yp(*)     - Y-coordinates for plots

	//!      Outputs:

	//!         none      - Outputs are written to PostScript file

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
	PROMPT = COMMON.PRMPTD.PROMPT
	DEFALT = COMMON.PRMPTD.DEFALT
	PSCOLR = COMMON.PRMPTD.PSCOLR
	PSREVS = COMMON.PRMPTD.PSREVS
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
	//!     Close out a line

	if *LSTRK {
		FPPSIN(func()*byte{y:=byte('s');return &y}())
		FPPSDU()
	}
	if XP(func()*int{y:=1;return &y}()) == XP((NUM)) && YP(func()*int{y:=1;return &y}()) == YP((NUM)) {
		(*NUMC) = (*(NUM)) - 1
	} else {
		(*NUMC) = (*(NUM))
	}
	//!     Draw a fill area

	(*X) = (*NINT(5400.0*XP(func()*int{y:=1;return &y}()) + 360.0))
	(*Y) = (*NINT(5400.0*YP(func()*int{y:=1;return &y}()) + 360.0))
	(*XLL) = (intrinsic.MIN((*X), (*XLL)))
	(*YLL) = (intrinsic.MIN((*Y), (*YLL)))
	(*XUR) = (intrinsic.MAX((*X), (*XUR)))
	(*YUR) = (intrinsic.MAX((*Y), (*YUR)))
	// Unused by f4go :  write ( COORD , "(i4,1x,i4,1x)" ) X , Y
	fmt.Println("WRITE SOMETHING")
	FPPSIN(append(append([]byte{}, (*COORD)), *func()*[]byte{y:=[]byte("m ");return &y}()))
	for (*I) = 2; (*I) <= (*NUMC); (*I)++ {
		(*X) = (*NINT(5400.0*XP(I) + 360.0))
		(*Y) = (*NINT(5400.0*YP(I) + 360.0))
		(*XLL) = (intrinsic.MIN((*X), (*XLL)))
		(*YLL) = (intrinsic.MIN((*Y), (*YLL)))
		(*XUR) = (intrinsic.MAX((*X), (*XUR)))
		(*YUR) = (intrinsic.MAX((*Y), (*YUR)))
		// Unused by f4go :  write ( COORD , "(i4,1x,i4,1x)" ) X , Y
		fmt.Println("WRITE SOMETHING")
		FPPSIN(append(append([]byte{}, (*COORD)), *func()*[]byte{y:=[]byte("l ");return &y}()))
	}
	if *PSCOLR {
		FPPSIN ( "c h" append ( append ( [ ] byte { } , append ( append ( [ ] byte { } , ) , ( * COLV ) ) ) , " f " ) )
		(*OCOLV) = (*COLV)
	} else {
		FPPSIN ( "c" append ( append ( [ ] byte { } , append ( append ( [ ] byte { } , ) , ( * CVAR ) ) ) , "f " ) )
		(*OCVAR) = (*CVAR)
	}
	(*LSTRK) = false
	(*LFILL) = true
	FPPSDU()
	(*XOLD) = (*X)
	(*YOLD) = (*Y)
	(*OCLIN) = ' '
}
