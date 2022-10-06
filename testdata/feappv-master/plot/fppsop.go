package main

type MEMORY struct {
	BDATA struct {
		O    *[ ]byte
		HEAD *[ ]byte
	}
	IODATA struct {
		IOP *int
		IOS *int
		IRD *int
		IWD *int
		ICL *int
		LUN *int
	}
	IOFILE struct {
		KEEPFL *bool
		IOR    *int
		IOW    *int
	}
	PDATPS struct {
		HDCPY  *bool
		HDLOGO *bool
		PSFRAM *bool
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
	PSDAT5 struct {
		XOLD  *int
		YOLD  *int
		DOLD  *int
		LWOLD *int
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
	PSDAT4 struct {
		FNAME *[  ]byte
	}
	PSDAT6 struct {
		XLL   *int
		YLL   *int
		XUR   *int
		YUR   *int
		PSCAL *float64
	}
}

var COMMON MEMORY
//!$Id:$
func FPPSOP(SCAL *int) {
	O := func() *[]byte {
		arr := make([]byte, 4)
		return &arr
	}()
	HEAD := func() *[]byte {
		arr := make([]byte, 4)
		return &arr
	}()
	COMMON.BDATA.HEAD = func() *[]float64 {
		arr := make([]float64, 20)
		return &arr
	}()
	COMMON.BDATA.O = new(int)
	IOP := new(int)
	IOS := new(int)
	IRD := new(int)
	IWD := new(int)
	ICL := new(int)
	LUN := new(int)
	COMMON.IODATA.LUN = new(float64)
	COMMON.IODATA.ICL = new(float64)
	COMMON.IODATA.IWD = new(float64)
	COMMON.IODATA.IRD = new(float64)
	COMMON.IODATA.IOS = new(float64)
	COMMON.IODATA.IOP = new(int)
	IOR := new(int)
	IOW := new(int)
	COMMON.IOFILE.IOW = new(float64)
	COMMON.IOFILE.IOR = new(int)
	KEEPFL := new(bool)
	COMMON.IOFILE.KEEPFL = new(int)
	HDCPY := new(bool)
	HDLOGO := new(bool)
	PSFRAM := new(bool)
	COMMON.PDATPS.PSFRAM = new(float64)
	COMMON.PDATPS.HDLOGO = new(float64)
	COMMON.PDATPS.HDCPY = new(int)
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
	FNAME := func() *[]byte {
		arr := make([]byte, 12)
		return &arr
	}()
	COMMON.PSDAT4.FNAME = new(int)
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
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	LEN := func() *[]byte {
		arr := make([]byte, -1)
		return &arr
	}()
	FEXIST := new(bool)
	I := new(int)
	II := new(int)
	ILN := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Open a new PostScript file to receive plot data

	//!               Maximum files: 676 (FeapAA.eps to FeapZZ.eps)

	//!

	//!      Inputs:

	//!         scal      - Scale factor for plot data to be written

	//!      Outputs:

	//!         none      - Outputs are written to PostScript file

	//!-----[--.----+----.----+----.-----------------------------------------]

	O = COMMON.BDATA.O
	HEAD = COMMON.BDATA.HEAD
	IOP = COMMON.IODATA.IOP
	IOS = COMMON.IODATA.IOS
	IRD = COMMON.IODATA.IRD
	IWD = COMMON.IODATA.IWD
	ICL = COMMON.IODATA.ICL
	LUN = COMMON.IODATA.LUN
	IOR = COMMON.IOFILE.IOR
	IOW = COMMON.IOFILE.IOW
	KEEPFL = COMMON.IOFILE.KEEPFL
	HDCPY = COMMON.PDATPS.HDCPY
	HDLOGO = COMMON.PDATPS.HDLOGO
	PSFRAM = COMMON.PDATPS.PSFRAM
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
	FNAME = COMMON.PSDAT4.FNAME
	XOLD = COMMON.PSDAT5.XOLD
	YOLD = COMMON.PSDAT5.YOLD
	DOLD = COMMON.PSDAT5.DOLD
	LWOLD = COMMON.PSDAT5.LWOLD
	XLL = COMMON.PSDAT6.XLL
	YLL = COMMON.PSDAT6.YLL
	XUR = COMMON.PSDAT6.XUR
	YUR = COMMON.PSDAT6.YUR
	PSCAL = COMMON.PSDAT6.PSCAL
	(*NXTCHR) = 0
	//!     Get current date and user"s name for banner page

	FDATE(CDATE)
	GETLOG(UNAME)
	//!     Find a file name that does not already exist

	(*FNAME) = *func()*[]byte{y:=[]byte("FeapAA.eps");return &y}()
	INQUIRE ( ( * FILE ) = ( * FNAME ) , ( * EXIST ) = ( * FEXIST ) )
	for *FEXIST {
		POSTNAME(FNAME)
		INQUIRE ( ( * FILE ) = ( * FNAME ) , ( * EXIST ) = ( * FEXIST ) )
	}
	//! while

	//!     Set initial BoundingBox coordinates

	(*XLL) = 5800
	(*YLL) = 4500
	(*XUR) = 0
	(*YUR) = 0
	//!     Add problem title to file

	(*II) = 0
	for (*I) = 1; (*I) <= 67; (*I) += 4 {
		(*II) = (*II) + 1
		TITLE ( ( * I ) : ( * I ) + 3 ) = (*HEAD)[(*II)-(1)]
	}
	//! i

	//!     Show initialization worked, i.e. we opened file.

	if (*IOR) < 0 {
		// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2000 ) FNAME ( 1 : 10 )
		fmt.Println("WRITE SOMETHING")
	}
	//  open ( UNIT = LUN , FILE = "temp.eps" , STATUS = "unknown" )
	//!     Write header information to file

	FPPSIN(func()*[]byte{y:=[]byte("%!PS-Adobe-3.0 EPSF-3.0");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("%%BoundingBox: (atend) ");return &y}())
	FPPSDU()
	FPPSIN(append(append([]byte{}, *func()*[]byte{y:=[]byte("%%Title: ");return &y}()), (*TITLE)))
	FPPSDU()
	FPPSIN(append(append([]byte{}, *func()*[]byte{y:=[]byte("%%Creator: ");return &y}()), (*UNAME)))
	FPPSDU()
	FPPSIN ( "%%Creation Date: " append ( append ( [ ] byte { } , append ( append ( [ ] byte { } , ) , ( * CDATE ) ) ) , " " ) )
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("%%EndComments ");return &y}())
	FPPSDU()
	//!     Set procedure definitions

	FPPSIN(func()*[]byte{y:=[]byte("/m {moveto} bind def /l {lineto} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/s {stroke} bind def /f {fill} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/n {newpath} bind def /c {closepath} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/g {setgray} bind def /h {setrgbcolor} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/d {setdash} bind def /lw {setlinewidth} bind def ");return &y}())
	FPPSDU()
	//!     Set gray scale shades

	FPPSIN(func()*[]byte{y:=[]byte("/g0 { 0.0 g} bind def /g1 { 1.0 g} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/g2 {0.95 g} bind def /g3 {0.81 g} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/g4 {0.67 g} bind def /g5 {0.53 g} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/g6 {0.39 g} bind def /g7 {0.25 g} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/g8 {0.11 g} bind def ");return &y}())
	FPPSDU()
	//!     Set color scale hues

	FPPSIN(func()*[]byte{y:=[]byte("/h0 { 0.0 0.0 0.0 h} bind def");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/h1 { 0.0 0.0 1.0 h} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/h2 { 0.4 0.6 0.9 h} bind def");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/h3 { 0.0 0.9 0.9 h} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/h4 { 0.0 0.8 0.0 h} bind def");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/h5 { 0.9 0.9 0.0 h} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/h6 { 0.9 0.6 0.4 h} bind def");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/h7 { 1.0 0.0 0.0 h} bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/h8 { 1.0 1.0 1.0 h} bind def ");return &y}())
	FPPSDU()
	//!     Set Line types

	FPPSIN(func()*[]byte{y:=[]byte("/l1 { [] 0 d } bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/l2 { [5 30] 0 d } bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/l3 { [40 20 5 20] 0 d } bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/l4 { [40] 0 d } bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/l5 { [60] 0 d } bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/l6 { [5 20 5 40 40 40] 0 d } bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/l7 { [40 60 80 60] 0 d } bind def ");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/l8 { [80] 0 d } bind def ");return &y}())
	FPPSDU()
	//!     Set for 12 point type in landscape mode (10 in portrait)

	if *PSFRAM {
		FPPSIN(func()*[]byte{y:=[]byte("/H {/Helvetica findfont 91 scalefont setfont} bind def");return &y}())
	} else {
		FPPSIN(func()*[]byte{y:=[]byte("/H {/Helvetica findfont 100 scalefont setfont} bind def");return &y}())
	}
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/w {stringwidth pop 2 div neg 0 rmoveto} bind def");return &y}())
	FPPSDU()
	//!     Set clipping definitions

	FPPSIN(func()*[]byte{y:=[]byte("/gr {grestore} bind def  /gs {gsave} bind def");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/cl {gr gs 802 802 3333 3333 rectclip} bind def");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("/fl {gr gs   0   0 5800 4800 rectclip} bind def");return &y}())
	FPPSDU()
	//!     End of prolog

	FPPSIN(func()*[]byte{y:=[]byte("%%EndProlog ");return &y}())
	FPPSDU()
	//!     Start landscape mode plot

	if *PSFRAM {
		FPPSIN(func()*[]byte{y:=[]byte("%Landscape mode ");return &y}())
		FPPSDU()
		if *BLK {
			FPPSIN(func()*[]byte{y:=[]byte(" 0 0 0 h ");return &y}())
			FPPSIN(func()*[]byte{y:=[]byte("n 0 0 m 612 0 l 612 792 l 0 792 l c f");return &y}())
			FPPSDU()
		}
		FPPSIN(func()*[]byte{y:=[]byte("90 rotate -10 -625 translate ");return &y}())
		// Unused by f4go :  write ( STRING , "(f7.4,f7.4,a7)" ) SCAL * 0.1333 , SCAL * 0.1333 , " scale "
		fmt.Println("WRITE SOMETHING")
		FPPSIN(STRING)
		(*PSCAL) = (*SCAL) * 0.1333
		//!     Start portrait mode plot

	} else {
		FPPSIN(func()*[]byte{y:=[]byte("%Portrait mode ");return &y}())
		FPPSDU()
		// Unused by f4go :  write ( STRING , "(f7.4,f7.4,a7)" ) SCAL * 0.1 , SCAL * 0.1 , " scale "
		fmt.Println("WRITE SOMETHING")
		(*PSCAL) = (*SCAL) * 0.1
		FPPSIN(STRING)
		if *BLK {
			FPPSDU()
			FPPSIN(func()*[]byte{y:=[]byte("0 0 0 h n ");return &y}())
			FPPSIN(func()*[]byte{y:=[]byte("318 318 m 5800 318 l 5800 4495 l 318 4495 l c f");return &y}())
		}
	}
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("1 setlinecap 1 setlinejoin ");return &y}())
	//!     Initialize plot state for lines, fills, and colors to false

	(*LSTRK) = false
	(*LFILL) = false
	(*XOLD) = -9980
	(*YOLD) = -9980
	(*DOLD) = -1
	(*LWOLD) = -1
	(*CLIN) = *func()*[]byte{y:=[]byte("g0");return &y}()
	(*OCLIN) = *func()*[]byte{y:=[]byte("  ");return &y}()
	(*CVAR) = *func()*[]byte{y:=[]byte(" g0");return &y}()
	(*OCVAR) = ' '
	(*COLV) = *func()*[]byte{y:=[]byte("z ");return &y}()
	(*OCOLV) = *func()*[]byte{y:=[]byte("  ");return &y}()
	ILN(func()*int{y:=1;return &y}()) = 0
	ILN(func()*int{y:=2;return &y}()) = 1
	PLLINE(ILN)
	FPPSIN(func()*[]byte{y:=[]byte(" gs n");return &y}())
	FPPSDU()
	//!     Format

//Label2000:
	;
	// Unused by f4go :  2000 format ( " --> Opening FEAP PostScript file: " , A )
}

func POSTNAME(NAME *[]byte) {
	N := new(int)
	NC := new(int)
	ADD := new(bool)
	//!     Initialize

	(*N) = 6
	(*ADD) = true
	//!     Check names

	for (*ADD) && (*N) > 1 {
		//!       Get a character from "name"

		(*NC) = (*ICHAR(&((*(NAME))[(*N)-(1)])))
		//!       Check that it is less than a "Z"

		if (*NC) < 90 {
			(*(NAME))[(*N)-(1)] = (*CHAR((*NC) + 1))
			(*ADD) = false
			//!       It is a "Z" (or something erroneous!).  Do next column

		} else {
			(*(NAME))[(*N)-(1)] = 'A'
		}
		(*N) = (*N) - 1
	}
	//! while

	//!     Too many files exist!

	if (*N) <= 1 {
		// Unused by f4go :  write ( * , * ) " *ERROR* - Too many file names"
		fmt.Println("WRITE SOMETHING")
	}
}
