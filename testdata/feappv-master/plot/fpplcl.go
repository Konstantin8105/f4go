package main

import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
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
}

var COMMON MEMORY
//!$Id:$
func FPPLCL() {
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
	FNAME := func() *[]byte {
		arr := make([]byte, 12)
		return &arr
	}()
	COMMON.PSDAT4.FNAME = new(int)
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
	LLX := func() *[]byte {
		arr := make([]byte, 9)
		return &arr
	}()
	LLY := func() *[]byte {
		arr := make([]byte, 9)
		return &arr
	}()
	URX := func() *[]byte {
		arr := make([]byte, 9)
		return &arr
	}()
	URY := func() *[]byte {
		arr := make([]byte, 9)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Close a PostScript file.

	//!      Inputs:

	//!         none

	//!      Outputs:

	//!         none      - Outputs written to postscript file: feappost.-

	//!-----[--.----+----.----+----.-----------------------------------------]

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
	FNAME = COMMON.PSDAT4.FNAME
	XLL = COMMON.PSDAT6.XLL
	YLL = COMMON.PSDAT6.YLL
	XUR = COMMON.PSDAT6.XUR
	YUR = COMMON.PSDAT6.YUR
	PSCAL = COMMON.PSDAT6.PSCAL
	//!     Close line with stroke if necessary

	if *LSTRK {
		FPPSIN(func()*byte{y:=byte('s');return &y}())
	}
	//!     Add closing information to file

	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("gr showpage");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("%%Trailer");return &y}())
	FPPSDU()
	FPPSIN(func()*[]byte{y:=[]byte("%%EOF");return &y}())
	FPPSDU()
	//!     Convert bounding box coordinates to character array

	if *PSFRAM {
		// Unused by f4go :  write ( LLX , "(i9)" ) - NINT ( YUR * PSCAL ) - 3 + 625
		fmt.Println("WRITE SOMETHING")
		// Unused by f4go :  write ( LLY , "(i9)" ) NINT ( XLL * PSCAL ) - 3 - 10
		fmt.Println("WRITE SOMETHING")
		// Unused by f4go :  write ( URX , "(i9)" ) - NINT ( YLL * PSCAL ) + 3 + 625
		fmt.Println("WRITE SOMETHING")
		// Unused by f4go :  write ( URY , "(i9)" ) NINT ( XUR * PSCAL ) + 3 - 10
		fmt.Println("WRITE SOMETHING")
		ELS(*E)
		// Unused by f4go :  write ( LLX , "(i9)" ) NINT ( XLL * PSCAL ) - 3
		fmt.Println("WRITE SOMETHING")
		// Unused by f4go :  write ( LLY , "(i9)" ) NINT ( YLL * PSCAL ) - 3
		fmt.Println("WRITE SOMETHING")
		// Unused by f4go :  write ( URX , "(i9)" ) NINT ( XUR * PSCAL ) + 3
		fmt.Println("WRITE SOMETHING")
		// Unused by f4go :  write ( URY , "(i9)" ) NINT ( YUR * PSCAL ) + 3
		fmt.Println("WRITE SOMETHING")
	}
	//!     Create "Feap#.eps file

	FEAPBB(FNAME, LLX, LLY, URX, URY)
	if (*IOR) < 0 {
		// Unused by f4go :  if ( IOR .lt. 0 ) write ( * , 2000 ) FNAME ( 1 : 10 )
		fmt.Println("WRITE SOMETHING")
	}
//Label2000:
	;
	// Unused by f4go :  2000 format ( " --> Closing FEAP PostScript file: " , A )
}

func FEAPBB(FILER *int, LLX *int, LLY *int, URX *int, URY *int) {
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
	EOFILE := new(bool)
	II := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!-----[--+---------+---------+---------+---------+---------+---------+-]

	//!      Purpose: Puts bounding box at beginning of file

	//!      Inputs:

	//!      Outputs:

	//!-----[--+---------+---------+---------+---------+---------+---------+-]

	IOP = COMMON.IODATA.IOP
	IOS = COMMON.IODATA.IOS
	IRD = COMMON.IODATA.IRD
	IWD = COMMON.IODATA.IWD
	ICL = COMMON.IODATA.ICL
	LUN = COMMON.IODATA.LUN
	//F4GO: NOT IMPLEMENTED :character ( LEN = 9 ) :: LLX , LLY , URX , URY
	//!     Set up bounding box record

	BOUNDBOX ( 1 : 14 ) = *func()*[]byte{y:=[]byte("%%BoundingBox:");return &y}()
	BOUNDBOX ( 15 : 23 ) = (*LLX)
	BOUNDBOX ( 24 : 32 ) = (*LLY)
	BOUNDBOX ( 33 : 41 ) = (*URX)
	BOUNDBOX ( 42 : 50 ) = (*URY)
	//!     Open and rewind write file

	//  open ( UNIT = IOS , FILE = FILER , STATUS = "unknown" )
	intrinsic.REWIND((LUN))
	intrinsic.REWIND((IOS))
	//!     Read records from "temp.eps" copy to "Feap#.eps"

	(*EOFILE) = true
	for *EOFILE {
		//  read ( LUN , "(a)" , end = 200 ) LINE
		//!       Non bounding box records

		if LINE ( 3 : 7 ) != "Bound" {
			for (*II) = 80; (*II) <= 1; (*II) += -1 {
				if LINE ( ( * II ) : ( * II ) ) != " " {
					goto Label100
					//  end
				}
			}
		Label100:
			;
			// Unused by f4go :  100 write ( IOS , "(a)" ) LINE ( 1 : II )
			fmt.Println("WRITE SOMETHING")
			//!       BoundingBox record

		} else {
			// Unused by f4go :  write ( IOS , "(a50)" ) BOUNDBOX
			fmt.Println("WRITE SOMETHING")
		}
	}
//Label200:
	;
	intrinsic.CLOSE(LUN)
	intrinsic.CLOSE(IOS)
}
