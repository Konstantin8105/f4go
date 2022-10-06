package main

type MEMORY struct {
	PLPOST struct {
		NXTCHR *int
		BUFFER *byte
	}
	IODATA struct {
		IOP *int
		IOS *int
		IRD *int
		IWD *int
		ICL *int
		LUN *int
	}
}

var COMMON MEMORY

// !$Id:$
func FPPSDU() {
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
	IBUFSZ := new(int)
	NXTCHR := new(int)
	BUFFER := new(byte)
	COMMON.PLPOST.BUFFER = func() *[]float64 {
		arr := make([]float64, 80)
		return &arr
	}()
	COMMON.PLPOST.NXTCHR = new(int)
	I := new(int)
	FIRST := new(int)
	LAST := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose:  Output string of characters to PostScript file

	//!      Inputs:

	//!        none       - Input through common /plpost/

	//!      Outputs:

	//!        none       - Outputs are written to PostScript file

	//!-----[--.----+----.----+----.-----------------------------------------]

	IOP = COMMON.IODATA.IOP
	IOS = COMMON.IODATA.IOS
	IRD = COMMON.IODATA.IRD
	IWD = COMMON.IODATA.IWD
	ICL = COMMON.IODATA.ICL
	LUN = COMMON.IODATA.LUN
	(*IBUFSZ) = 80
	NXTCHR = COMMON.PLPOST.NXTCHR
	BUFFER = COMMON.PLPOST.BUFFER
	if (*NXTCHR) > 0 {
		//!       Write to lun

		for (*FIRST) = 1; (*FIRST) <= (*NXTCHR); (*FIRST)++ {
			if (*BUFFER(FIRST)) != ' ' {
				goto Label100
				//  end
			}
		}
		return
	Label100:
		;
		for (*LAST) = (*NXTCHR); (*LAST) <= (*FIRST); (*LAST) += -1 {
			if (*BUFFER(LAST)) != ' ' {
				goto Label200
				//  end
			}
		}
	Label200:
		;
		// Unused by f4go :  200 write ( LUN , "(80a1)" ) ( BUFFER ( I ) , I = FIRST , LAST )
		fmt.Println("WRITE SOMETHING")
		(*NXTCHR) = 0
		//!       Clear buffer

		for (*I) = 1; (*I) <= 80; (*I)++ {
			BUFFER(I) = ' '
		}
	}
}
