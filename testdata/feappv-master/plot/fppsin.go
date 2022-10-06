package main

type MEMORY struct {
	PLPOST struct {
		NXTCHR *int
		BUFFER *byte
	}
}

var COMMON MEMORY
//!$Id:$
func FPPSIN(STRING *byte) {
	IBUFSZ := new(int)
	NXTCHR := new(int)
	BUFFER := new(byte)
	COMMON.PLPOST.BUFFER = func() *[]float64 {
		arr := make([]float64, 80)
		return &arr
	}()
	COMMON.PLPOST.NXTCHR = new(int)
	I := new(int)
	L := new(int)
	//!     * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Stores string into buffer array in common /plplst/

	//!      Inputs:

	//!         string    - String of data to store

	//!      Outputs:

	//!         none

	//!-----[--.----+----.----+----.-----------------------------------------]

	(*IBUFSZ) = 80
	NXTCHR = COMMON.PLPOST.NXTCHR
	BUFFER = COMMON.PLPOST.BUFFER
	//!     Get length of string

	(*L) = (*len((*(STRING))))
	//!     Move string into buffer array

	if ((*NXTCHR) + (*L)) >= (*IBUFSZ) {
		FPPSDU()
	}
	for (*I) = 1; (*I) <= (*L); (*I)++ {
		(*NXTCHR) = (*NXTCHR) + 1
		BUFFER(NXTCHR) = STRING ( ( * I ) : ( * I ) )
	}
}
