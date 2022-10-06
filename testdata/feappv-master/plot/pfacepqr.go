package main

type MEMORY struct {
	UBLK1 struct {
		UBLKNUM *int
		UBLKDAT *int
	}
}

var COMMON MEMORY

// !$Id:$
func PFACEPQR(NP *int, IU *int, NFAC *int) {
	UBLKDAT := new(int)
	COMMON.UBLK1.UBLKDAT = func() *[][]int {
		arr := make([][]int, 3)
		for u := 0; u < 3; u++ {
			arr[u] = make([]int, 50)
		}
		return &arr
	}()
	UBLKNUM := new(int)
	COMMON.UBLK1.UBLKNUM = new(int)
	NE := new(int)
	NS := new(int)
	I := new(int)
	J := new(int)
	II := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Set plot data for brick elements

	//!      Inputs:

	//!         np      - Order of element (2 for 27 node; 3 for 64 node; etc)

	//!      Output:

	//!         iu(4,*) - 4-node quadrilateral face

	//!         nfac    - Number of faces

	//!-----[--.----+----.----+----.-----------------------------------------]

	UBLKDAT = COMMON.UBLK1.UBLKDAT
	UBLKNUM = COMMON.UBLK1.UBLKNUM
	for (*I) = 1; (*I) <= 3; (*I)++ {
		NE(I) = (*(NP))
		NS(I) = NE(I) + 1
	}
	//! i

	//!     Negative 1-face

	(*(NFAC)) = 0
	for (*J) = 1; (*J) <= (*NE(func() *int { y := 3; return &y }())); (*J)++ {
		(*II) = NS(func() *int { y := 1; return &y }()) * NS(func() *int { y := 2; return &y }()) * ((*J) - 1)
		for (*I) = 1; (*I) <= (*NE(func() *int { y := 2; return &y }())); (*I)++ {
			(*(NFAC)) = (*(NFAC)) + 1
			(IU)(func() *int { y := 1; return &y }(), (NFAC)) = NS(func() *int { y := 1; return &y }())*((*I)-1) + (*II) + 1
			(IU)(func() *int { y := 2; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())*NS(func() *int { y := 2; return &y }())
			(IU)(func() *int { y := 3; return &y }(), (NFAC)) = (IU)(func() *int { y := 2; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())
			(IU)(func() *int { y := 4; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())
		}
		//! i

	}
	//! j

	//!     Positive 1-face

	for (*J) = 1; (*J) <= (*NE(func() *int { y := 3; return &y }())); (*J)++ {
		(*II) = NS(func() *int { y := 1; return &y }())*NS(func() *int { y := 2; return &y }())*((*J)-1) + NE(func() *int { y := 1; return &y }())
		for (*I) = 1; (*I) <= (*NE(func() *int { y := 2; return &y }())); (*I)++ {
			(*(NFAC)) = (*(NFAC)) + 1
			(IU)(func() *int { y := 1; return &y }(), (NFAC)) = NS(func() *int { y := 1; return &y }())*((*I)-1) + (*II) + 1
			(IU)(func() *int { y := 2; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())
			(IU)(func() *int { y := 4; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())*NS(func() *int { y := 2; return &y }())
			(IU)(func() *int { y := 3; return &y }(), (NFAC)) = (IU)(func() *int { y := 4; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())
		}
		//! i

	}
	//! j

	//!     Negative 2-face

	for (*J) = 1; (*J) <= (*NE(func() *int { y := 3; return &y }())); (*J)++ {
		(*II) = NS(func() *int { y := 1; return &y }()) * NS(func() *int { y := 2; return &y }()) * ((*J) - 1)
		for (*I) = 1; (*I) <= (*NE(func() *int { y := 1; return &y }())); (*I)++ {
			(*(NFAC)) = (*(NFAC)) + 1
			(IU)(func() *int { y := 1; return &y }(), (NFAC)) = (*II) + (*I)
			(IU)(func() *int { y := 2; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + 1
			(IU)(func() *int { y := 4; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())*NS(func() *int { y := 2; return &y }())
			(IU)(func() *int { y := 3; return &y }(), (NFAC)) = (IU)(func() *int { y := 4; return &y }(), (NFAC)) + 1
		}
		//! i

	}
	//! j

	//!     Positive 2-face

	for (*J) = 1; (*J) <= (*NE(func() *int { y := 3; return &y }())); (*J)++ {
		(*II) = NS(func() *int { y := 1; return &y }())*NS(func() *int { y := 2; return &y }())*(*J) - NS(func() *int { y := 1; return &y }())
		for (*I) = 1; (*I) <= (*NE(func() *int { y := 1; return &y }())); (*I)++ {
			(*(NFAC)) = (*(NFAC)) + 1
			(IU)(func() *int { y := 1; return &y }(), (NFAC)) = (*II) + (*I)
			(IU)(func() *int { y := 2; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())*NS(func() *int { y := 2; return &y }())
			(IU)(func() *int { y := 3; return &y }(), (NFAC)) = (IU)(func() *int { y := 2; return &y }(), (NFAC)) + 1
			(IU)(func() *int { y := 4; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + 1
		}
		//! i

	}
	//! j

	//!     Negative 3-face

	for (*J) = 1; (*J) <= (*NE(func() *int { y := 2; return &y }())); (*J)++ {
		(*II) = NS(func() *int { y := 1; return &y }()) * ((*J) - 1)
		for (*I) = 1; (*I) <= (*NE(func() *int { y := 1; return &y }())); (*I)++ {
			(*(NFAC)) = (*(NFAC)) + 1
			(IU)(func() *int { y := 1; return &y }(), (NFAC)) = (*II) + (*I)
			(IU)(func() *int { y := 2; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())
			(IU)(func() *int { y := 3; return &y }(), (NFAC)) = (IU)(func() *int { y := 2; return &y }(), (NFAC)) + 1
			(IU)(func() *int { y := 4; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + 1
		}
		//! i

	}
	//! j

	//!     Positive 3-face

	for (*J) = 1; (*J) <= (*NE(func() *int { y := 2; return &y }())); (*J)++ {
		(*II) = NS(func() *int { y := 1; return &y }())*((*J)-1) + NS(func() *int { y := 1; return &y }())*NS(func() *int { y := 2; return &y }())*(NS(func() *int { y := 3; return &y }())-1)
		for (*I) = 1; (*I) <= (*NE(func() *int { y := 1; return &y }())); (*I)++ {
			(*(NFAC)) = (*(NFAC)) + 1
			(IU)(func() *int { y := 1; return &y }(), (NFAC)) = (*II) + (*I)
			(IU)(func() *int { y := 2; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + 1
			(IU)(func() *int { y := 4; return &y }(), (NFAC)) = (IU)(func() *int { y := 1; return &y }(), (NFAC)) + NS(func() *int { y := 1; return &y }())
			(IU)(func() *int { y := 3; return &y }(), (NFAC)) = (IU)(func() *int { y := 4; return &y }(), (NFAC)) + 1
		}
		//! i

	}
	//! j

}
