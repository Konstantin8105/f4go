package main

type MEMORY struct {
	PDATA5 struct {
		EXORD *int
		EPORD *int
	}
	PDATA6 struct {
		INORD *int
		IPORD *int
		IPU   *int
	}
}

var COMMON MEMORY

// !$Id:$
func PLBKPQR(NP *int, IEL *int) {
	EXORD := new(int)
	EPORD := new(int)
	COMMON.PDATA5.EPORD = func() *[][]float64 {
		arr := make([][]float64, 50)
		for u := 0; u < 50; u++ {
			arr[u] = make([]float64, 50)
		}
		return &arr
	}()
	COMMON.PDATA5.EXORD = func() *[]int {
		arr := make([]int, 50)
		return &arr
	}()
	INORD := new(int)
	IPORD := new(int)
	IPU := new(int)
	COMMON.PDATA6.IPU = func() *[][]float64 {
		arr := make([][]float64, 4)
		for u := 0; u < 4; u++ {
			arr[u] = make([]float64, 200)
		}
		return &arr
	}()
	COMMON.PDATA6.IPORD = func() *[][]float64 {
		arr := make([][]float64, 200)
		for u := 0; u < 200; u++ {
			arr[u] = make([]float64, 50)
		}
		return &arr
	}()
	COMMON.PDATA6.INORD = func() *[]int {
		arr := make([]int, 50)
		return &arr
	}()
	I := new(int)
	NS := new(int)
	NT := new(int)
	NU := new(int)
	NS12 := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Set 3-D Plot Sequence for 64-node brick elements

	//!      Inputs:

	//!         np        - Order of element (3 for 64 node brick)

	//!         iel       - Element number: > 0 for user    elements

	//!                                     < 0 for program elements

	//!      Outputs:

	//!         none      - Sequesnce returned in common /pdata6/

	//!-----[--.----+----.----+----.-----------------------------------------]

	EXORD = COMMON.PDATA5.EXORD
	EPORD = COMMON.PDATA5.EPORD
	INORD = COMMON.PDATA6.INORD
	IPORD = COMMON.PDATA6.IPORD
	IPU = COMMON.PDATA6.IPU
	//!     Set control variables

	for (*I) = 1; (*I) <= 3; (*I)++ {
		NS(I) = (*(NP)) + 1
	}
	//! i

	(*NS12) = NS(func() *int { y := 1; return &y }()) * NS(func() *int { y := 2; return &y }())
	//!     Set number of points

	if (*(IEL)) > 0 {
		//!       Trace around bottom

		for (*I) = 1; (*I) <= (*NS(func() *int { y := 1; return &y }())); (*I)++ {
			IPORD(I, (IEL)) = (*I)
		}
		//! i

		(*NT) = (*NS(func() *int { y := 1; return &y }()))
		(*NU) = (*NS(func() *int { y := 1; return &y }()))
		for (*I) = 2; (*I) <= (*NS(func() *int { y := 2; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + NS(func() *int { y := 1; return &y }())
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		for (*I) = NS(func() *int { y := 1; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - 1
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		for (*I) = NS(func() *int { y := 2; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - NS(func() *int { y := 1; return &y }())
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		//!       Up first 3-edge

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 3; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + (*NS12)
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		//!       Around top first edge

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 1; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + 1
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		//!       Down-up second 3-edge

		for (*I) = NS(func() *int { y := 3; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - (*NS12)
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 3; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + (*NS12)
			IPORD(NT, (IEL)) = (*NU)
		}
		//!

		//!       Second top edge

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 2; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + NS(func() *int { y := 1; return &y }())
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		//!       Down-up third 3-edge

		for (*I) = NS(func() *int { y := 3; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - (*NS12)
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 3; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + (*NS12)
			IPORD(NT, (IEL)) = (*NU)
		}
		//!

		//!       Third top edge

		for (*I) = NS(func() *int { y := 1; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - 1
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		//!       Down-up fourth 3-edge

		for (*I) = NS(func() *int { y := 3; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - (*NS12)
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 3; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + (*NS12)
			IPORD(NT, (IEL)) = (*NU)
		}
		//!

		//!       Last top edge

		for (*I) = NS(func() *int { y := 2; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - NS(func() *int { y := 1; return &y }())
			IPORD(NT, (IEL)) = (*NU)
		}
		//! i

		INORD((IEL)) = (*NT)
	} else if (*(IEL)) < 0 {
		//!       Trace around bottom

		for (*I) = 1; (*I) <= (*NS(func() *int { y := 1; return &y }())); (*I)++ {
			EPORD(I, -(*(IEL))) = (*I)
		}
		//! i

		(*NT) = (*NS(func() *int { y := 1; return &y }()))
		(*NU) = (*NS(func() *int { y := 1; return &y }()))
		for (*I) = 2; (*I) <= (*NS(func() *int { y := 2; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + NS(func() *int { y := 1; return &y }())
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		for (*I) = NS(func() *int { y := 1; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - 1
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		for (*I) = NS(func() *int { y := 2; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - NS(func() *int { y := 1; return &y }())
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		//!       Up first 3-edge

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 3; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + (*NS12)
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		//!       Around top first edge

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 1; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + 1
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		//!       Down-up second 3-edge

		for (*I) = NS(func() *int { y := 3; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - (*NS12)
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 3; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + (*NS12)
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//!

		//!       Second top edge

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 2; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + NS(func() *int { y := 1; return &y }())
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		//!       Down-up third 3-edge

		for (*I) = NS(func() *int { y := 3; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - (*NS12)
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 3; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + (*NS12)
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//!

		//!       Third top edge

		for (*I) = NS(func() *int { y := 1; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - 1
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		//!       Down-up fourth 3-edge

		for (*I) = NS(func() *int { y := 3; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - (*NS12)
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		for (*I) = 2; (*I) <= (*NS(func() *int { y := 3; return &y }())); (*I)++ {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) + (*NS12)
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//!

		//!       Last top edge

		for (*I) = NS(func() *int { y := 2; return &y }()) - 1; (*I) <= 1; (*I) += -1 {
			(*NT) = (*NT) + 1
			(*NU) = (*NU) - NS(func() *int { y := 1; return &y }())
			EPORD(NT, -(*(IEL))) = (*NU)
		}
		//! i

		EXORD(-(*(IEL))) = (*NT)
	}
}
