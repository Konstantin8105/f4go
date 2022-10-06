package main

import "github.com/Konstantin8105/f4go/intrinsic"

// !$Id:$
func PFACEX(IL *int, IX *int, IXF *int, NEN *int, NEN1 *int, NF *int, N *int) {
	J := new(int)
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Set up nodes for faces

	//!      Inputs:

	//!         il(*)   - Location of face nodes on element

	//!         ix(*)   - Node numbers on elements

	//!         nen     - Number of nodes on element

	//!         nen1    - Location of material set number on element

	//!         n       - Element number

	//!      Outputs:

	//!         ixf(*)  - Face nodes

	//!         nf      - Face number

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!     Set face nodes

	for (*J) = 1; (*J) <= (intrinsic.MIN(int(4), (*(NEN)))); (*J)++ {
		(IXF)(J) = (*(IX)((IL)(J)))
	}
	//!     Set region and material number

	(IXF)(func() *int { y := 5; return &y }()) = (*(N))
	(IXF)(func() *int { y := 6; return &y }()) = (*(IX)((*(NEN1)) - 1))
	(IXF)(func() *int { y := 7; return &y }()) = (*(IX)((NEN1)))
	(*(NF)) = (*(NF)) + 1
}
