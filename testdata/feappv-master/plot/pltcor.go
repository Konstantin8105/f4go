package main

// !$Id:$
func PLTCOR(NEL *int, IC *int, V *int, VC *int, NC *int) {
	I := new(int)
	N := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Compute number of contour at element corners for

	//!               use by contour plot routines

	//!      Inputs:

	//!         nel       - Number of nodes on element

	//!         v(*)      - Contour value at node

	//!         vc(*)     - Contour values to plot

	//!         nc        - Number of contours plotted

	//!      Outputs:

	//!         ic(*)     - Contour number at node

	//!-----[--.----+----.----+----.-----------------------------------------]

	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: V ( NEL ) , VC ( NC )
	for (*I) = 1; (*I) <= (*(NEL)); (*I)++ {
		(IC)(I) = 1
	}
	//! i

	for (*N) = 1; (*N) <= (*(NC)); (*N)++ {
		for (*I) = 1; (*I) <= (*(NEL)); (*I)++ {
			if (*V(I)) >= (*VC(N)) {
				(IC)(I) = (*N) + 1
			}
		}
		//! i

	}
	//! n

}
