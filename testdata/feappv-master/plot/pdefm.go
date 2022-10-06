package main

type MEMORY struct {
	POINTER struct {
		NP *int64
		UP *int64
	}
	NPOINTS struct {
		NPUD *int64
		NPID *int64
		NPIX *int64
		NPUU *int64
		NPXX *int64
		NPER *int64
		NPNP *int64
		NPEV *int64
		NPRN *int64
		NPTY *int64
	}
	PPOINTS struct {
		PLIX *int64
	}
	COMBLK struct {
		HR *float64
		MR *int
	}
	PDATA4 struct {
		XMIN *float64
		XMAX *float64
		NZM1 *int
		NZM2 *int
		NFAC *int
		NFMX *int
		PDF  *int
	}
}

var COMMON MEMORY

// !$Id:$
func PDEFM(X *int, B *int, C *int, ANGL *int, NDM *int, NDF *int, NUMNP *int, DR *int, FLAG *bool) {
	NUM_NPS := new(int)
	NUM_UPS := new(int)
	NP := new(int64)
	UP := new(int64)
	COMMON.POINTER.UP = func() *[]float64 {
		arr := make([]float64, 200)
		return &arr
	}()
	COMMON.POINTER.NP = func() *[]int {
		arr := make([]int, 400)
		return &arr
	}()
	NPID := new(int64)
	NPIX := new(int64)
	NPUU := new(int64)
	NPXX := new(int64)
	NPER := new(int64)
	NPNP := new(int64)
	NPEV := new(int64)
	NPRN := new(int64)
	NPTY := new(int64)
	COMMON.NPOINTS.NPTY = new(float64)
	COMMON.NPOINTS.NPRN = new(float64)
	COMMON.NPOINTS.NPEV = new(float64)
	COMMON.NPOINTS.NPNP = new(float64)
	COMMON.NPOINTS.NPER = new(float64)
	COMMON.NPOINTS.NPXX = new(float64)
	COMMON.NPOINTS.NPUU = new(float64)
	COMMON.NPOINTS.NPIX = new(float64)
	COMMON.NPOINTS.NPID = new(int)
	NPUD := new(int64)
	COMMON.NPOINTS.NPUD = new(int)
	PLIX := new(int64)
	COMMON.PPOINTS.PLIX = new(int)
	HR := new(float64)
	MR := new(int)
	COMMON.COMBLK.MR = func() *[]float64 {
		arr := make([]float64, 1024)
		return &arr
	}()
	COMMON.COMBLK.HR = func() *[]int {
		arr := make([]int, 1024)
		return &arr
	}()
	XMIN := new(float64)
	XMAX := new(float64)
	NZM1 := new(int)
	NZM2 := new(int)
	NFAC := new(int)
	NFMX := new(int)
	PDF := new(int)
	COMMON.PDATA4.PDF = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATA4.NFMX = new(float64)
	COMMON.PDATA4.NFAC = new(float64)
	COMMON.PDATA4.NZM2 = new(float64)
	COMMON.PDATA4.NZM1 = new(float64)
	COMMON.PDATA4.XMAX = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATA4.XMIN = func() *[]int {
		arr := make([]int, 3)
		return &arr
	}()
	I := new(int)
	N := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Compute deformed position of nodes

	//!      Inputs:

	//!         x(ndm,*)  - Nodal coordinates of mesh

	//!         b(ndf,*)  - Solution vector to add to coordinates

	//!         c         - Scale factor for added solution

	//!         angl(*)   - Value of boundary angle for node

	//!         ndm       - Dimension of x array

	//!         ndf       - Number dof/node

	//!         numnp     - Number of nodes in mesh

	//!         flag      - Check angle if true

	//!      Outputs:

	//!         dr(ndf,*) - Deformed coordinates

	//!-----[--.----+----.----+----.-----------------------------------------]

	(*NUM_NPS) = 400
	(*NUM_UPS) = 200
	//! int8

	NP = COMMON.POINTER.NP
	UP = COMMON.POINTER.UP
	NPID = COMMON.NPOINTS.NPID
	NPIX = COMMON.NPOINTS.NPIX
	NPUU = COMMON.NPOINTS.NPUU
	NPXX = COMMON.NPOINTS.NPXX
	NPER = COMMON.NPOINTS.NPER
	NPNP = COMMON.NPOINTS.NPNP
	NPEV = COMMON.NPOINTS.NPEV
	NPRN = COMMON.NPOINTS.NPRN
	NPTY = COMMON.NPOINTS.NPTY
	NPUD = COMMON.NPOINTS.NPUD
	PLIX = COMMON.PPOINTS.PLIX
	HR = COMMON.COMBLK.HR
	MR = COMMON.COMBLK.MR
	XMIN = COMMON.PDATA4.XMIN
	XMAX = COMMON.PDATA4.XMAX
	NZM1 = COMMON.PDATA4.NZM1
	NZM2 = COMMON.PDATA4.NZM2
	NFAC = COMMON.PDATA4.NFAC
	NFMX = COMMON.PDATA4.NFMX
	PDF = COMMON.PDATA4.PDF
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: C , CN , SN
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X ( NDM , * ) , B ( NDF , * ) , ANGL ( * ) , UU ( 3 ) , VV ( 15 ) , DR ( 3 , * )
	for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
		if (*MR((*NPTY) - 1 + (*N))) >= 0 {
			for (*I) = 1; (*I) <= (*(NDF)); (*I)++ {
				VV(I) = (*B(I, N))
			}
			//! i

			if *(FLAG) {
				if (*(NDM)) > 1 && (*(NDF)) > 1 && ANGL(N) != 0.0e0 {
					PDEGREE(ANGL(N), SN, CN)
					VV(func() *int { y := 1; return &y }()) = B(func() *int { y := 1; return &y }(), N)*(*CN) - B(func() *int { y := 2; return &y }(), N)*(*SN)
					VV(func() *int { y := 2; return &y }()) = B(func() *int { y := 1; return &y }(), N)*(*SN) + B(func() *int { y := 2; return &y }(), N)*(*CN)
				}
			}
			for (*I) = 1; (*I) <= 3; (*I)++ {
				if PDF(I) > 0 && PDF(I) <= (*(NDF)) {
					UU(I) = (*VV(PDF(I)))
				} else {
					UU(I) = 0.0e0
				}
			}
			//! i

			for (*I) = 1; (*I) <= (*(NDM)); (*I)++ {
				DR(I, N) = X(I, N) + (*C)*UU(I)
			}
			//! i

			for (*I) = (*(NDM)) + 1; (*I) <= 3; (*I)++ {
				DR(I, N) = (*C) * UU(I)
			}
			//! i

		}
	}
	//! n

}
