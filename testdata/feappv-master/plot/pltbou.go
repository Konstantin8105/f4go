package main

type MEMORY struct {
	PDATA1 struct {
		SCALEF *float64
		SCALEG *float64
		S0     *float64
		DX     *float64
		SX     *float64
		FACT   *float64
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
}

var COMMON MEMORY

// !$Id:$
func PLTBOU(ID *int, X *int, ANGL *int, IP *int, NDM *int, NDF *int, NUMNP *int, NBOU *int) {
	SCALEF := new(float64)
	SCALEG := new(float64)
	S0 := new(float64)
	DX := new(float64)
	SX := new(float64)
	FACT := new(float64)
	COMMON.PDATA1.FACT = new(float64)
	COMMON.PDATA1.SX = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA1.DX(2) = new(float64)
	COMMON.PDATA1.S0 = func() *[]float64 {
		arr := make([]float64, 2)
		return &arr
	}()
	COMMON.PDATA1.SCALEG = new(float64)
	COMMON.PDATA1.SCALEF = new(int)
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
	ZOOM := new(bool)
	BC0 := new(bool)
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

	//!      Purpose: Display boundary conditions on screen for 1-3 dof

	//!      Inputs:

	//!         id(ndf,*) - Boundary condition indicator array

	//!         x(ndm,*)  - Nodal coordinates of mesh

	//!         angl(*)   - Angle for sloping boundaries

	//!         ip(*)     - Active node indicators

	//!         ndm       - Dimension of x array

	//!         ndf       - Number dof/node

	//!         numnp     - Number of nodes in mesh

	//!         nbou      - Component to display ( 0 = all)

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	SCALEF = COMMON.PDATA1.SCALEF
	SCALEG = COMMON.PDATA1.SCALEG
	S0 = COMMON.PDATA1.S0
	DX = COMMON.PDATA1.DX
	SX = COMMON.PDATA1.SX
	FACT = COMMON.PDATA1.FACT
	XMIN = COMMON.PDATA4.XMIN
	XMAX = COMMON.PDATA4.XMAX
	NZM1 = COMMON.PDATA4.NZM1
	NZM2 = COMMON.PDATA4.NZM2
	NFAC = COMMON.PDATA4.NFAC
	NFMX = COMMON.PDATA4.NFMX
	PDF = COMMON.PDATA4.PDF
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
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: DX1 , X1 , X2 , X3 , CS , SN , ANG
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X ( NDM , * ) , ANGL ( * )
	//!     Plot boundary restraints (lines = fixed)

	(*BC0) = (*(NBOU)) == 0
	(*DX1) = .006e0 / (*SCALEF)
	for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
		if (*(IP)(N)) > 0 {
			if (*ANGL(N)) != 0.0e0 {
				(*ANG) = ANGL(N) * 0.017453292e0
				(*CS) = COS(ANG) * (*DX1)
				(*SN) = SIN(ANG) * (*DX1)
			} else {
				(*CS) = (*DX1)
				(*SN) = 0.0e0
			}
			if (*ZOOM(X(func() *int { y := 1; return &y }(), N), (NDM))) && MR(NP(func() *int { y := 190; return &y }())+(*N)-1) >= 0 {
				(*X1) = (*X(func() *int { y := 1; return &y }(), N))
				(*X2) = (*X(func() *int { y := 2; return &y }(), N))
				(*X3) = (*X(func() *int { y := 3; return &y }(), N))
				if ((*BC0) || (*(NBOU)) == 1) && (ID)(func() *int { y := 1; return &y }(), N) <= 0 {
					PLOTL((*X1)+(*CS), (*X2)+(*SN), X3, func() *int { y := 3; return &y }())
					PLOTL((*X1)-(*CS), (*X2)-(*SN), X3, func() *int { y := 2; return &y }())
				}
				if (*(NDF)) >= 2 && (*(NDM)) >= 2 {
					if (ID)(func() *int { y := 2; return &y }(), N) <= 0 && ((*BC0) || (*(NBOU)) == 2) {
						PLOTL((*X1)-(*SN), (*X2)+(*CS), X3, func() *int { y := 3; return &y }())
						PLOTL((*X1)+(*SN), (*X2)-(*CS), X3, func() *int { y := 2; return &y }())
					}
				}
				if (*(NDF)) >= 3 && (*(NDM)) >= 2 {
					if (ID)(func() *int { y := 3; return &y }(), N) <= 0 && ((*BC0) || (*(NBOU)) == 3) {
						PLOTL(X1, X2, (*X3)+(*DX1), func() *int { y := 3; return &y }())
						PLOTL(X1, X2, (*X3)-(*DX1), func() *int { y := 2; return &y }())
					}
				}
				if (*(NDF)) >= 4 && (*(NDM)) >= 2 {
					if (ID)(func() *int { y := 4; return &y }(), N) <= 0 && (*(NBOU)) == 4 {
						PLOTL((*X1)+(*CS), (*X2)+(*SN), X3, func() *int { y := 3; return &y }())
						PLOTL((*X1)-(*CS), (*X2)-(*SN), X3, func() *int { y := 2; return &y }())
					}
				}
				if (*(NDF)) >= 5 && (*(NDM)) >= 2 {
					if (ID)(func() *int { y := 5; return &y }(), N) <= 0 && (*(NBOU)) == 5 {
						PLOTL((*X1)-(*SN), (*X2)+(*CS), X3, func() *int { y := 3; return &y }())
						PLOTL((*X1)+(*SN), (*X2)-(*CS), X3, func() *int { y := 2; return &y }())
					}
				}
				if (*(NDF)) >= 6 && (*(NDM)) >= 2 {
					if (ID)(func() *int { y := 6; return &y }(), N) <= 0 && (*(NBOU)) == 6 {
						PLOTL(X1, X2, (*X3)+(*DX1), func() *int { y := 3; return &y }())
						PLOTL(X1, X2, (*X3)-(*DX1), func() *int { y := 2; return &y }())
					}
				}
			}
		}
	}
}
