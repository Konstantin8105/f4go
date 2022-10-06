package main

import "math"
import "github.com/Konstantin8105/f4go/intrinsic"

type MEMORY struct {
	PDATXT struct {
		DTEXT *float64
		XSYC  *float64
		JX1   *int
		JY1   *int
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
	IOFILE struct {
		KEEPFL *bool
		IOR    *int
		IOW    *int
	}
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
}

var COMMON MEMORY

// !$Id:$
func PLTFOR(X *int, F *int, ANGL *int, ID *int, IP *int, NDM *int, NDF *int, NUMNP *int, N1 *int, ISGN *int) {
	IOR := new(int)
	IOW := new(int)
	COMMON.IOFILE.IOW = new(float64)
	COMMON.IOFILE.IOR = new(int)
	KEEPFL := new(bool)
	COMMON.IOFILE.KEEPFL = new(int)
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
	DTEXT := new(float64)
	XSYC := new(float64)
	JX1 := new(int)
	JY1 := new(int)
	COMMON.PDATXT.JY1 = new(float64)
	COMMON.PDATXT.JX1 = new(float64)
	COMMON.PDATXT.XSYC = func() *[]float64 {
		arr := make([]float64, 3)
		return &arr
	}()
	COMMON.PDATXT.DTEXT = new(int)
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
	VFL := new(bool)
	ZOOM := new(bool)
	FDIS := new(bool)
	I := new(int)
	J := new(int)
	K := new(int)
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

	//!      Purpose: Draw vectors for forces on mesh

	//!      Inputs:

	//!         x(ndm,*)  - Nodal coordinates for mesh

	//!         f(ndf,*)  - Nodal forces

	//!         angl(*)   - Value of angle for sloping boundary

	//!         id(ndf,*) - Boundary condition indicator array

	//!         ip(*)     - Active node indicator

	//!         ndm       - Dimension of x array

	//!         ndf       - Dimension of f and id arrays

	//!         numnp     - Number of nodes in mesh

	//!         n1        - Flag, place tip at node if > 0

	//!         isgn      - Flag, plot displacements if > 1

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	IOR = COMMON.IOFILE.IOR
	IOW = COMMON.IOFILE.IOW
	KEEPFL = COMMON.IOFILE.KEEPFL
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
	DTEXT = COMMON.PDATXT.DTEXT
	XSYC = COMMON.PDATXT.XSYC
	JX1 = COMMON.PDATXT.JX1
	JY1 = COMMON.PDATXT.JY1
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
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: FM , X1 , X2 , X3 , DX1 , DX2 , DX3 , D , TM , CS , SN , ANG
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: DD ( 3 ) , XX ( 3 , 4 ) , X ( NDM , * ) , F ( NDF , * ) , ANGL ( * )
	//!     Compute longest vector

	PZERO(DD, func() *int { y := 3; return &y }())
	PZERO(XX, func() *int { y := 12; return &y }())
	(*FDIS) = (*(ISGN)) > 1
	(*FM) = 0.e0
	for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
		if (IP)(N) > 0 && MR(NP(func() *int { y := 190; return &y }())+(*N)-1) >= 0 && (*ZOOM(X(func() *int { y := 1; return &y }(), N), (NDM))) {
			(*D) = 0.e0
			for (*I) = 1; (*I) <= (intrinsic.MIN((*(NDM)), (*(NDF)))); (*I)++ {
				(*J) = (*PDF(I))
				if (*J) > 0 && (*J) <= (*(NDF)) {
					if (ID)(J, N) > 0 || (*(ISGN)) < 0 {
						(*D) = (*D) + math.Pow(F(J, N), 2)
					}
				}
			}
			(*FM) = (intrinsic.MAX((*FM), (*D)))
		}
	}
	//!     Zero length vectors

	if (*FM) <= 0.0e0 {
		if (*IOW) < 0 {
			// Unused by f4go :  if ( IOW .lt. 0 ) write ( * , 2000 )
			fmt.Println("WRITE SOMETHING")
		}
		//!     Compute vector at each node

	} else {
		(*FM) = (*(ISGN)) * intrinsic.SQRT((*FM)) * (*SCALEF) * 40.e0
		(*X3) = 0.0e0
		for (*N) = 1; (*N) <= (*(NUMNP)); (*N)++ {
			if (IP)(N) > 0 && (*ZOOM(X(func() *int { y := 1; return &y }(), N), (NDM))) {
				(*X1) = (*X(func() *int { y := 1; return &y }(), N))
				(*X2) = (*X(func() *int { y := 2; return &y }(), N))
				(*VFL) = false
				for (*I) = 1; (*I) <= 3; (*I)++ {
					DD(I) = 0.0e0
				}
				for (*I) = 1; (*I) <= (intrinsic.MIN((*(NDM)), (*(NDF)))); (*I)++ {
					(*J) = (*PDF(I))
					if (*J) > 0 && (*J) <= (*(NDF)) {
						if (((ID)(J, N) > 0) || (*(ISGN)) < 0) && (F(J, N) != 0.0e0) || (*FDIS) {
							DD(I) = (*F(J, N))
							(*VFL) = true
						}
					}
				}
				if *VFL {
					DD(func() *int { y := 1; return &y }()) = DD(func() *int { y := 1; return &y }()) / (*FM)
					DD(func() *int { y := 2; return &y }()) = DD(func() *int { y := 2; return &y }()) / (*FM)
					if (*ANGL(N)) != 0.0e0 {
						(*ANG) = ANGL(N) * 0.017453292e0
						(*CS) = (*COS(ANG))
						(*SN) = (*SIN(ANG))
						(*TM) = DD(func() *int { y := 1; return &y }())*(*CS) - DD(func() *int { y := 2; return &y }())*(*SN)
						DD(func() *int { y := 2; return &y }()) = DD(func() *int { y := 1; return &y }())*(*SN) + DD(func() *int { y := 2; return &y }())*(*CS)
						DD(func() *int { y := 1; return &y }()) = (*TM)
					}
					if (*(NDM)) >= 3 {
						DD(func() *int { y := 3; return &y }()) = DD(func() *int { y := 3; return &y }()) / (*FM)
						(*X3) = (*X(func() *int { y := 3; return &y }(), N))
						XX(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }()) = (*X3)
						XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }()) = XX(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }()) + DD(func() *int { y := 3; return &y }())
						XX(func() *int { y := 3; return &y }(), func() *int { y := 3; return &y }()) = XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }()) - .6e0*DD(func() *int { y := 3; return &y }()) + .2e0*(DD(func() *int { y := 1; return &y }())+DD(func() *int { y := 2; return &y }()))
						XX(func() *int { y := 3; return &y }(), func() *int { y := 4; return &y }()) = XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }()) - .6e0*DD(func() *int { y := 3; return &y }()) - .2e0*(DD(func() *int { y := 1; return &y }())+DD(func() *int { y := 2; return &y }()))
					}
					XX(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }()) = (*X1)
					XX(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }()) = (*X2)
					XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }()) = XX(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }()) + DD(func() *int { y := 1; return &y }())
					XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }()) = XX(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }()) + DD(func() *int { y := 2; return &y }())
					XX(func() *int { y := 1; return &y }(), func() *int { y := 3; return &y }()) = XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }()) - .6e0*DD(func() *int { y := 1; return &y }()) - .2e0*(DD(func() *int { y := 2; return &y }())+DD(func() *int { y := 3; return &y }()))
					XX(func() *int { y := 2; return &y }(), func() *int { y := 3; return &y }()) = XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }()) - .6e0*DD(func() *int { y := 2; return &y }()) + .2e0*(DD(func() *int { y := 1; return &y }())+DD(func() *int { y := 3; return &y }()))
					XX(func() *int { y := 1; return &y }(), func() *int { y := 4; return &y }()) = XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }()) - .6e0*DD(func() *int { y := 1; return &y }()) + .2e0*(DD(func() *int { y := 2; return &y }())+DD(func() *int { y := 3; return &y }()))
					XX(func() *int { y := 2; return &y }(), func() *int { y := 4; return &y }()) = XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }()) - .6e0*DD(func() *int { y := 2; return &y }()) - .2e0*(DD(func() *int { y := 1; return &y }())+DD(func() *int { y := 3; return &y }()))
					for (*K) = 1; (*K) <= 4; (*K)++ {
						for (*J) = 1; (*J) <= 3; (*J)++ {
							XX(J, K) = (XX(J, K) - XSYC(J)) + XSYC(J)
						}
					}
					if (*(N1)) == 0 {
						(*DX1) = 0.e0
						(*DX2) = 0.e0
						(*DX3) = 0.e0
					} else {
						(*DX1) = XX(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }()) - XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }())
						(*DX2) = XX(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }()) - XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }())
						(*DX3) = XX(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }()) - XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }())
					}
					PLOTL(XX(func() *int { y := 1; return &y }(), func() *int { y := 1; return &y }())+(*DX1), XX(func() *int { y := 2; return &y }(), func() *int { y := 1; return &y }())+(*DX2), XX(func() *int { y := 3; return &y }(), func() *int { y := 1; return &y }())+(*DX3), func() *int { y := 3; return &y }())
					PLOTL(XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }())+(*DX1), XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }())+(*DX2), XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }())+(*DX3), func() *int { y := 2; return &y }())
					PLOTL(XX(func() *int { y := 1; return &y }(), func() *int { y := 3; return &y }())+(*DX1), XX(func() *int { y := 2; return &y }(), func() *int { y := 3; return &y }())+(*DX2), XX(func() *int { y := 3; return &y }(), func() *int { y := 3; return &y }())+(*DX3), func() *int { y := 2; return &y }())
					PLOTL(XX(func() *int { y := 1; return &y }(), func() *int { y := 4; return &y }())+(*DX1), XX(func() *int { y := 2; return &y }(), func() *int { y := 4; return &y }())+(*DX2), XX(func() *int { y := 3; return &y }(), func() *int { y := 4; return &y }())+(*DX3), func() *int { y := 2; return &y }())
					PLOTL(XX(func() *int { y := 1; return &y }(), func() *int { y := 2; return &y }())+(*DX1), XX(func() *int { y := 2; return &y }(), func() *int { y := 2; return &y }())+(*DX2), XX(func() *int { y := 3; return &y }(), func() *int { y := 2; return &y }())+(*DX3), func() *int { y := 2; return &y }())
				}
			}
		}
	}
	//Label2000:

	// Unused by f4go :  2000 format ( "  Zero values acting on mesh " )
}
