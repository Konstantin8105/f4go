package main

type MEMORY struct {
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
func PLFACE(IX *int, IP *int, X *int, NDM *int, NEN1 *int, NUMNP *int, NUMEL *int, ILN *int, CT *int) {
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
	N := new(int)
	NFACE := new(int)
	KIND := func() *[]float64 {
		arr := make([]float64, -1)
		return &arr
	}()
	//!      * * F E A P * * A Finite Element Analysis Program

	//!....  Copyright (c) 1984-2021: Regents of the University of California

	//!                               All rights reserved

	//!-----[--.----+----.----+----.-----------------------------------------]

	//!      Purpose: Driver routine to display visible faces of elements

	//!               in perspective views.

	//!      Inputs:

	//!         ix(nen1,*)- Nodal connection list

	//!         ip(*)     - Sorted order for symmetry plots

	//!         x(ndm,*)  - Nodal coordinates for plot

	//!         nen1      - Dimension of ix array

	//!         numnp     - Number of nodes

	//!         numel     - Number of elements/faces

	//!         iln(*)    - Line type data

	//!         ct        - Option to plot faces with negative normals

	//!      Outputs:

	//!         none      - Plot outputs to screen/file

	//!-----[--.----+----.----+----.-----------------------------------------]

	XMIN = COMMON.PDATA4.XMIN
	XMAX = COMMON.PDATA4.XMAX
	NZM1 = COMMON.PDATA4.NZM1
	NZM2 = COMMON.PDATA4.NZM2
	NFAC = COMMON.PDATA4.NFAC
	NFMX = COMMON.PDATA4.NFMX
	PDF = COMMON.PDATA4.PDF
	//F4GO: NOT IMPLEMENTED :real ( KIND = 8 ) :: X ( NDM , NUMNP ) , CT
	//!     Plot faces which are visible

	(*NFACE) = 0
	for (*N) = 1; (*N) <= (*(NUMEL)); (*N)++ {
		if (*(IX)((NEN1), N)) > 0 {
			(IP)(N) = (*N)
			PFACEV((IX)(func() *int { y := 1; return &y }(), N), X, (NDM), (ILN), CT, (IP)(N), NFACE)
		} else {
			(IP)(N) = 0
		}
	}
	//!     Pack ip array

	(*NFACE) = 0
	for (*N) = 1; (*N) <= (*(NUMEL)); (*N)++ {
		if (IP)(N) > 0 && (IX)((NEN1), N) > 0 {
			(*NFACE) = (*NFACE) + 1
			(IP)(NFACE) = (*(IP)(N))
			if (*N) > (*NFACE) {
				(IP)(N) = 0
			}
		}
	}
	(*NFAC) = (*NFACE)
	//!     Set line type to original

	PLLINE((ILN))
}
