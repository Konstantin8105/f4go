c-----------------------------------------------------------------------
c     ftnchek test file: namelist.f, Mon Mar 13 14:01:03 1995
c-----------------------------------------------------------------------

      common b,e,f
      namelist /n1/ a,b,c
      namelist /n2/ x,y,z /n3/ l,m,
      namelist /n4/ x1,y2 z3, /n5/ l6
      read(*,n1)
      write(*,n2)
      read(1,nml=n3)
      write(1,nml=n4)
      end


      subroutine grfgg3
C$    (grfgg3)
C$    This is a substantial portion of the declaration section from
C$    the PLOT79 grfgg3.sf3 file.  The formatting was done by the
C$    Extended PFORT Verifier, and the extensive NAMELIST block serves
C$    as a test for ftnchek.
C$    (13-Mar-1995)
C$ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C$
C$    EXTERNAL FUNCTIONS AND SUBROUTINES
C$
      INTEGER             I1MACH,      NINT,        UTISL
      REAL                ALOG10,      R1MACH,      UTRE3
C$ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C$
C$    INTRINSIC FUNCTIONS
C$
      INTEGER             IABS,        MAX0,        MIN0
      REAL                ABS,         AMAX1,       AMIN1,       FLOAT
      REAL                SIGN
C$ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C$
C$    STATEMENT FUNCTIONS
C$
      LOGICAL             IN
      REAL                CMTOIN,      CMTOUN,      FRAC,        INTOCM
      REAL                INTOUN,      MMTOCM,      UNTOCM,      UNTOIN
C$ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C$
C$    HOLLERITH STRING VARIABLES
C$
      INTEGER             BLANK,       GREEK,       QUOTE,       ROMAN
C$
C$ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C$
C$    NON-COMMON VARIABLES
C$
      INTEGER             BIGINT,      CARD(500),   CURCOL,      CURROW
      INTEGER             FSTYLE,      I,           IARG,        IARG1
      INTEGER             IARG2,       IDFONT,      IERVAL(1),   II
      INTEGER             J,           JX,          JY,          JZ
      INTEGER             K,           KX,          KXYZ,        KY
      INTEGER             KZ,          LSTYLE(06),  MARK(06),    MAXCRD
      INTEGER             MAXCRV,      MAXVAL,      MODEU,       MODEV
      INTEGER             MODEX,       MODEY,       MODEZ,       N(06)
      INTEGER             NC,          NCELLX,      NCELLY,      NCINIT
      INTEGER             NCISAV,      NCSTEP,      NCTERM,      NCU
      INTEGER             NCV,         NCX,         NCY,         NCZ
      INTEGER             NIN,         NJ,          NOUT,        NRINIT
      INTEGER             NRISAV,      NRSTEP,      NRTERM,      NSUM
      INTEGER             NU(06),      NUJ,         NUMINT(06),  NUSUB
      INTEGER             NVSUB,       NXSUB,       NYSUB,       NZSUB
      INTEGER             PEN(06),     TITLE(500),  UTITLE(500)
      INTEGER             VTITLE(500), XTITLE(500), YTITLE(500)
      INTEGER             ZTITLE(500)
      LOGICAL             AUTOX,       AUTOY,       AUTOZ
      LOGICAL             AVRAGE(06),  AXES3D,      BCLIP,       BOX
      LOGICAL             BYROWS,      CHKDUP,      CVTX(06)
      LOGICAL             CVTY(06),    CVTZ(06),    DERIV(06),   EOFILE
      LOGICAL             FCLIP,       INTGRT(06),  LOGX,        LOGY
      LOGICAL             LOGZ,        LSTXYZ,      PARLEL,      PLOPEN
      LOGICAL             PLOTID,      RESET,       RIGHT,       UAXIS
      LOGICAL             UVAXES,      VARPEN,      VAXIS,       WCLIP
      LOGICAL             XAXIS,       XFALLS,      YAXIS,       YFALLS
      LOGICAL             ZAXIS,       ZFALLS
      REAL                ARCLEN,      ARG,         ARG1,        ARG2
      REAL                ARX,         ARY,         CMFACT,      CPX
      REAL                CPY,         DEFHT,       DEFMAR,      DT
      REAL                DUSUB,       DVSUB,       DXSUB,       DYSUB
      REAL                DZSUB,       FSCALE,      GSCALE,      HALF
      REAL                HH,          HSCALE,      HT,          HTDEF
      REAL                HTFACT,      HX,          HY,          INFITY
      REAL                LWIDTH(06),  MARGIN(4),   MSCALE,      NINETY
      REAL                ONE,         PAGESQ,      PAGEX,       PAGEY
      REAL                PENDIA,      PERDST,      PICTHX,      PICTHY
      REAL                PICTX,       PICTY,       PPXYZ(3)
      REAL                RPXYZ(3),    SIGMA(06),   SWAP,        T(4,4)
      REAL                TICDEF,      TICKU,       TICKV,       TICKX
      REAL                TICKY,       TICKZ,       TIMAGE(4,4)
      REAL                TMODEL(4,4), TOPMAR,      TT,          TTLHEI
      REAL                TTLLEN,      TTOTAL(4,4), TWO,         U1
      REAL                U2,          UDIST,       UMAX,        UMAXP
      REAL                UMAXRC,      UMIN,        UMINP,       UMINRC
      REAL                UNITS,       UNSPEC,      UPXYZ(3),    V1
      REAL                V2,          VDIST,       VIEWD,       VMAX
      REAL                VMAXP,       VMAXRC,      VMIN,        VMINP
      REAL                VMINRC,      VNXYZ(3),    VPUMAX,      VPUMIN
      REAL                VPVMAX,      VPVMIN,      VSCALE,      WMAX
      REAL                WMAXP,       WMIN,        WMINP
      REAL                WORK(1600,5),X(1600,06),  X0,          XDIST
      REAL                XFACT(06),   XINT,        XMAX,        XMAXJ
      REAL                XMAXP,       XMIN,        XMINJ,       XMINP
      REAL                XORG,        XSHIFT(06),  XX
      REAL                Y(1600,06),  Y0,          YDIST
      REAL                YFACT(06),   YINT,        YMAX,        YMAXJ
      REAL                YMAXP,       YMIN,        YMINJ,       YMINP
      REAL                YORG,        YSHIFT(06),  YY
      REAL                Z(1600,06),  Z0,          ZDIST,       ZERO
      REAL                ZFACT(06),   ZINT,        ZMAX,        ZMAXJ
      REAL                ZMAXP,       ZMIN,        ZMINJ,       ZMINP
      REAL                ZORG,        ZSHIFT(06),  ZZ
C$ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C$
C$    NAMELIST BLOCKS
C$
      NAMELIST / DATA /
     X                  AVRAGE, AXES3D, BCLIP,  BOX,    BYROWS, CHKDUP,
     X                  DERIV,  DUSUB,  DVSUB,  DXSUB,  DYSUB,  DZSUB,
     X                  FCLIP,  FSCALE, FSTYLE, GSCALE, HSCALE, IDFONT,
     X                  INTGRT, KX,     KY,     KZ,     LOGX,   LOGY,
     X                  LOGZ,   LSTXYZ, LSTYLE, LWIDTH, MARGIN, MARK,
     X                  MODEU,  MODEV,  MODEX,  MODEY,  MODEZ,  MSCALE,
     X                  N,      NCINIT, NCSTEP, NCTERM, NRINIT, NRSTEP,
     X                  NRTERM, NU,     NUMINT, NUSUB,  NVSUB,  NXSUB,
     X                  NYSUB,  NZSUB,  PARLEL, PEN,    PENDIA, PERDST,
     X                  PLOTID, PPXYZ,  RESET,  RIGHT,  RPXYZ,  SIGMA,
     X                  TICKU,  TICKV,  TICKX,  TICKY,  TICKZ,  TIMAGE,
     X                  TMODEL, UAXIS,  UMAX,   UMIN,   UPXYZ,  UVAXES,
     X                  VAXIS,  VIEWD,  VMAX,   VMIN,   VNXYZ,  VSCALE,
     X                  WCLIP,  WMAX,   WMIN,   X,      XAXIS,  XFACT,
     X                  XFALLS, XMAX,   XMIN,   XSHIFT, Y,      YAXIS,
     X                  YFACT,  YFALLS, YMAX,   YMIN,   YSHIFT, Z,
     X                  ZAXIS,  ZFACT,  ZFALLS, ZMAX,   ZMIN,   ZSHIFT
      END
