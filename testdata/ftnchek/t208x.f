c-----------------------------------------------------------------------
c     ftnchek test file: t208x.f, Mon Mar 13 14:13:16 1995
c     Adapted from a benchmark program at the University of Utah, with
c     code bodies and most comments eliminated.  ftnchek's -makedcls
c     option and the dcl2inc program have been subsequently used on
c     the original copy of this program to replace all in-line COMMON
c     blocks with INCLUDE statements and separate include files.
c
c  Modified Sat Feb  3 10:07:52 EST 2001 by R. Moniot to include a
c  NAMELIST declaration to improve it as a test of ftnchek.  The
c  declarations were excerpted from the PLOT79 grfgg3.sf3 file.
c-----------------------------------------------------------------------
      program prob5_4dim
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      parameter(nr=32, nt=16, nr2=nr+2, nt1=nt+1)
      parameter(nw=2*nt+15)
      parameter(mmax=200)
c
c common block variables
c
      double precision u,f,fb,elf,elfn
      double precision phin,phinn,phia,phian
      double precision c,cn
      double precision xm
      double precision h,dlt
      double precision zag,zagn
      double precision k0
      double precision uy,vy,ux,vx
      double precision wsave,dr,r
c
c other variables
c
      character*15 rname
      character*18 ufile,ffile,pafile,zfile
      character*18 pnfile,efile
      character*18 cfile,mfile,parfile,fmfile
      character*18 mmfile,maifile,symfile
      character*18 e1file,e2file,e3file,e4file,e5file
      character*18 e6file,e7file,e8file,e9file,e10file
      character*18 e11file,e12file,e13file,e14file,e15file
      character*18 e16file,e17file,e18file,e19file,e20file
      character*18 e21file,e22file,e23file,e24file
      integer      rlen
      double precision uold(0:nbp1,0:nbp1,2)
      double precision zagi(0:nbp1,0:nbp1)

      LOGICAL             AVRAGE(06),  AXES3D,      BCLIP,       BOX
      LOGICAL             BYROWS,      CHKDUP,    DERIV(06)
      REAL                DUSUB,       DVSUB,       DXSUB,       DYSUB
c
c namelists
c
      NAMELIST / DATA /
     X                  AVRAGE, AXES3D, BCLIP,  BOX,    BYROWS, CHKDUP,
     X                  DERIV,  DUSUB,  DVSUB,  DXSUB,  DYSUB,  DZSUB
c
      common/vel   /u(0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
      common/force /f(0:nbp1,0:nbp1,2)
      common/link  /elf (0:nbp1,0:nbp1,0:nr2,nt1)
      common/link  /elfn(0:nbp1,0:nbp1,0:nr2,nt1)
      common/phi   /phin (0:nbp1,0:nbp1)
      common/phi   /phinn(0:nbp1,0:nbp1)
      common/phi   /phia (0:nbp1,0:nbp1)
      common/phi   /phian(0:nbp1,0:nbp1)
      common/adp   /c  (0:nbp1,0:nbp1)
      common/adp   /cn (0:nbp1,0:nbp1)
      common/agg   /zag   (0:nbp1,0:nbp1)
      common/agg   /zagn  (0:nbp1,0:nbp1)
      common/forceb/fb(0:nbp1,0:nbp1,2)
      common/stiff /k0
      common/steps /h,dlt
      common/rsize  /dr(nr2),r(0:nr2),nk
      common/wave   /wsave(nw)
      common/mth    /method,mthlim
      common/efnum  /nfil
c
      common/fmarkers/xm(mmax,2)
c
      double precision cmax,cmin,phiamax,phiamin
      double precision phinmax,phinmin,zagmax,zagmin
c
      double precision umax,usmax,fbmax,flmax,q
      double precision tcoef,time,tfreq,tpi,f0
c
c.... code body eliminated ....
c
      end
c
c-------------------------------------------------
c
      subroutine uinit
c
c
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      parameter(nr=32, nt=16, nr2=nr+2, nt1=nt+1)
      parameter(mmax=200)
c
c common block variables
c
      double precision u,fb
      double precision elf,elfn
      double precision phin,phinn,phia,phian
      double precision c,cn
      double precision zag,zagn,xm
      double precision h,dlt,s1,s2,s3,a
      double precision re,pen,pec,cnd1,cnd2,cnd3,cnd4,cnd5
      double precision uy,vy,ux,vx,x,y
      double precision theta,dr,r
      double precision k0,alpha0,beta0
c
c
c other variables
c
      double precision ro,mu,achem,ct,r0,a2,a0
      double precision c0,c1,c2,d,pi,tpi,u0,b0
      double precision s0,phi0,z0,elf0,adp0,xlow,ylow,wl
      double precision xchar,uchar,tchar,pchar,fchar
      double precision elfchar,phichar,zchar,cchar
      double precision ang,ax,ay,co,cx,cxh,cy,cyh
      double precision dc,dn,si,sx,sxh,sy,syh,th,xh,yh
      double precision cmax,cmin
c
      common/vel   /u (0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
      common/forceb/fb(0:nbp1,0:nbp1,2)
      common/link  /elf  (0:nbp1,0:nbp1,0:nr2,nt1)
      common/link  /elfn (0:nbp1,0:nbp1,0:nr2,nt1)
      common/phi   /phin (0:nbp1,0:nbp1)
      common/phi   /phinn(0:nbp1,0:nbp1)
      common/phi   /phia (0:nbp1,0:nbp1)
      common/phi   /phian(0:nbp1,0:nbp1)
      common/adp   /c  (0:nbp1,0:nbp1)
      common/adp   /cn (0:nbp1,0:nbp1)
      common/agg   /zag   (0:nbp1,0:nbp1)
      common/agg   /zagn  (0:nbp1,0:nbp1)
      common/coefs /s1,s2,s3,re,a
      common/cnd   /cnd1,cnd2,cnd3,cnd4,cnd5
      common/steps /h,dlt
      common/psteps /theta
      common/rsize  /dr(nr2),r(0:nr2),nk
      common/grid  /x(0:nbp1),y(0:nbp1)
      common/diffn /pen
      common/diffc /pec
c
      common/stiff /k0
      common/linkf /alpha0
      common/linkb /beta0
      common/char  /fchar,zchar,phichar
c
      common/fmarkers/xm(mmax,2)
c
c.... code body eliminated ....
c
      end
c
c--------------------------------------------------------------
c
          subroutine xinit
c
      parameter(l2ng=6,ng=2**l2ng,nb =ng+2,ngm1=ng-1)
      parameter(nbp1=nb+1)
c
      double precision sinsq,bzero,z
      double precision h,dlt
      double precision re,s1,s2,s3,a
      double precision pi,tpon,si,sj
c
      common/fft   /sinsq(0:nbp1,0:nbp1)
      common/fft   /bzero(0:nbp1,0:nbp1)
      common/ptds  /z(0:nbp1,0:nbp1)
      common/steps /h,dlt
      common/coefs /s1,s2,s3,re,a
c
c.... code body eliminated ....
c
      end
c
c-------------------------------------------------
c
      subroutine navs2d
c
      parameter(l2ng=6,ng=2**l2ng,nb =ng+2,ngm1=ng-1)
      parameter(nbp1=nb+1)
c
      double precision u,f,sinsq,bzero,z
      double precision h,dlt,re,s1,s2,s3,a
      double precision uy,vy,ux,vx
c
      double precision w1,w2,b,r,yp,c,lam,yh,p,d
      double precision pro,xl,yl
c
      common/vel   /u(0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
      common/force /f(0:nbp1,0:nbp1,2)
      common/pres  /p(0:nbp1,0:nbp1)
      common/fft   /sinsq(0:nbp1,0:nbp1)
      common/fft   /bzero(0:nbp1,0:nbp1)
      common/ptds  /z(0:nbp1,0:nbp1)
      common/steps /h,dlt
      common/coefs /s1,s2,s3,re,a
c
      dimension w1(0:nbp1,0:nbp1,1:2)
      dimension w2(0:nbp1,0:nbp1,1:2)
c
      dimension b (0:nbp1,0:nbp1)
      dimension r (0:nbp1,0:nbp1)
      dimension yp(0:nbp1,0:nbp1)
      dimension c (0:nbp1,0:nbp1)
      dimension yh(0:nbp1,0:nbp1)
      dimension d (0:nbp1,0:nbp1)
      dimension lam(ng)
c
      equivalence(f,w1,w2)
      equivalence(d,p)
c
c.... code body eliminated ....
c
      end
c
c---------------------------------------------------------------
c
      subroutine tridgx(a,b,c,w,y)
c
      parameter(l2ng=6,ng=2**l2ng,nb =ng+2,ngm1=ng-1)
      parameter(n=ng-1,np1=n+1,nm1=n-1)
      parameter(nbp1=nb+1)
c
      double precision a,b,c,w,y
c
      double precision x,l,r,d
c
      common/tdspace/x(0:nbp1,0:nbp1)
      common/tdspace/l(0:nbp1,0:nbp1)
      common/tdspace/r(0:nbp1,0:nbp1)
      common/tdspace/d(0:nbp1,0:nbp1)
c
      dimension y(0:nbp1,0:nbp1)
      dimension b(0:nbp1,0:nbp1)
      dimension c(0:nbp1,0:nbp1)
      dimension w(0:nbp1,0:nbp1)
c
c.... code body eliminated ....
c
      end
c
c---------------------------------------------------------------
c
      subroutine tridgy(a,b,c,w,y)
c
      parameter(l2ng=6,ng=2**l2ng,nb =ng+2,ngm1=ng-1)
      parameter(n=ng-1,np1=n+1,nm1=n-1)
      parameter(nbp1=nb+1)
c
      double precision a,b,c,w,y
c
      double precision x,l,r,d
c
      common/tdspace/x(0:nbp1,0:nbp1)
      common/tdspace/l(0:nbp1,0:nbp1)
      common/tdspace/r(0:nbp1,0:nbp1)
      common/tdspace/d(0:nbp1,0:nbp1)
c
      dimension y(0:nbp1,0:nbp1)
      dimension b(0:nbp1,0:nbp1)
      dimension c(0:nbp1,0:nbp1)
      dimension w(0:nbp1,0:nbp1)
c
c.... code body eliminated ....
c
      end
c
c------------------------------------------------------------
c
      subroutine newu(w2,p)
c
      parameter(l2ng=6,ng=2**l2ng,nb =ng+2,ngm1=ng-1)
      parameter(nbp1=nb+1)
c
      double precision u,uy,vy,ux,vx
      double precision h,k
      common/vel   /u(0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
      common/steps /h,k
c
      double precision p (0:nbp1,0:nbp1)
      double precision w2(0:nbp1,0:nbp1,2),s
c
c.... code body eliminated ....
c
      end
c
c------------------------------------------------------------
c
      subroutine div(s3,w2,d)
c
      parameter(l2ng=6,ng=2**l2ng,nb =ng+2,ngm1=ng-1)
      parameter(nbp1=nb+1)
c
      double precision w2(0:nbp1,0:nbp1,2)
      double precision d (0:nbp1,0:nbp1)
      double precision s3
c
c.... code body eliminated ....
c
      end
c
c------------------------------------------------------------------
c
      subroutine fft2d(a,b,isign)
      parameter(l2ng=6,ng=2**l2ng,nb =ng+2,ngm1=ng-1)
      parameter(n=ng,m=l2ng)
      parameter(nbp1=nb+1)
c
      double precision a(0:nbp1,0:nbp1)
      double precision b(0:nbp1,0:nbp1)
      double precision t1(n),t2(n),t3(n),t4(n)
      double precision pi,ang,ssign,tu1,u1,u2,w1,w2
c
c.... code body eliminated ....
c
      end
c
c-----------------------------------------------------
c
      subroutine phia2d(n)
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
c
c common block variables
c
      double precision k,h
      double precision u,phin,phinn,phia,phian
      double precision uy,vy,ux,vx
c
c other variables
c
c
      common/vel   /u(0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
c
      common/phi   /phin (0:nbp1,0:nbp1)
      common/phi   /phinn(0:nbp1,0:nbp1)
      common/phi   /phia (0:nbp1,0:nbp1)
      common/phi   /phian(0:nbp1,0:nbp1)
c
      common/steps /h,k
c
c.... code body eliminated ....
c
      end
c
c----------------------------------------------------------------
c
      subroutine zag2d(n)
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
c
c common block variables
c
      double precision k,h
      double precision u,zag,zagn
      double precision uy,vy,ux,vx
c
c other variables
c
c
      common/vel   /u(0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
      common/agg   /zag   (0:nbp1,0:nbp1)
      common/agg   /zagn  (0:nbp1,0:nbp1)
c
      common/steps /h,k
c
c.... code body eliminated ....
c
      end
c
c--------------------------------------------------------------------
c
      subroutine plot(time,n,u,f,phia,phin,c,zag,xm,elf,nk)
c
c prints out data(0:ng,0:ng) for subsequent plotting
c
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      parameter(nr=32, nt=16, nr2=nr+2, nt1=nt+1)
      parameter(mmax=200)
c
c argument list variables
c
      double precision u,f,elf
      double precision phin,phia
      double precision c
      double precision zag
      double precision xm
      double precision time
      integer n,nk
c
      dimension u(0:nbp1,0:nbp1,2)
      dimension f(0:nbp1,0:nbp1,2)
      dimension phia (0:nbp1,0:nbp1)
      dimension zag   (0:nbp1,0:nbp1)
c
      dimension xm(mmax,2)
c
      dimension elf (0:nbp1,0:nbp1,0:nr2,nt1)
      dimension phin (0:nbp1,0:nbp1)
      dimension c    (0:nbp1,0:nbp1)
      common/efnum/nfil
c
c.... code body eliminated ....
c
      end
c
c---------------------------------------------------------------------
c
      subroutine markers
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      parameter(mmax=200)
c
c common block variables
c
      double precision u
      double precision h,dlt
      double precision uy,vy,ux,vx,xm
c
c argument list variables
c
      double precision ax,ay,um,umn,vm,vmn,x,xn,y,yn
c
      common/vel   /u(0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
c
      common/steps /h,dlt
      common/fmarkers/xm(mmax,2)
c
c.... code body eliminated ....
c
       end
c
c-------------------------------------------------------------
c
      subroutine phin2d(n)
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      parameter(mmax=200)
c
c common block variables
c
      double precision k,h
      double precision u,phin,phinn,phia,phian
      double precision pen
      double precision uy,vy,ux,vx
c
      common/vel   /u(0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
      common/phi   /phin (0:nbp1,0:nbp1)
      common/phi   /phinn(0:nbp1,0:nbp1)
      common/phi   /phia (0:nbp1,0:nbp1)
      common/phi   /phian(0:nbp1,0:nbp1)
c
      common/diffn /pen
c
      common/steps /h,k
c
c.... code body eliminated ....
c
      end
c
c-------------------------------------------------------------
c
      subroutine chem2d(n)
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      parameter(mmax=200)
c
c common block variables
c
      double precision k,h
      double precision u,c,cn
      double precision pec
      double precision uy,vy,ux,vx
c
      common/vel   /u(0:nbp1,0:nbp1,2)
      common/vel   /uy(0:nbp1,0:nbp1),vy(0:nbp1,0:nbp1)
      common/vel   /ux(0:nbp1,0:nbp1),vx(0:nbp1,0:nbp1)
      common/adp   /c  (0:nbp1,0:nbp1)
      common/adp   /cn (0:nbp1,0:nbp1)
c
      common/steps /h,k
      common/diffc /pec
c
c.... code body eliminated ....
c
      end
c
c-------------------------------------------------
c
      subroutine chlfac(diag,subd,nn)
      double precision   diag(nn),subd(nn)
c
c.... code body eliminated ....
c
      end
c
c-------------------------------------------------
c
      subroutine chlslv(diag,sub,nm1,b,x)
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      parameter (max = nbp1)
c
      double precision   diag(nm1), sub(nm1), b(nm1+1), x(nm1), y(max)
c
c.... code body eliminated ....
c
      end
c
c-------------------------------------------------
c
      subroutine period(ph)
c
c extend ph periodically
c
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      double precision ph
      dimension ph(0:nbp1,0:nbp1)
c
c.... code body eliminated ....
c
      end
c
c-------------------------------------------------
c
      function sol(v,l,r)
      double precision v,l,r,sol
c
c.... code body eliminated ....
c
      end
c
c-----------------------------------------------------------------------
c
      subroutine diffu(ph,phn,k1,alpha)
      parameter (l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter (ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
c
c common block variables
c
      double precision h,k
c
c other variables
c
      double precision k1,alpha,ph,phn
      double precision gdiag,gsub,b,z,y,lam,eta
c
      dimension gdiag(nb-1),gsub(nb-1),b(nb)
      dimension z(nb-1),y(nb-1)
      dimension ph (0:nbp1,0:nbp1)
      dimension phn(0:nbp1,0:nbp1)
c
      common/steps /h,k
c
c.... code body eliminated ....
c
      end
c
c-----------------------------------------------------------------------
c
      subroutine react
      parameter(l2ng=6,ng=2**l2ng,nb=ng+2,ngm1=ng-1)
      parameter(ngp1=ng+1,ngp2=ng+2,nbp1=nb+1)
      parameter(nr=32, nt=16, nr2=nr+2, nt1=nt+1)
c
c common block variables
c
      double precision k,h,cnd1,cnd2,cnd3,cnd4,cnd5
      double precision elf,elfn,phin,phinn,phia,phian,c,cn,zag,zagn
      double precision dr,r
c
c other variables
c
      double precision rc,y,yn,temp1,temp2,temp3,temp4,c4,alpha
c
      common/link  /elf (0:nbp1,0:nbp1,0:nr2,nt1)
      common/link  /elfn(0:nbp1,0:nbp1,0:nr2,nt1)
      common/phi   /phin (0:nbp1,0:nbp1)
      common/phi   /phinn(0:nbp1,0:nbp1)
      common/phi   /phia (0:nbp1,0:nbp1)
      common/phi   /phian(0:nbp1,0:nbp1)
      common/adp   /c  (0:nbp1,0:nbp1)
      common/adp   /cn (0:nbp1,0:nbp1)
      common/agg   /zag(0:nbp1,0:nbp1)
      common/agg   /zagn(0:nbp1,0:nbp1)
      common/cnd   /cnd1,cnd2,cnd3,cnd4,cnd5
      common/steps /h,k
      common/rsize/dr(nr2),r(0:nr2),nk
c
      dimension rc (0:nbp1)
      dimension y  (0:nbp1,4), yn(0:nbp1,4)
c
c.... code body eliminated ....
c
      end
