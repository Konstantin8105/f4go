      doubleprecisionfunctiondnrm2(n,dx,incx)
      integernext
      doubleprecisiondx(1),cutlo,cuthi,hitest,sum,xmax,zero,one
      datazero,one/0.0d0,1.0d0/
c
c     euclideannormofthen-vectorstoredindx()withstorage
c     incrementincx.
c     ifn.le.0returnwithresult=0.
c     ifn.ge.1thenincxmustbe.ge.1
c
c     c.l.lawson,1978jan08
c
c     fourphasemethodusingtwobuilt-inconstantsthatare
c     hopefullyapplicabletoallmachines.
c     cutlo=maximumofdsqrt(u/eps)overallknownmachines.
c     cuthi=minimumofdsqrt(v)overallknownmachines.
c     where
c     eps=smallestno.suchthateps+1..gt.1.
c     u=smallestpositiveno.(underflowlimit)
c     v=largestno.(overflowlimit)
c
c     briefoutlineofalgorithm..
c
c     phase1scanszerocomponents.
c     movetophase2whenacomponentisnonzeroand.le.cutlo
c     movetophase3whenacomponentis.gt.cutlo
c     movetophase4whenacomponentis.ge.cuthi/m
c     wherem=nforx()realandm=2*nforcomplex.
c
c     valuesforcutloandcuthi..
c     fromtheenvironmentalparameterslistedintheimslconverter
c     documentthelimitingvaluesareasfollows..
c     cutlo,s.p.u/eps=2**(-102)forhoneywell.closesecondsare
c     univacanddecat2**(-103)
c     thuscutlo=2**(-51)=4.44089e-16
c     cuthi,s.p.v=2**127forunivac,honeywell,anddec.
c     thuscuthi=2**(63.5)=1.30438e19
c     cutlo,d.p.u/eps=2**(-67)forhoneywellanddec.
c     thuscutlo=2**(-33.5)=8.23181d-11
c     cuthi,d.p.sameass.p.cuthi=1.30438d19
c     datacutlo,cuthi/8.232d-11,1.304d19/
c     datacutlo,cuthi/4.441e-16,1.304e19/
      datacutlo,cuthi/8.232d-11,1.304d19/
c
      if(n.gt.0)goto10
      dnrm2=zero
      goto300
c
   10 assign30tonext
      sum=zero
      nn=n*incx
c     beginmainloop
      i=1
   20 gotonext,(30,50,70,110)
   30 if(dabs(dx(i)).gt.cutlo)goto85
      assign50tonext
      xmax=zero
c
c     phase1.sumiszero
c
   50 if(dx(i).eq.zero)goto200
      if(dabs(dx(i)).gt.cutlo)goto85
c
c     prepareforphase2.
      assign70tonext
      goto105
c
c     prepareforphase4.
c
  100 i=j
      assign110tonext
      sum=(sum/dx(i))/dx(i)
  105 xmax=dabs(dx(i))
      goto115
c
c     phase2.sumissmall.
c     scaletoavoiddestructiveunderflow.
c
   70 if(dabs(dx(i)).gt.cutlo)goto75
c
c     commoncodeforphases2and4.
c     inphase4sumislarge.scaletoavoidoverflow.
c
  110 if(dabs(dx(i)).le.xmax)goto115
      sum=one+sum*(xmax/dx(i))**2
      xmax=dabs(dx(i))
      goto200
c
  115 sum=sum+(dx(i)/xmax)**2
      goto200
c
c
c     prepareforphase3.
c
   75 sum=(sum*xmax)*xmax
c
c
c     forrealord.p.sethitest=cuthi/n
c     forcomplexsethitest=cuthi/(2*n)
c
   85 hitest=cuthi/float(n)
c
c     phase3.sumismid-range.noscaling.
c
      do95j=i,nn,incx
      if(dabs(dx(j)).ge.hitest)goto100
   95 sum=sum+dx(j)**2
      dnrm2=dsqrt(sum)
      goto300
c
  200 continue
      i=i+incx
      if(i.le.nn)goto20
c
c     endofmainloop.
c
c     computesquarerootandadjustforscaling.
c
      dnrm2=xmax*dsqrt(sum)
  300 continue
      return
      end
