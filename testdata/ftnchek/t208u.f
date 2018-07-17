      integer             MAXTOK
      parameter           (MAXTOK = 10)
      character*(MAXTOK)  toklst, tokls2*(2*MAXTOK), tokls3*(MAXTOK**2)
      character*(MAXTOK)  tokarr(100), tokar2(200)*(2*MAXTOK), 
     x    tokar3(300)*(MAXTOK**2)

      integer             MAXTK1
      parameter           (MAXTK1 = 10)

*     Test with comment lines between continuation lines
      character*(MAXTK1)  
*     intervening comment
     x    tk1lst, 
*     intervening comment
     x    tk1ls2*(2*MAXTK1), 
*     intervening comment
     x    tk1ls3*(MAXTK1**2)


*     Test with blank lines between continuation lines
      integer             MAXTK2
      parameter           (MAXTK2 = 10)
      character*(MAXTK2)

     x    tk2arr(100),

     x    tk2ar2(200)*(2*MAXTK2), 

     x    tk2ar3(300)*(MAXTK2**2)
      end
