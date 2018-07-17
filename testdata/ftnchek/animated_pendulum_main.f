      program AnimatedPendulum
**********************************************
*       Program:    Simple_Pendulum_Solver   *
*       Written_By: Brian Downing            *
*                   Fordham University       *
*       Date:       October 23rd, 1990       *
**********************************************

        Implicit None
*
*       Declare double precision variables.
*
        double precision a,g,l,t,dt,w,Pi,Theta,ThetaM
*
*       Initialize some constant values.
*
        Pi = 4.0*ATAN(1.0)
*
*       Clear screen and then prompt for real numbers for variables.
*
        Call CLS
        write(5,100)
        read*,dt

        write(5,200)
        read*,Theta

        write(5,300)
        read*,l

*
*       Initialize other variables.
*
        g  = 9.81
        ThetaM = Theta*Pi/180.0     
        Theta  = ThetaM
        t  = 0.0
        w  = 0.0
        a  = (-g/l)*DSIN(Theta)
*
*       Loop until finished (angle.eq.-Theta_naught .or. omega = 0)?
*
        Call EnterGraphicsMode
        do while (.true.)
            a = (-g/l)*DSIN(Theta)
            w = w + a*dt
            Theta = Theta + w*dt
            t = t + dt
            Call PlotBall(Theta)
            if (Theta.le.(-ThetaM)) goto 10
        enddo
10      continue
*
*       Print out final results.
* 
        print*,'Finished calculations.'
        write(5,20)a,w,Theta,t
*
*       Program format statements.
*
20      format(1x,'Acceleration',T15,'= ',D12.4/
     1         1x,'Omega',T15,'= ',D12.4/
     2         1x,'Theta',T15,'= ',D12.4/
     3         1x,'Time',T15,'= ',D12.4/)
100     format(1x,'Enter real number for delta time in seconds: ',$)
200     format(1x,'Enter real number for Theta in degrees: ',$)
300     format(1x,'Enter real number for Length in meters: ',$)
      Call ExitGraphicsMode
      End
