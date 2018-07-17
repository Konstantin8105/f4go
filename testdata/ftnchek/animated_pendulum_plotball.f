
      SubRoutine PlotBall (Theta)
c       Clear the screen and draw/redraw the ball circle.
      Double Precision Theta
      Integer Row,Col
        Row=Int(300.0*DSIN(Theta))
        Col=Int(300.0*DCOS(Theta))
        Write(*,*)'S(E)P[Col,Row]C[+20]'
        Return
      End
