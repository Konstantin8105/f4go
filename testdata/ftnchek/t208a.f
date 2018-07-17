      SUBROUTINE  DDIAPA (IX,IY)
C     (Add to Path)
C     Add the point (IX,IY) to the current path, which is assumed
C     to have  already been  started by  a call  to DDIBPA.   The
C     current point and visibility are updated in COMMON.
C     (27-May-1991)
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     EXTERNAL REFERENCES (FUNCTION,SUBROUTINE,COMMON)
C
C     EXTERNAL REFS       TKIOLL,      TKIOWB,      TKIOWH,      TKIOWN
C
      INTEGER             TKIOLL
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     NON-COMMON VARIABLES
C
C     SETS                BLANK
C
      INTEGER             BLANK,       DUMMY,       IX,          IY
C
C-----------------------------------------------------------------------
C                         P o s t S c r i p t
C           D i s p l a y   D e v i c e   I n t e r f a c e
C                       C O M M O N   B l o c k
C
C     DSSIZE                   display surface size in cm
C     IMAGE                    image transformation in effect
C     (LASTX,LASTY,VILAST)     last position and visibility of pen
C     LINEIN                   line intensity (0..1 scale)
C     LINEWT                   line weight (1..25 scale)
C     MAGFAC                   resolution magnification factor
C     (MOVEX,MOVEY)            position of first point in path
C     NPATH                    number of line segments in current page
C     PLSTEP                   plotter step size in cm
C     (PSXOFF,PSYOFF)          image offset in PostScript units (bp)
C     ROTATE                   plot frame is rotated
C     (SX,SY)                  scale factors (unit square to plot steps)
C     TIMAGE(*,*)              image transformation matrix
C     (XMAX,YMAX,ZMAX)         normalized device space extents
C     (MAXX,MAXY,MINX,MINY)    actual coordinate limits reached
C
C-----------------------------------------------------------------------
      INTEGER             LASTX,       LASTY,       LINEWT,      MAGFAC
      INTEGER             MAXX,        MAXY,        MINX,        MINY
      INTEGER             MOVEX,       MOVEY,       NPATH,       PSXOFF
      INTEGER             PSYOFF
C
      LOGICAL             DVINIT,      IMAGE,       ROTATE,      VILAST
C
      REAL                DSSIZE,      LINEIN,      PLSTEP,      SX
      REAL                SY,          TIMAGE,      XMAX,        YMAX
      REAL                ZMAX
C
      COMMON / DDI01  /   DSSIZE,      LINEIN,      PLSTEP,      SX
      COMMON / DDI01  /   SY,          TIMAGE(4,4), XMAX,        YMAX
      COMMON / DDI01  /   ZMAX,        LASTX,       LASTY,       LINEWT
      COMMON / DDI01  /   MAGFAC,      MAXX,        MAXY,        MINX
      COMMON / DDI01  /   MINY,        MOVEX,       MOVEY,       NPATH
      COMMON / DDI01  /   PSXOFF,      PSYOFF,      DVINIT,      IMAGE
      COMMON / DDI01  /   ROTATE,      VILAST
C
C
      DATA                BLANK     / 32 /
C
C     We can generate an  entry of size "-ddddd -ddddd R "; try to
C     keep line lengths under 80 characters.
C
      IF (TKIOLL(DUMMY) .GT. 64) CALL TKIOWH (2H$N,2)
C
C     Use relative coordinates for lines to reduce the  number of
C     bytes in the  verbose  PostScript  command language.   This
C     shortens the plot file by 5 to 10 percent.
C
C     We optimize output into 3 different forms:
C
C     # # R   (relative lineto)
C     # X     (relative lineto with delta-y = 0)
C     # Y     (relative lineto with delta-x = 0)
C
C     The  latter two cases  occur frequently enough  to be worth
C     taking advantage of  to compress the  output and reduce the
C     parsing  time.    We cannot  discard zero length  segments,
C     however, because that  would discard single  points from  a
C     point plot.
C
      IF (IX .EQ. LASTX) GO TO 10
      IF (IY .EQ. LASTY) GO TO 20
      CALL TKIOWN (IX-LASTX)
      CALL TKIOWB (BLANK)
      CALL TKIOWN (IY-LASTY)
      CALL TKIOWH (3H R ,3)
      GO TO 30
   10 CALL TKIOWN (IY-LASTY)
      CALL TKIOWH (3H Y ,3)
      GO TO 30
   20 CALL TKIOWN (IX-LASTX)
      CALL TKIOWH (3H X ,3)
   30 NPATH = NPATH + 1
      LASTX = IX
      LASTY = IY
      VILAST = .TRUE.
      MAXX = MAX0(MAXX,IX)
      MINX = MIN0(MINX,IX)
      MAXY = MAX0(MAXY,IY)
      MINY = MIN0(MINY,IY)
C
C     Insert a  line break  periodically to  prevent  excessively
C     long lines.
C
C     IF (MOD(NPATH,10) .EQ. 0) CALL TKIOWH (2H$N,2)
C
   40 RETURN
      END
