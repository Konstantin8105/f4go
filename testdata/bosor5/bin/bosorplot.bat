#
# bosorplot.bat
# bosorplot.bat takes the name of a file containing a set of plot data.  The
# first line contains the column headings, and all lines below contain data.
# One or more new files are created from this one file.  Each of these new
# files is sent to xgraph for graphing. $1 is the case, $2 is the method of 
# plotting, and $3 is the name of the input file.

set case = $1
set method = $2
set infile = $3
set psfile = "metafile.ps"

if ($method:q == "X" || $method:q == "x") then
  set PLOT = (${BUSHNELL}/bin/xgraph.${MACHINE} -bg White -fg Black -zg Magenta -t ${infile:q})
else
  set PLOT = (${BUSHNELL}/bin/plotps.${MACHINE} -t ${infile:q})
endif
set GAWK = "${BUSHNELL}/bin/gawk.${MACHINE}"
set PLOTMSG = (${BOSOR5}/bin/bplot_msg.bat $method $psfile)
set loptions = ""
set soptions = (-bb -tk -nl -m)
set outfile = /tmp/out$$

ln -s $infile in$$
set linfile = in$$

switch ($infile)

  case "${case}..R,Z_LOADSTEP??":
  case "${case}..R,Z_EIGENMODE*":
    echo "Plotting: Undeformed & Deformed Axial Station as a function of Radius"
    set x = "Radius"
    set y = "Axial Station"
    if ($method:q == "X" || $method:q == "x") then
      echo '"Undeformed' >! $outfile
      $GAWK 'BEGIN {p = 0} \
             {if (p == 1) {print $2, $3}; if ($0 == "*") {getline; p = 1}}' \
            $linfile >> $outfile
      echo "" >> $outfile
      echo '"Deformed' >> $outfile
      $GAWK 'BEGIN {p = 0} \
             {if (p == 1) {print $4, $5}; if ($0 == "*") {getline; p = 1}}' \
            $linfile >> $outfile
      $PLOT $soptions -x ${x:q} -y ${y:q} $outfile
    else
      echo "=markeroff" >! $outfile
      echo "=scaleline(0.65)" >> $outfile
      echo "=samescale" >> $outfile
      echo "=border" >> $outfile
      echo "=xlabel($x)" >> $outfile
      echo "=ylabel($y)" >> $outfile
      echo "=gridlines(0, 0)" >> $outfile
      echo "" >> $outfile
      $GAWK -f ${BOSOR5}/bin/geom.awk pass=1 $infile >> $outfile
      $GAWK -f ${BOSOR5}/bin/geom.awk pass=2 $infile >> $outfile
      $PLOT $outfile >! $psfile
      $PLOTMSG
    endif
    breaksw

  case "${case}..AXISYM_LOADSTEP??":
    echo "4 plots..."
    set x = "Arc Length"
# file 1
    echo "Plot 1: Normal & Meridional Displacements as a function of Arc Length"
    set y = "Displacement"
    echo '"Normal' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $3}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Meridional' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $4}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
# file 2
    echo "Plot 2: Rotation (BETA) as a function of Arc Length"
    set y = "Rotation (BETA)"
    echo '"Meridional' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $5}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
# file 3
    echo "Plot 3: Meridional & Circumferential Resultants as a function of Arc Length"
    set y = "Resultant"
    echo '"Meridional' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $6}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Circumferential' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $7}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
# file 4
    echo "Plot 4: Meridional Moment as a function of Arc Length"
    set y = "Meridional Moment"
    echo '"Meridional Moment' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $8}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
    breaksw

  case "${case}..STRESS_LOADSTEP??":
    echo "3 plots..."
    set x = "Arc Length"
# file 1
    echo "Plot 1: Merid. Stress in Left & Right fibers as a func. of Arc Length"
    set y = "Meridional Stress"
    echo '"Leftmost fiber' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $3}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Rightmost fiber' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $4}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
# file 2
    echo "Plot 2: Circumf. Stress in Left & Right fibers as a func. of Arc Length"
    set y = "Circumferential Stress"
    echo '"Leftmost fiber' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $5}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Rightmost fiber' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $6}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
# file 3
    echo "Plot 3: Effective Stress in Left & Right fibers as a func. of Arc Length"
    set y = "Effective Stress"
    echo '"Leftmost fiber' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $7}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Rightmost fiber' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $8}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
    breaksw

  case "${case}..STRAIN_LOADSTEP??":
    echo "2 Plots..."
    set x = "Arc Length"
# file 1
    echo "Plot 1: Meridional & Circumferential Strain as a function of Arc Length"
    set y = "Strain"
    echo '"Meridional' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $3}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Cirumferential' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $4}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
# file 2
    echo "Plot 2: Merid. & Circumf. Curvature Change as a function of Arc Length"
    set y = "Curvature Change"
    echo '"Meridional' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $5}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Circumferential' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $6}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
    breaksw

  case "${case}..EIGENMODE*":
    echo "Plotting: (u, v, w) Displacements as a function of Arc Length"
    set x = "Arc Length"
    set y = "Displacement"
    echo '"Meridional' >! $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $3}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Circumferential' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $4}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    echo "" >> $outfile
    echo '"Normal' >> $outfile
    $GAWK 'BEGIN {p = 0} \
	   {if (p == 1) {print $2, $5}; if ($0 == "*") {getline; p = 1}}' \
	  $linfile >> $outfile
    if ($PLOT[1] == "${BUSHNELL}/bin/xgraph.${MACHINE}") then
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile
    else
      $PLOT $loptions -x ${x:q} -y ${y:q} $outfile >! $psfile
      $PLOTMSG
    endif
    breaksw

endsw # $infile

if ($method:q == "X" || $method:q == "x") then
  /bin/rm -f $psfile
endif

/bin/rm -f in$$ /tmp/out$$
# end bosorplot.bat
