#
#     BOSORPLOT COM FILE...
# bosorplot.com performs all of the necessary processing of the {case}.PLT2
# file so that graphs can be produced using the Xwindows utility, xgraph or
# plotbosor.

set GAWK = "${BUSHNELL}/bin/gawk.${MACHINE}"
echo -n 'Please enter the BOSOR5 case name: '
set case = $<

echo ""
echo -n 'Do you want to use Xgraph or create a PostScript file? (Choose X or P) '
set method = $<
if !($method:q == "X" || $method:q == "x") then
  set method = "p"
endif
echo ""

if !(-e ${case}.PLT2) then
  echo "ERROR: ${case}.PLT2 does not exist in the current working directory."
  exit (1)
endif

echo "One moment please..."
$GAWK -f ${BOSOR5}/bin/bosorplot.awk ${case}.PLT2

while (1) 
  echo ""
  echo "Text file(s) have been created containing plot data.  The names of the"
  echo "files explain to a greater or lesser extent what the data represent."
  echo "Some plot files contain data for more than one plot."

# Find all relevant plot files that should be split and sent to xgraph.
  if !(-e /tmp/table$$) then
    if (($MACHINE == "sgi") || ($MACHINE == "hp700")) then
      /bin/ls ${case}..* | nl >! /tmp/table$$
    else
      /bin/ls -1 ${case}..* | cat -n >! /tmp/table$$
    endif
  endif
  $GAWK '{printf "%d)\t%s\n", $1, $2}' /tmp/table$$

  echo "CR)     to QUIT"
  echo -n "Please choose the number of the file you wish to plot: "
  set choice = $<
  set npfiles = "`$GAWK 'END {print NR}' /tmp/table$$`"

# Check for exit condition
  $GAWK -v a=$choice 'BEGIN {found = 0; choice = a}; $1 == choice {found = 1}; END {exit found}' /tmp/table$$
  if !($status) then
    /bin/rm ${case}..* BOSOR5 /tmp/*$$ >& /dev/null
    exit($status)
  endif

# Build command to extract name of plot file to split.
  echo -n "'$choice ==" >! /tmp/com$$
  echo -n ' $1 { print $2 }' >> /tmp/com$$
  echo "' /tmp/table$$" >> /tmp/com$$
  set command = "`cat /tmp/com$$`"
  set plotfile = "`$GAWK $command`"

# Call bosorplot.bat
  ${BOSOR5}/bin/bosorplot.bat $case $method $plotfile

end # while
