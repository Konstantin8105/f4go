#
# assemble.com
# assemble.com assembles the {case}.SEG files to {case}.ALL in preparation
# for a BOSOR5 run.

echo -n "Please enter case name: "
set case = $<
echo -n "How many segments in the model (excluding global data)? "
set nseg = $<
${BOSOR5}/execute/assemble.${MACHINE} $case $nseg

# end assemble.com
