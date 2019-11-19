#
# getsegs.com
# getsegs.com divides a {case}.ALL into its constituent {case}.SEGn files.

echo -n "Please enter case name: "
set case = $<
${BOSOR5}/execute/getsegs.${MACHINE} $case

# end getsegs.com
