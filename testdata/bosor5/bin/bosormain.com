#
# bosormain.com
# bosormain.com sets up a run of BOSOR5, prompting the user for casename, 
# background/foreground execution and, if background, execution priority.

set name = "bosormain"
echo -n  "Enter case name: "
set casename = $<
echo -n  "B (background) or F (foreground): "
set ground = $<
if (($ground:q == "B") || ($ground:q == "b")) then
   echo -n  "H (high) or L (low) priority: "
   set priority = $<
   if (($priority:q == "H") || ($priority:q == "h")) then
      set  p = 5
   else
      set  p = 15
   endif
   echo "Diagnostics will be mailed to you upon program termination."
   (nice +$p time ${BOSOR5}/bin/${name}.bat $casename) |& mail $USER \
    >& /dev/null &
else
   time ${BOSOR5}/bin/${name}.bat $casename
endif

# end bosormain.com
