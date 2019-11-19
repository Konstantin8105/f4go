#
# cleanup.com
# cleanup.com is used for clearing out old files during testing or between
# executions.  Do not use it unless your {case}.DOC file is good!

echo "This procedure clears out old files and replaces the {case}.ALL file"
echo "with the most recent {case}.DOC file."
echo ""
echo "WARNING:	Do not use this procedure unless you are sure that you have"
echo "		a good {case}.DOC file."

echo -n "Please enter the case name: "
set case = $<
/bin/rm -f ${case}.BLK >& /dev/null
/bin/rm -f ${case}.LAB >& /dev/null
/bin/rm -f ${case}.OUT >& /dev/null
/bin/rm -f ${case}.PLT2 >& /dev/null
/bin/rm -f ${case}.RAN >& /dev/null
/bin/rm -f ${case}.MAI >& /dev/null
/bin/rm -f ${case}.POS >& /dev/null
/bin/rm -f ${case}.m.ERR >& /dev/null
   'rm'  FOR0* fort.* >& /dev/null
   'cp'  ${case}.DOC  ${case}.ALL
echo "You now have the following files associated with case ${case}:"
'ls' -t ${case}.*

# end cleanup.com
