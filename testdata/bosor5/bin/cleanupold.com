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
'mkdir' temp
'mv' ${case}.DOC temp
'mv' ${case}.IMP* temp
'mv' ${case}.IPP* temp
'mv' ${case}.[a-z]* temp
'rm' ${case}.* fort.* FOR0* >& /dev/null
'mv' temp/* .
'rm' -rf temp
'cp' ${case}.DOC ${case}.ALL

echo "You now have the following files associated with case ${case}:"
'ls' -t ${case}.*

# end cleanup.com
