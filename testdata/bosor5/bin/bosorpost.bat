#
# bosorpost.bat
# bosorpost.bat prepares the user environment for execution of bosorpost.

set case = $1
echo ""
echo "Running BOSOR5: bosorpost, case: $case"
echo ""
'rm' fort.* PROMPT*.DAT >>& /dev/null
'rm' ${case}.POS  >>& /dev/null
cp ${case}.IPP ${case}.POS
ln -s ${case}.RAN fort.11 
ln -s ${BOSOR5}/execute/PROMPT.DAT PROMPT.DAT
ln -s ${BOSOR5}/execute/PROMPT3.DAT PROMPT3.DAT
echo  Executing bosorpost
#dbx -I ../dbx_sources ${BOSOR5}/execute/bosorpost.${MACHINE}
#exit 99
${BOSOR5}/execute/bosorpost.${MACHINE} $case < ${case}.IPP >! ${case}.ERR 
set stat = $status
if ($stat == 0) then
   echo  Normal termination: bosorpost
else
   echo  Abnormal termination: bosorpost
   echo  "Exit status: $stat"
   exit 2
endif
cat ${case}.ERR >> ${case}.POS
cat ${case}.POS >>! ${case}.OUT
'rm'  fort.* PROMPT*.DAT ${case}.ERR >& /dev/null
echo "Case ${case} postprocessor run completed."
echo 'Next, choose from the commands:'
echo '	BOSORPLOT, MAINSETUP, POSTSETUP, INPUT, CLEANUP, GETSEGS, OR MODIFY'
echo ""
# end bosorpost.bat
