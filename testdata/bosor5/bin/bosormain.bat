#
# bosormain.bat
# bosormain.bat prepares the user environment for execution of bosormain.

set case = $1
echo ""
echo "Running BOSOR5: bosormain, case: $case"
echo ""
'rm' fort.* >>& /dev/null
'rm' ${case}.ERR >>& /dev/null
'rm' ${case}.MAI >>& /dev/null
cp ${case}.IMP ${case}.MAI
ln -s ${case}.RAN fort.11 
ln -s ${BOSOR5}/execute/PROMPT.DAT PROMPT.DAT
ln -s ${BOSOR5}/execute/PROMPT3.DAT PROMPT3.DAT
echo  Executing bosormain
#dbx -I ../dbx_sources ${BOSOR5}/execute/bosormain.${MACHINE}
#exit 99
${BOSOR5}/execute/bosormain.${MACHINE} $case < ${case}.IMP > ${case}.ERR 
set stat = $status
if ($stat == 0) then
   echo  Normal termination: bosormain
else
   echo  Abnormal termination: bosormain
   echo  "Exit status: $stat"
   exit 2
endif
cat ${case}.ERR >> ${case}.MAI
cat ${case}.MAI >>! ${case}.OUT
'rm'  fort.* PROMPT*.DAT ${case}.ERR >& /dev/null
echo "Case ${case} mainprocessor run completed."
echo 'Next, give the command "mainsetup" or "postsetup"'
echo ""
# end bosormain.bat
