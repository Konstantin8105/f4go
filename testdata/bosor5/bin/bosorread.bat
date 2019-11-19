#
# bosorread.bat
# bosorread.bat prepares the user environment for execution of bosorread.

set case = $1
echo ""
echo "Running BOSOR5: bosorread, case: $case"
echo ""
/bin/rm fort.* FOR0* >>& /dev/null
/bin/rm ${case}.ERR >>& /dev/null
/bin/rm ${case}.OUT >>& /dev/null
/bin/rm ${case}.BLK >>& /dev/null
/bin/rm ${case}.RAN >>& /dev/null
/bin/rm ${case}.000 >>& /dev/null

if (!( -e ${case}.ALL)) then
  if (-e ${case}.DOC) then
    cp ${case}.DOC ${case}.ALL
  else
    echo "To run bosorread you need a ${case}.ALL or ${case}.DOC file."
    exit 1
  endif
endif

ln -s ${case}.RAN fort.11 
ln -s ${BOSOR5}/execute/PROMPT.DAT PROMPT.DAT
ln -s ${BOSOR5}/execute/PROMPT2.DAT PROMPT2.DAT
ln -s ${BOSOR5}/execute/PROMPT3.DAT PROMPT3.DAT
echo  Executing bosorread
#exit 99
${BOSOR5}/execute/bosorread.${MACHINE} $case < ${case}.ALL > ${case}.ERR 
set stat = $status
if ($stat == 0) then
   echo  Normal termination: bosorread
else
   echo  Abnormal termination: bosorread
   echo  "Exit status: $stat"
   exit 2
endif
/bin/rm  fort.* ${case}.ERR PROMPT*.DAT >& /dev/null
echo "Case ${case} preprocessor run completed."
echo 'Next, give the command "mainsetup"'
echo ""
# end bosorread.bat
