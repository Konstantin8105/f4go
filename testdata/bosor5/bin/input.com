#
# input.com
# This procedure is for generating a file containing data for structural
# segments and global and constraint data for BOSOR5.

echo -n "Please enter case name: "
set case = $<
ln -s ${BOSOR5}/execute/PROMPT.DAT PROMPT.DAT
ln -s ${BOSOR5}/execute/PROMPT2.DAT PROMPT2.DAT
ln -s ${BOSOR5}/execute/PROMPT3.DAT PROMPT3.DAT
${BOSOR5}/execute/input.${MACHINE} $case
'rm' PROMPT*.DAT fort.*

# end input.com
