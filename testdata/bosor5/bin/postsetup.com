#
# postsetup.com
# postsetup.com initializes the BOSOR5 post processor.

echo -n "Please enter case name: "
set case = $<
ln -s ${BOSOR5}/execute/PROMPT.DAT PROMPT.DAT
ln -s ${BOSOR5}/execute/PROMPT2.DAT PROMPT2.DAT
ln -s ${BOSOR5}/execute/PROMPT3.DAT PROMPT3.DAT
${BOSOR5}/execute/postsetup.${MACHINE} $case
'rm' PROMPT*.DAT fort.*

# end postsetup.com
