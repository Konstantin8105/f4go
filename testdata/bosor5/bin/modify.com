#
# modify.com
# modify.com allows the user to modify data in a {case}.SEGn file.

echo -n "Please enter case name: "
set case = $<
ln -s ${BOSOR5}/execute/PROMPT.DAT PROMPT.DAT
ln -s ${BOSOR5}/execute/PROMPT2.DAT PROMPT2.DAT
ln -s ${BOSOR5}/execute/PROMPT3.DAT PROMPT3.DAT
#dbx -I ../dbx_sources ${BOSOR5}/execute/modify.${MACHINE}
${BOSOR5}/execute/modify.${MACHINE} $case
'rm' PROMPT*.DAT fort.*

# end modify.com
