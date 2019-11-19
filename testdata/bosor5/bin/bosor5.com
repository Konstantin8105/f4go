#
# bosor5.com
# bosor5.com sets up the aliases associated with BOSOR5.

set BIN = ${BOSOR5}/bin

alias a alias
a help5	     ${BIN}/help5.com
a modify     ${BIN}/modify.com
a input	     ${BIN}/input.com
a getsegs    ${BIN}/getsegs.com
a cleanup    ${BIN}/cleanup.com
a assemble   ${BIN}/assemble.com
a bosorread  ${BIN}/bosorread.com
a mainsetup  ${BIN}/mainsetup.com
a postsetup  ${BIN}/postsetup.com
a bosormain  ${BIN}/bosormain.com
a bosorpost  ${BIN}/bosorpost.com
a bosorplot  ${BIN}/bosorplot.com

a HELP5	     ${BIN}/help5.com
a MODIFY     modify
a INPUT	     input
a GETSEGS    getsegs
a CLEANUP    cleanup
a ASSEMBLE   assemble
a BOSORREAD  bosorread
a MAINSETUP  mainsetup
a POSTSETUP  postsetup
a BOSORMAIN  bosormain
a BOSORPOST  bosorpost
a BOSORPLOT  bosorplot
unalias a

echo ""
echo BOSOR5 commands have been activated.
echo Below are the BOSOR5 commands, listed in the order in which you
echo would probably use them.
echo ""
#echo help5	- get information on BOSOR5
echo input	- interactive input of data for each segment
echo assemble	- concatenates the segment data files
echo bosorread	- runs BOSOR5 pre-processor
echo mainsetup	- interactive input of data for main processor
echo bosormain	- runs BOSOR5 main processor
echo postsetup	- interactive input of data for post processor
echo bosorpost	- runs BOSOR5 post processor
echo bosorplot	- generates plot files from most previous run
echo cleanup	- delete all {case} files except for .DOC file
echo getsegs	- generate segment files from .DOC file
echo modify	- interactively modify a segment or global data file
echo ""
echo Please consult the following sources for more information about BOSOR5:
#echo '1. HELP5 file (type "help5")'
echo "1. .../bosor5/doc/bosor5st.ory (good idea to print this file)"
echo "2. Documents listed under HELP5 OVERVIEW DOC"
echo ""

# Add the user's name to the user.list file at the top level
# directory.  This is done so that an administrator can keep track of
# who uses what program, etc. so that he/she might notify these users of
# upgrades and changes.
if (-e ${BUSHNELL}/user.list) then
  echo -n "${USER}@" >> ${BUSHNELL}/user.list
  echo -n "`hostname` " >> ${BUSHNELL}/user.list
  echo -n "$MACHINE " >> ${BUSHNELL}/user.list
  echo -n "BOSOR5 " >> ${BUSHNELL}/user.list
  echo  "`date`" >> ${BUSHNELL}/user.list
endif
