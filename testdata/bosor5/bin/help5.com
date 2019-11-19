#
# help5.com
# help5 runs the "VMS-like" on-line help utility.

echo "help5 is not supported on this machine.  Please use the on-line,"
echo "in-program help facility or consult the file, doc/bosor5.story."
exit 0

ln -s ${BOSOR5}/execute BOSOR5
BOSOR5/help5.${MACHINE}
/bin/rm BOSOR5

# end help5.com
