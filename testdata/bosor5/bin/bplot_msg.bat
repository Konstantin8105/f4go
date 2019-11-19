#
# This script prints a message to the screen of the user when running
# BOSORPLOT with the PostScript option.  If running BOSORPLOT in Xgraph
# mode, the message is not printed.
#
#  Arguments:   $0  -  bplot_msg.bat
#               $1  -  $method ( (P)splot or (X)graph )
#               $2  -  $psfile (name of the newly-created Postscript file)
#
set method = $1
set psfile = $2

if ($method:q != "x" && $method:q != "X") then # Print message.

  echo ""
  echo "The PostScript file, $psfile, has been created."
  echo "Please choose one of the three options below:"
  echo ""
  echo "   1) Rename the PostScript file this is useful if"
  echo "      you don't have access to a PostScript printer on your"
  echo "      machine, but you wish to save to a file so you can later"
  echo "      transfer it to a different machine for printing."
  echo ""
  echo "         Example:  mv $psfile plot1.ps"
  echo ""
  echo '   2) Enter an "lpr" command.  This is useful if your default'
  echo "      printer is not PostScript, but there is a PostScript"
  echo "      printer available on your system."
  echo ""
  echo "         Example:  lpr -PApplelaser $psfile"
  echo ""
  echo "   3) Press the return key.  This executes the command:"
  echo ""
  echo "                   lpr $psfile"
  echo ""
  echo "      This assumes that your default printer is a PostScript"
  echo "      printer."
  echo ""
  echo -n "Enter your command> "
  set command = $<
  if ($command:q == "") then
    echo "Printing PostScript plot on the default printer..."
    lpr $psfile
  else
    echo "Executing user command: $command"
    $command
  endif

endif # if $method ...

