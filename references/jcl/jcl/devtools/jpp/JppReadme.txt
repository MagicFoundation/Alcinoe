jpp ist a modified version of Barry Kelly's Pascal Preprocessor ppp.

When called with the "-c" option ("Process conditional directives"), ppp
resolves /all/ conditional directives.  Symbols not passed on the command line
per "-d" option are treated as undefined.  No $IFDEF, $IFNDEF, $ELSE or $ENDIF
is left in the processed source files.

In contrast, jpp considers symbols not passed on the command line as unknown and
leaves them alone, that is, related source code remains untouched.  To specify a
symbol as undefined, the "-u" option has been introduced.

Further, a new option "-x" allows to specify a file name prefix (which may
include a path portion).

The example command line below generates a file JclQGraphics.pas in subdirectory
CLX from file Graphics.cb located in the current directory.  Symbols "VisualCLX"
and "COMPILER6_UP" are specified as defined, "Bitmap32" and "VCL" as undefined.

  jpp -c -dVisualCLX -dCOMPILER6_UP -uBitmap32 -uVCL -xCLX\JclQ Graphics.cb


