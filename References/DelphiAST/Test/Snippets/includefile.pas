{$I includefile.inc}
{$INCLUDE 'include file2.inc'}
unit includefile;

interface

{$IFNDEF TESTINCLUDE}
  this must be ignored
{$ENDIF}

{$IFNDEF TESTINCLUDE2}
  this must be ignored
{$ENDIF}

implementation
                
end.
