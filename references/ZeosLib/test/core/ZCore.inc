
{$IFDEF LINUX}{$DEFINE UNIX}{$ENDIF} // Kylix legacy.
{$IFNDEF UNIX}   // Linux here breaks OS X, Solaris and *BSD.

{$I ..\..\src\Zeos.inc}

{$ELSE}

{$I ../../src/Zeos.inc}

{$ENDIF}