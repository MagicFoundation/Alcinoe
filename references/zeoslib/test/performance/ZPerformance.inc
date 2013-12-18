{$IFDEF LINUX}{$DEFINE UNIX}{$ENDIF} // Kylix legacy.

{$IFNDEF UNIX}

{$I ..\..\src\Zeos.inc}

{$ELSE}

{$I ../../src/Zeos.inc}

{$ENDIF}