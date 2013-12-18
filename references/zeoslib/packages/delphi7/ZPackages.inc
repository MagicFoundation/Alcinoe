{$IFDEF FREEBSD}
  {$DEFINE UNIX}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
{$ENDIF}

{$IFNDEF UNIX}
{$I ..\..\src\Zeos.inc}
{$ELSE}
{$I ../../src/Zeos.inc}
{$ENDIF}
