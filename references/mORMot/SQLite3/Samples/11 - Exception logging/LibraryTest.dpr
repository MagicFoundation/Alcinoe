program LibraryTest;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils;

procedure Test; external 'MyLibrary.dll';

begin
  Test;
end.
