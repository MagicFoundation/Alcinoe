program SynTest;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SynTestTest;

begin
  with TTestSuit.Create do
  try
    Run;
    readln;
  finally
    Free;
  end;
end.
