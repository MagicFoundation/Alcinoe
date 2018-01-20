/// test JavaScript execution using the SpiderMonkey library
program TestSynSM;

{
  -------------------------------------------------------------------------
   Download the SpiderMonkey library at https://synopse.info/files/synsm.7z
   and put mozjs-24.dll and libnspr4.dll files with your TestSynSM.exe
  -------------------------------------------------------------------------
}

{$APPTYPE CONSOLE}

{$ifdef WIN64}
begin
  writeln('SpiderMonkey is not handled in 64 bit mode yet');
  
{$else}
// first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  SynSMSelfTest in 'SynSMSelfTest.pas';

begin
  SynSMConsoleTests;
{$endif WIN64}
end.


