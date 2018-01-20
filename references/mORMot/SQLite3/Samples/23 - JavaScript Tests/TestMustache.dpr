/// test Mustache libraries: native SynMustache vs JavaScript/SpiderMonkey
program TestMustache;

{
  -------------------------------------------------------------------------
   Download the SpiderMonkey library at https://synopse.info/files/synsm.7z
   and put mozjs-24.dll and libnspr4.dll files with your TestMustache.exe
  -------------------------------------------------------------------------
}

// first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  TestMustacheUnit in 'TestMustacheUnit.pas' {MainForm};

{$ifndef FPC}
{$R *.res}
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
