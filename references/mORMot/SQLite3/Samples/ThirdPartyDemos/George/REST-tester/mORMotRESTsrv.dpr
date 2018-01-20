program mORMotRESTsrv;

{$ifdef Linux}
  {$ifdef FPC_CROSSCOMPILING}
    {$ifdef CPUARM}
    {$linklib GLESv2}
    {$endif}
    {$linklib libc_nonshared.a}
  {$endif}
{$endif}

{$I Synopse.inc}

uses
  {$IFNDEF FPC}
  {$ELSE}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  {$ENDIF }
  Forms,
  {$ifndef DELPHI5OROLDER}
  mORMotRESTFPCInterfaces,
  {$endif}
  {$ifdef COMPUTEFPCINTERFACES}
  SynCommons,
  mORMotWrappers,
  {$endif}
  RestServerFormUnit in 'RestServerFormUnit.pas' {Form1},
  RestServerUnit in 'RestServerUnit.pas',
  RestServerMethodsUnit in 'RestServerMethodsUnit.pas',
  RestMethodsInterfaceUnit in 'RestMethodsInterfaceUnit.pas'
  {$ifndef FPC}
  ,Vcl.Themes
  ,Vcl.Styles
  {$endif}
  ;

{$ifndef FPC}
{$R *.res}
{$endif}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
  {$ifdef MSWINDOWS}
  Application.MainFormOnTaskbar := True;
  {$endif}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  {$ifdef COMPUTEFPCINTERFACES}
  ChDir(ExeVersion.ProgramFilePath);
  ComputeFPCInterfacesUnit(
    ['..\..\..\..\..\..\CrossPlatform\templates'],
     '..\..\mORMotRESTFPCInterfaces.pas');
  {$endif}
end.
