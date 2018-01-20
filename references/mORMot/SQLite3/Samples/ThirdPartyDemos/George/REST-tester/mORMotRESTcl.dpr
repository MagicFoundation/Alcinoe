program mORMotRESTcl;

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
  {$IFDEF FPC}
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
  RestClientFormUnit in 'RestClientFormUnit.pas' {Form1} ,
  RestMethodsInterfaceUnit in 'RestMethodsInterfaceUnit.pas',
  RestClientUnit in 'RestClientUnit.pas'
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
  {$ifndef FPC}
  TStyleManager.TrySetStyle('Turquoise Gray');
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
