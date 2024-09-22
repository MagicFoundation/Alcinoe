program ALFmxControls;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  UnitMain in 'UnitMain.pas' {MainForm},
  UnitScrollBoxDemo in 'UnitScrollBoxDemo.pas' {ScrollBoxDemoForm};

{$R *.res}

begin
  {$IF defined(SKIA)}
  GlobalUseSkia := True;
  GlobalUseVulkan := False;
  {$ELSE}
  GlobalUseSkia := FALSE;
  GlobalUseVulkan := False;
  {$ENDIF}
  //GlobalUseGPUCanvas := True;
  GlobalUseMetal := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

