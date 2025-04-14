program ALFmxDynamicListBoxDemo;

{$R 'Resources.res' 'Resources\Resources.rc'}

{$I Alcinoe.inc}

uses
  System.StartUpCopy,
  {$IF defined(SKIA)}
  FMX.Skia,
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  FMX.Context.Metal,
  {$ENDIF}
  FMX.Forms,
  FMX.Types,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  {$IF defined(SKIA)}
  GlobalUseSkia := True;
  GlobalUseVulkan := False;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  if TCustomContextMetal.IsMetalSupported then
    GlobalUseMetal := True;
  {$ENDIF}
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait];
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
