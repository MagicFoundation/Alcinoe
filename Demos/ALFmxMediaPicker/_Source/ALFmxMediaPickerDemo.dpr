program ALFmxMediaPickerDemo;

{$I Alcinoe.inc}

{$R 'AlcinoeResources.res' '..\..\..\Source\Resources\AlcinoeResources.rc'}
{$R 'Resources.res' 'Resources\Resources.rc'}

uses
  System.StartUpCopy,
  {$IF defined(SKIA)}
  FMX.Skia,
  {$ENDIF }
  {$IF defined(ALAppleOS)}
  FMX.Context.Metal,
  {$ENDIF }
  FMX.Forms,
  FMX.Types,
  Unit1 in 'Unit1.pas' {Form1};

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
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.