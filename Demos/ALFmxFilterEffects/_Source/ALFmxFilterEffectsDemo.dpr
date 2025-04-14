program ALFmxFilterEffectsDemo;

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
  Unit1 in 'Unit1.pas' {Form2};

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
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
