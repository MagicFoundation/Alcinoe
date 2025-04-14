unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Alcinoe.FMX.StdCtrls,
  FMX.Effects, FMX.Filter.Effects, FMX.Objects, FMX.Layouts, Alcinoe.FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, Alcinoe.FMX.FilterEffects, system.messaging,
  Alcinoe.FMX.Controls, Alcinoe.FMX.Objects;

type
  TForm2 = class(TForm)
    Image1: TImage;
    ALTrackBarContrast: TALTrackBar;
    ALTrackBarSaturation: TALTrackBar;
    ALLayout1: TALLayout;
    ButtonActivateDeactivate: TALButton;
    ALTrackBarVibrance: TALTrackBar;
    ALTrackBarWhites: TALTrackBar;
    ALTrackBarBlacks: TALTrackBar;
    ALVertScrollBox1: TALVertScrollBox;
    ALTrackBarTemperature: TALTrackBar;
    ALTrackBarTint: TALTrackBar;
    ALTrackBarExposure: TALTrackBar;
    ALTrackBarGamma: TALTrackBar;
    Label3: TALText;
    Label6: TALText;
    Label7: TALText;
    Label8: TALText;
    Label9: TALText;
    Label10: TALText;
    Label13: TALText;
    Label11: TALText;
    Label12: TALText;
    ALColorAdjustEffect1: TALColorAdjustEffect;
    procedure ALTrackBarContrastChange(Sender: TObject);
    procedure ALTrackBarSaturationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonActivateDeactivateClick(Sender: TObject);
    procedure ALTrackBarVibranceChange(Sender: TObject);
    procedure ALTrackBarBlacksChange(Sender: TObject);
    procedure ALTrackBarWhitesChange(Sender: TObject);
    procedure ALTrackBarTintChange(Sender: TObject);
    procedure ALTrackBarTemperatureChange(Sender: TObject);
    procedure ALTrackBarGammaChange(Sender: TObject);
    procedure ALTrackBarExposureChange(Sender: TObject);
    procedure ALLayout1Resized(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.Common;

{*********************************************************}
procedure TForm2.ALTrackBarContrastChange(Sender: TObject);
begin
  ALColorAdjustEffect1.contrast := ALTrackBarcontrast.Value;
end;

{***********************************************************}
procedure TForm2.ALTrackBarSaturationChange(Sender: TObject);
begin
  ALColorAdjustEffect1.Saturation := ALTrackBarSaturation.Value;
end;

{*********************************************************}
procedure TForm2.ALTrackBarVibranceChange(Sender: TObject);
begin
  ALColorAdjustEffect1.Vibrance := ALTrackBarVibrance.Value;
end;

{*******************************************************}
procedure TForm2.ALTrackBarWhitesChange(Sender: TObject);
begin
  ALColorAdjustEffect1.Whites := ALTrackBarWhites.Value;
end;

{*******************************************************}
procedure TForm2.ALTrackBarBlacksChange(Sender: TObject);
begin
  ALColorAdjustEffect1.Blacks := ALTrackBarBlacks.Value;
end;

{************************************************************}
procedure TForm2.ALTrackBarTemperatureChange(Sender: TObject);
begin
  ALColorAdjustEffect1.temperature := ALTrackBartemperature.Value;
end;

{*****************************************************}
procedure TForm2.ALTrackBarTintChange(Sender: TObject);
begin
  ALColorAdjustEffect1.tint := ALTrackBartint.Value;
end;

{*********************************************************}
procedure TForm2.ALTrackBarExposureChange(Sender: TObject);
begin
  ALColorAdjustEffect1.Exposure := ALTrackBarExposure.Value;
end;

{******************************************************}
procedure TForm2.ALTrackBarGammaChange(Sender: TObject);
begin
  ALColorAdjustEffect1.gamma := ALTrackBargamma.Value;
end;

{**************************************************************}
procedure TForm2.ButtonActivateDeactivateClick(Sender: TObject);
begin
  ALColorAdjustEffect1.Enabled := not ALColorAdjustEffect1.Enabled;
  if ALColorAdjustEffect1.Enabled then ButtonActivateDeactivate.Text := 'Deactivate'
  else ButtonActivateDeactivate.Text := 'Activate';
end;

{*************************************************}
procedure TForm2.ALLayout1Resized(Sender: TObject);
begin
  ALLayout1.Height := ALLayout1.Width / (720 / 404)
end;

{*******************************************}
procedure TForm2.FormCreate(Sender: TObject);
begin
  TALErrorReporting.Instance;
  ALColorAdjustEffect1.Enabled := true;
  ALLayout1.Height := ALLayout1.Width / (720 / 404)
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
