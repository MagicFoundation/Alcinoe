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
    ALTrackBar2: TALTrackBar;
    ALTrackBar3: TALTrackBar;
    ALTrackBar4: TALTrackBar;
    ALLayout1: TALLayout;
    Button1: TALButton;
    ALTrackBar5: TALTrackBar;
    ALTrackBar6: TALTrackBar;
    ALTrackBar7: TALTrackBar;
    ALVertScrollBox1: TALVertScrollBox;
    ALTrackBar8: TALTrackBar;
    ALTrackBar9: TALTrackBar;
    ALTrackBar10: TALTrackBar;
    ALTrackBar11: TALTrackBar;
    ALTrackBar12: TALTrackBar;
    Label3: TALText;
    Label4: TALText;
    Label5: TALText;
    Label6: TALText;
    Label7: TALText;
    Label8: TALText;
    Label9: TALText;
    Label10: TALText;
    tint: TALText;
    Label11: TALText;
    Label12: TALText;
    procedure ALTrackBar2Change(Sender: TObject);
    procedure ALTrackBar3Change(Sender: TObject);
    procedure ALTrackBar4Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ALTrackBar5Change(Sender: TObject);
    procedure ALTrackBar7Change(Sender: TObject);
    procedure ALTrackBar6Change(Sender: TObject);
    procedure ALTrackBar9Change(Sender: TObject);
    procedure ALTrackBar8Change(Sender: TObject);
    procedure ALTrackBar11Change(Sender: TObject);
    procedure ALTrackBar10Change(Sender: TObject);
    procedure ALTrackBar12Change(Sender: TObject);
    procedure ALLayout1Resized(Sender: TObject);
  private
    fColorAdjustEffect: TALColorAdjustEffect;
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

{**************************************************}
procedure TForm2.ALTrackBar2Change(Sender: TObject);
begin
  fColorAdjustEffect.contrast := ALTrackBar2.Value;
end;

{**************************************************}
procedure TForm2.ALTrackBar3Change(Sender: TObject);
begin
  fColorAdjustEffect.Highlights := ALTrackBar3.Value;
end;

{**************************************************}
procedure TForm2.ALTrackBar4Change(Sender: TObject);
begin
  fColorAdjustEffect.Saturation := ALTrackBar4.Value;
end;

{**************************************************}
procedure TForm2.ALTrackBar5Change(Sender: TObject);
begin
  fColorAdjustEffect.Vibrance := ALTrackBar5.Value;
end;

{**************************************************}
procedure TForm2.ALTrackBar6Change(Sender: TObject);
begin
  fColorAdjustEffect.Whites := ALTrackBar6.Value;
end;

{**************************************************}
procedure TForm2.ALTrackBar7Change(Sender: TObject);
begin
  fColorAdjustEffect.Blacks := ALTrackBar7.Value;
end;

{**************************************************}
procedure TForm2.ALTrackBar8Change(Sender: TObject);
begin
  fColorAdjustEffect.temperature := ALTrackBar8.Value;
end;

{**************************************************}
procedure TForm2.ALTrackBar9Change(Sender: TObject);
begin
  fColorAdjustEffect.tint := ALTrackBar9.Value;
end;

{*************************************************}
procedure TForm2.ALLayout1Resized(Sender: TObject);
begin
  ALLayout1.Height := ALLayout1.Width / (720 / 404)
end;

{***************************************************}
procedure TForm2.ALTrackBar10Change(Sender: TObject);
begin
  fColorAdjustEffect.Exposure := ALTrackBar10.Value;
end;

{***************************************************}
procedure TForm2.ALTrackBar11Change(Sender: TObject);
begin
  fColorAdjustEffect.gamma := ALTrackBar11.Value;
end;

{***************************************************}
procedure TForm2.ALTrackBar12Change(Sender: TObject);
begin
  fColorAdjustEffect.Shadows := ALTrackBar12.Value;
end;

{*********************************************}
procedure TForm2.Button1Click(Sender: TObject);
begin
  fColorAdjustEffect.Enabled := not fColorAdjustEffect.Enabled;
end;

{*******************************************}
procedure TForm2.FormCreate(Sender: TObject);
begin
  TALErrorReporting.Instance;
  fColorAdjustEffect := TALColorAdjustEffect.Create(Image1);
  fColorAdjustEffect.Parent := Image1;
  fColorAdjustEffect.Enabled := true;
  ALLayout1.Height := ALLayout1.Width / (720 / 404)
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
