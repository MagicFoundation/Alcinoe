unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ALFmxStdCtrls,
  FMX.Effects, FMX.Filter.Effects, FMX.Objects, FMX.Layouts, ALFmxLayouts,
  FMX.Controls.Presentation, FMX.StdCtrls, ALFmxFilterEffects, system.messaging;

type
  TForm2 = class(TForm)
    Image1: TImage;
    ALTrackBar2: TALTrackBar;
    ALTrackBar3: TALTrackBar;
    ALTrackBar4: TALTrackBar;
    ALLayout1: TALLayout;
    Button1: TButton;
    ALTrackBar5: TALTrackBar;
    ALTrackBar6: TALTrackBar;
    ALTrackBar7: TALTrackBar;
    ALVertScrollBox1: TALVertScrollBox;
    ALTrackBar8: TALTrackBar;
    ALTrackBar9: TALTrackBar;
    ALTrackBar10: TALTrackBar;
    ALTrackBar11: TALTrackBar;
    ALTrackBar12: TALTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    tint: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label1: TLabel;
    procedure ALTrackBar2Tracking(Sender: TObject);
    procedure ALTrackBar3Tracking(Sender: TObject);
    procedure ALTrackBar4Tracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ALTrackBar5Tracking(Sender: TObject);
    procedure ALTrackBar7Tracking(Sender: TObject);
    procedure ALTrackBar6Tracking(Sender: TObject);
    procedure ALTrackBar9Tracking(Sender: TObject);
    procedure ALTrackBar8Tracking(Sender: TObject);
    procedure ALTrackBar11Tracking(Sender: TObject);
    procedure ALTrackBar10Tracking(Sender: TObject);
    procedure ALTrackBar12Tracking(Sender: TObject);
    procedure ALLayout1Resize(Sender: TObject);
  private
    fColorAdjustEffect: TALColorAdjustEffect;
    {$IF Defined(IOS) or Defined(ANDROID)}
    procedure ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$IF Defined(MSWINDOWS) or Defined(_MACOS)}
    procedure ApplicationExceptionHandler(Sender: TObject; E: Exception);
    {$ENDIF}
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses Grijjy.ErrorReporting,
     alcommon;

procedure TForm2.ALTrackBar2Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar2.Value);
  fColorAdjustEffect.contrast := ALTrackBar2.Value;
end;

procedure TForm2.ALTrackBar3Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar3.Value);
  fColorAdjustEffect.Highlights := ALTrackBar3.Value;
end;

procedure TForm2.ALTrackBar4Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar4.Value);
  fColorAdjustEffect.Saturation := ALTrackBar4.Value;
end;

procedure TForm2.ALTrackBar5Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar5.Value);
  fColorAdjustEffect.Vibrance := ALTrackBar5.Value;
end;

procedure TForm2.ALTrackBar6Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar6.Value);
  fColorAdjustEffect.Whites := ALTrackBar6.Value;
end;

procedure TForm2.ALTrackBar7Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar7.Value);
  fColorAdjustEffect.Blacks := ALTrackBar7.Value;
end;

procedure TForm2.ALTrackBar8Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar8.Value);
  fColorAdjustEffect.temperature := ALTrackBar8.Value;
end;

procedure TForm2.ALTrackBar9Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar9.Value);
  fColorAdjustEffect.tint := ALTrackBar9.Value;
end;

procedure TForm2.ALLayout1Resize(Sender: TObject);
begin
  ALLayout1.Height := ALLayout1.Width / (720 / 404)
end;

procedure TForm2.ALTrackBar10Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar10.Value);
  fColorAdjustEffect.Exposure := ALTrackBar10.Value;
end;

procedure TForm2.ALTrackBar11Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar11.Value);
  fColorAdjustEffect.gamma := ALTrackBar11.Value;
end;

procedure TForm2.ALTrackBar12Tracking(Sender: TObject);
begin
  Label1.Text := FloatToStr(ALTrackBar12.Value);
  fColorAdjustEffect.Shadows := ALTrackBar12.Value;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  fColorAdjustEffect.Enabled := not fColorAdjustEffect.Enabled;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  fColorAdjustEffect := TALColorAdjustEffect.Create(Image1);
  fColorAdjustEffect.Parent := Image1;
  fColorAdjustEffect.Enabled := true;
  {$IF Defined(IOS) or Defined(ANDROID)}
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ELSE}
  Application.OnException := ApplicationExceptionHandler;
  {$ENDIF}
  ALLayout1.Height := ALLayout1.Width / (720 / 404)
end;

{$IF Defined(IOS) or Defined(ANDROID)}
procedure TForm2.ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
var aReport: IgoExceptionReport;
begin

  aReport := TgoExceptionReportMessage(M).Report;
  allog('ERROR', aReport.Report, TalLogType.error);

  {$IF Defined(IOS)}
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        Halt(1); // << This is the only way i found to crash the app :(
      end);
    end).Start;
  {$ELSE}
  Application.Terminate;
  {$ENDIF}

end;
{$ENDIF}

{$IF Defined(MSWINDOWS) or Defined(_MACOS)}
procedure TForm2.ApplicationExceptionHandler(Sender: TObject; E: Exception);
begin
  Application.Terminate;
end;
{$ENDIF}

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
