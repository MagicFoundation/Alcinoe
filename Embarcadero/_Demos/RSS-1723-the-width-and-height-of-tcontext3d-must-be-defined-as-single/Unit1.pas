unit Unit1;

interface

uses FMX.Forms, FMX.Objects, FMX.StdCtrls, FMX.Controls,
  FMX.Controls.Presentation, System.Classes, FMX.Types, Fmx.Graphics;

type
  TForm1 = class(TForm)
    Rectangle1: TRectangle;
    Button1: TButton;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PaintBox1: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


uses
  system.Types,
  system.SysUtils,
  FMX.Platform;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin

  var LScreenSrv: IFMXScreenService;
  var LScreenScale: Single;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then LScreenScale := LScreenSrv.GetScreenScale
  else LScreenScale := 1;

  Label1.Text := 'form Width = ' + FloatToStr(Width);
  Label2.Text := 'form height = ' + FloatToStr(Height);
  Label3.Text := 'form clientWidth = ' + FloatToStr(clientWidth);
  Label4.Text := 'form clientheight = ' + FloatToStr(clientHeight);
  Label7.Text := 'Rectangle Width = ' + FloatToStr(rectangle1.Width);
  Label8.Text := 'Rectangle height = ' + FloatToStr(rectangle1.Height);
  Label9.Text := 'ScreenScale = ' + FloatToStr(LScreenScale);

end;

procedure TForm1.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin

  canvas.fill.Color := $ffffffff;
  canvas.fill.Kind := TbrushKind.Solid;

  canvas.Stroke.Color := $ffffffff;
  canvas.Stroke.Kind := TbrushKind.Solid;
  canvas.Stroke.Thickness := 1;

  for var i := 0 to 20 do begin
    canvas.FillRect(TrectF.Create(TpointF.Create(i * 2,                 0),1,10), 0,0, [], 1);
    canvas.FillRect(TrectF.Create(TpointF.Create(PaintBox1.Width-1-(i*2), 0),1,10), 0,0, [], 1);
  end;

end;

end.
