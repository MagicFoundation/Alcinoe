unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Effects, FMX.Filter.Effects,
  FMX.Objects, fmx.Filter, fmx.Types3D;

type
  TForm1 = class(TForm)
    WaveEffect1: TWaveEffect;
    Button1: TButton;
    Image1: TImage;
    PaintBox1: TPaintBox;
    Text1: TText;
    procedure Button1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  private
    { Private declarations }
    FTexture: TTexture;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Fmx.Canvas.GPU;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FTexture := TContextManager.DefaultContextClass.BitmapToTexture(Image1.Bitmap);
  WaveEffect1.ProcessTexture(FTexture, nil);
  FTexture := TFilterManager.FilterTexture;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  if FTexture = nil then exit;
  TCustomCanvasGpu(Canvas).DrawTexture(PaintBox1.LocalRect, // ATexRect
                                       TRectF.Create(0,
                                                     0,
                                                     FTexture.Width,
                                                     FTexture.Height), // ARect
                                       $ffffffff, // https://quality.embarcadero.com/browse/RSP-15432
                                       FTexture);
end;


end.
