unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects;

type
  TForm1 = class(TForm)
    Arc1: TArc;
    FloatAnimation1: TFloatAnimation;
    Arc2: TArc;
    FloatAnimation4: TFloatAnimation;
    Arc3: TArc;
    FloatAnimation5: TFloatAnimation;
    Arc6: TArc;
    FloatAnimation8: TFloatAnimation;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

initialization
  GlobalUseGPUCanvas := true;

end.
