unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Objects;

type
  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    Button1: TButton;
    Rectangle1: TRectangle;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Rectangle1.SetBounds(Rectangle1.Position.x, Rectangle1.Position.y, 100, 500);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Rectangle1.Size.Height := 600;
end;

end.
