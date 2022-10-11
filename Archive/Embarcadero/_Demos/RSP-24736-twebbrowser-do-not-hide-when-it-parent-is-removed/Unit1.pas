unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, unit2, FMX.Objects;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Text1: TText;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fFrame2: TFrame2;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if fFrame2 = nil then begin
    fFrame2 := TFrame2.Create(self);
    fFrame2.Parent := self;
    fFrame2.Align := TalignLayout.Client;
    fFrame2.WebBrowser1.Navigate('https://www.wikipedia.org/');
    Button1.text := 'Remove the Frame with WebBrowser';
  end
  else begin
    fFrame2.parent := nil;
    fFrame2.Free;
    fFrame2 := Nil;
    Button1.text := 'Show the Frame with WebBrowser';
  end;
end;

end.
