unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TForm3 = class(TForm)
    Rectangle1: TRectangle;
    Text1: TText;
    Text2: TText;
    procedure Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Rectangle1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Rectangle1MouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Text1.Text := 'MouseDown';
end;

procedure TForm3.Rectangle1MouseLeave(Sender: TObject);
begin
  Text1.Text := 'MouseLeave';
end;

procedure TForm3.Rectangle1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Text1.Text := 'MouseUp';
end;

end.
