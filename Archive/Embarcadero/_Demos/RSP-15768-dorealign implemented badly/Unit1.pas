unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm6 = class(TForm)
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Button1: TButton;
    procedure Rectangle2Resize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

var ButtonClicked: Boolean = False;

procedure TForm6.Button1Click(Sender: TObject);
begin
  ButtonClicked := True;
  Showmessage('we have Rectangle1 (yellow) with align = center and who have a child Rectangle2 (black) with align = client');
  Showmessage('now we resize Rectangle1 (yellow) to 300px height. We must also see it''s child Rectangle2 (black) resized to 300px height');
  rectangle1.Height := 300;
end;

procedure TForm6.Rectangle2Resize(Sender: TObject);
begin
  if not ButtonClicked then exit;
  Showmessage('but in the onresize of Rectangle2 (black) we decide to change the height of rectangle1 (yellow) to 400px instead');
  Showmessage('Normally we must also see the size of rectangle2 (black) resized to 400px (because align = alclient) but that will not happen');
  rectangle1.Height := 400;
end;

end.
