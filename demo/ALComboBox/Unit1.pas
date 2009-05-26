unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ExtCtrls, StdCtrls, ALComboBox;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    ALComboBox1: TALComboBox;
    Panel1: TPanel;
    Label5: TLabel;
    procedure ALComboBox1Paint(Sender: TObject; var continue: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses alfcnSkin;


procedure TForm1.ALComboBox1Paint(Sender: TObject; var continue: Boolean);
begin
  PaintAlComboBoxBlueSkin(Sender, Continue);
end;

end.
