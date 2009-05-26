unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ExtCtrls, StdCtrls, ALEdit;

type
  TForm1 = class(TForm)
    Search_imageList: TImageList;
    edt_FastSearch: TALEdit;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Panel1: TPanel;
    Label5: TLabel;
    procedure edt_FastSearchPaint(Sender: TObject; var continue: Boolean);
    procedure edt_FastSearchButtonClick(Sender: TObject);
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

procedure TForm1.edt_FastSearchPaint(Sender: TObject;
  var continue: Boolean);
begin
  PaintAlEditBlueSkin(Sender, Continue);
end;

procedure TForm1.edt_FastSearchButtonClick(Sender: TObject);
begin
  showmessage('Button Click');
end;

end.
