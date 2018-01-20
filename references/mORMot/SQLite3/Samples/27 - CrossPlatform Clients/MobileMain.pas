unit MobileMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo, FMX.StdCtrls, FMX.Edit,
  SynCrossPlatformJSON;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    edtValue: TEdit;
    lbl2: TLabel;
    mmoJSON: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure edtValueChangeTracking(Sender: TObject);
  private
  public
    doc: variant;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.edtValueChangeTracking(Sender: TObject);
begin
  doc.value := edtValue.Text;
  mmoJSON.Text := doc;
end;

procedure TForm1.FormCreate(Sender: TObject);
var json: string;
begin
  doc := JSONVariant('{"test":1234,"name":"Joh\"n\r","zero":0.0}');
  assert(doc.test=1234);
  assert(doc.name='Joh"n'#13);
  assert(doc.name2=null);
  assert(doc.zero=0);
  json := doc;
  assert(json='{"test":1234,"name":"Joh\"n\r","zero":0}');
  doc.name2 := 3.1415926;
  doc.name := 'John';
  json := doc;
  assert(json='{"test":1234,"name":"John","zero":0,"name2":3.1415926}');
end;

end.
