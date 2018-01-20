unit FMMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Edit,
  SynCrossPlatformJSON;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    edtValue: TEdit;
    lbl2: TLabel;
    lbl3: TLabel;
    mmoJSON: TMemo;
    grpTable: TGroupBox;
    btnRewind: TButton;
    btnNext: TButton;
    grpORM: TGroupBox;
    btnRewindORM: TButton;
    btnNextORM: TButton;
    procedure edtValueChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnORMClick(Sender: TObject);
  private
  public
    doc: variant;
    table: TJSONTableObject;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnNextClick(Sender: TObject);
begin
  if table.Step(Sender=btnRewind) then
    mmoJSON.Text := JSONVariant(table.RowValues) else
    mmoJSON.Text := 'null';
end;

type
  TSQLRecordPeople = class(TPersistent)
  private
    fRowID: integer;
    fData: TByteDynArray;
    fFirstName: string;
    fLastName: string;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property RowID: integer read fRowID write fRowID;
    property FirstName: string read fFirstName write fFirstName;
    property LastName: string read fLastName write fLastName;
    property Data: TByteDynArray read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;

procedure TForm1.btnORMClick(Sender: TObject);
var people: TSQLRecordPeople;
begin
  people := TSQLRecordPeople.Create;
  try
    if table.StepObject(people,Sender=btnRewindORM) then
      mmoJSON.Text := ObjectToJSON(people) else
      mmoJSON.Text := 'null';
  finally
    people.Free;
  end;
end;

procedure TForm1.edtValueChange(Sender: TObject);
begin
  doc.value := edtValue.Text;
  mmoJSON.Text := doc;
end;

procedure TForm1.FormCreate(Sender: TObject);
var json: string;
    FN: TFileName;
    level: integer;
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
  FN := 'people.json';
  for level := 1 to 4 do
    if FileExists(FN) then
      break else
      FN := IncludeTrailingPathDelimiter('..')+FN;
  table := TJSONTableObject.Create(UTF8FileToString(FN));
  assert(length(table.FieldNames)=6);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  table.Free;
end;

end.
