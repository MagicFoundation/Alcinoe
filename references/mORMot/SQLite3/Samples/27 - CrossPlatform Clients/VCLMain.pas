unit VCLMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCrossPlatformJSON, SynCrossPlatformREST;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    edtValue: TEdit;
    lbl2: TLabel;
    mmoJSON: TMemo;
    grpTable: TGroupBox;
    btnTableRewind: TButton;
    btnTableNext: TButton;
    grpORM: TGroupBox;
    btnORMFirst: TButton;
    btnORMNext: TButton;
    procedure FormCreate(Sender: TObject);
    procedure edtValueChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTableNextClick(Sender: TObject);
    procedure ORMClick(Sender: TObject);
  private
  public
    doc: variant;
    table: TJSONTableObject;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var FN: TFileName;
    level: integer;
begin
  doc := JSONVariant('{"test":1234,"name":"Joh\"n\r","zero":0.0}');
  FN := 'people.json';
  for level := 1 to 4 do
    if FileExists(FN) then
      break else
      FN := '..\'+FN;
  table := TJSONTableObject.Create(UTF8FileToString(FN));
  assert(length(table.FieldNames)=6);
end;

procedure TForm1.edtValueChange(Sender: TObject);
begin
  doc.value := edtValue.Text;
  mmoJSON.Text := doc;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  table.Free;
end;

procedure TForm1.btnTableNextClick(Sender: TObject);
begin
  if table.Step(Sender=btnTableRewind) then
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

procedure TForm1.ORMClick(Sender: TObject);
var people: TSQLRecordPeople;
begin
  people := TSQLRecordPeople.Create;
  try
    if table.StepObject(people,Sender=btnORMFirst) then
      mmoJSON.Text := ObjectToJSON(people) else
      mmoJSON.Text := 'null';
  finally
    people.Free;
  end;
end;

end.
