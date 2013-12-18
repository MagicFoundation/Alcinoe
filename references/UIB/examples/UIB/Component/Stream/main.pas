unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls, QGrids,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls, Grids,
{$ENDIF}
  SysUtils, Classes, uib, uibLib;

type
  TMainForm = class(TForm)
    DataBase: TUIBDataBase;
    Transaction: TUIBTransaction;
    Query: TUIBQuery;
    Button1: TButton;
    StringGrid: TStringGrid;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var
  MemStream: TMemoryStream;
  FieldResult: TSQLResult;
  i, j: integer;
begin
  Query.Open;
  Query.FetchAll;
  MemStream := TMemoryStream.Create;
  FieldResult := TSQLResult.Create;
  Query.Fields.SaveToStream(MemStream);
  Query.Close(etmCommit);

  MemStream.Seek(0, soFromBeginning);
  FieldResult.LoadFromStream(MemStream);
  MemStream.Free;

  StringGrid.ColCount := FieldResult.FieldCount  + 1;
  StringGrid.RowCount := FieldResult.RecordCount + 1;

  for i := 1 to FieldResult.FieldCount do
    StringGrid.Cells[i, 0] := FieldResult.AliasName[i-1];

  for i := 1 to FieldResult.RecordCount do
  begin
    FieldResult.GetRecord(i-1);
    StringGrid.Cells[0, i] := Inttostr(i);
    for j := 1 to FieldResult.FieldCount do
      StringGrid.Cells[j, i] := FieldResult.AsString[j-1];
  end;

  FieldResult.Free;


end;

end.
