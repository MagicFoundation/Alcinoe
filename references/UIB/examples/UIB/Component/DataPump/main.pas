unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uib, StdCtrls;

type
  TMainForm = class(TForm)
    DataBase: TUIBDataBase;
    Transaction: TUIBTransaction;
    Query: TUIBQuery;
    go: TButton;
    procedure goClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TARecord = record
    COUNTRY: string;
    CURRENCY: string;
  end;



var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.goClick(Sender: TObject);
var
  i: Integer;
const
  Datas : array[1..10] of TARecord = (
    (COUNTRY: 'blabla0'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla1'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla2'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla3'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla4'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla5'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla6'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla7'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla8'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla9'; CURRENCY: 'blabla'));
begin
  for i := 1 to 10 do
  begin
    Query.Params.AsString[0] := Datas[i].COUNTRY;
    Query.Params.AsString[1] := Datas[i].CURRENCY;
    Query.Execute;
    // for better performance commit every 1000 records
    // Transaction.Commit;
  end;
  Query.Close(etmRollback); // change to etmCommit to apply.
end;

end.
