unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
   SysUtils, Classes, uib;

type
  TForm1 = class(TForm)
    DataBase: TUIBDataBase;
    Button1: TButton;
    Transaction: TUIBTransaction;
    Query: TUIBQuery;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Query.Params.ByNameAsInteger['Dept'] := 623;
  Query.Open;
  memo.Clear;
  while not Query.EOF do
    with Query, Fields do
    begin
      memo.Lines.Add(format('%s %s, Salary: %f',
        [ByNameAsString['FIRST_NAME'],
         ByNameAsString['LAST_NAME'],
         ByNameAsCurrency['SALARY']]));
      Next;
    end;
  memo.Lines.Add(Query.Plan);  
  Query.Close(etmCommit);

end;

end.
