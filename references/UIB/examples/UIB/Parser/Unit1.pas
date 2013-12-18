unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, uibSQLParser, uib;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
const


  NodeInfos: array[TSQLStatement] of string = (
    'Unknow',
    'AlterException',
    'AlterTable',
    'AlterTrigger',
    'AlterProcedure',
    'AlterDatabase',
    'AlterDomain',
    'AlterIndex',
    'ReadBlob',
    'InsertBlob',
    'Commit',
    'DeclareFilter',
    'DeclareFunction',
    'Delete',
    'DropException',
    'DropIndex',
    'DropProcedure',
    'DropTable',
    'DropTrigger',
    'DropView',
    'DropFilter',
    'DropDomain',
    'DropFunction',
    'DropShadow',
    'DropRole',
    'DropGenerator',
    'Grant',
    'InsertInto',
    'ExecuteProcedure',
    'RecreateProcedure',
    'RecreateTable',
    'RecreateView',
    'SetSqlDialect',
    'SetTransaction',
    'SetGenerator',
    'SetStatistics',
    'SetNames',
    'CreateException',
    'CreateIndex',
    'CreateProcedure',
    'CreateTable',
    'CreateTrigger',
    'CreateView',
    'CreateGenerator',
    'CreateDatabase',
    'CreateDomain',
    'CreateShadow',
    'CreateRole',
    'ReplaceProcedure',
    'ReplaceTrigger',
    'Revoke',
    'Rollback',
    'SetSavepoint',
    'ReleaseSavepoint',
    'UndoSavepoint',
    'Select',
    'Update',
    'Debug',
    'AutoDDL',
    'Connect',
    'EOF'
   );

procedure TForm1.Button1Click(Sender: TObject);
var
  parser: TUIBSQLParser;
  st: TSQLStatement;
begin
  Memo1.Clear;
  parser := TUIBSQLParser.Create(Memo2.Lines);
  try
    while true do
    begin
      st := Parser.NextStatement;
      if st = ssEOF then
        Break;
      memo1.Lines.Add('<--- ' + NodeInfos[st] + ' --->');
      memo1.Lines.Add(parser.Statement);
    end;
  finally
    parser.Free;
  end;
end;

end.

