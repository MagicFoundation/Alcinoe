(*******************************************************************************
*  The TUIBQuery.QuickScript property is a way to execute more than one Update
*  SQL without trying to parse your script. If you uses the quickScript property
*  you must have only one query per line.
*******************************************************************************)

unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, uib;

type
  TMainForm = class(TForm)
    Query: TUIBQuery;
    DataBase: TUIBDataBase;
    Transaction: TUIBTransaction;
    btExecute: TButton;
    procedure btExecuteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btExecuteClick(Sender: TObject);
begin
  Query.SQL.Clear;
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test0'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test1'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test2'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test3'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test4'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test5'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test6'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test7'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test8'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test9'',''FFranc'')');

  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test0''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test1''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test2''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test3''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test4''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test5''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test6''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test7''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test8''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test9''');
  Query.ExecSQL;
  Query.Close(etmCommit);
end;

end.
