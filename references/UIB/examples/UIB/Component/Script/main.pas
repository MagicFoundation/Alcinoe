unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uib, StdCtrls, uibSQLParser, ComCtrls;

type
  TMainForm = class(TForm)
    Script: TUIBScript;
    DataBase: TUIBDataBase;
    Transaction: TUIBTransaction;
    Button1: TButton;
    Edit: TEdit;
    StatusBar: TStatusBar;
    Label1: TLabel;
    Label2: TLabel;
    Log: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure ScriptParse(Sender: TObject; NodeType: TSQLStatement;
      const Statement: String);
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
var c: cardinal;
begin
  if FileExists('d:\database.db') then
    DeleteFile('d:\database.db');
  c := GetTickCount;
  Script.ExecuteScript;
  Transaction.Commit;
  Edit.text := inttostr(GetTickCount - c) + ' ms';
  DataBase.Connected := False;
end;



procedure TMainForm.ScriptParse(Sender: TObject; NodeType: TSQLStatement;
  const Statement: String);
begin
  Log.Text := Statement;
  Application.ProcessMessages;
end;

end.
