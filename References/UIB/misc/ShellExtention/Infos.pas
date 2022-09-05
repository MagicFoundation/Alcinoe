unit Infos;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uib, Grids, ValEdit, StdCtrls;

type
  TInfosForm = class(TForm)
    Database: TUIBDataBase;
    ValueList: TValueListEditor;
    UserList: TListBox;
    Label1: TLabel;
    Config: TUIBConfig;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DatabaseInfoUserNames(Sender: TObject; Value: String);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
    procedure Refresh;
  public
    { Déclarations publiques }
  end;

var
  InfosForm: TInfosForm;

implementation
uses inifiles;

{$R *.dfm}

procedure TInfosForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TInfosForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
  try
    Database.LibraryName := ini.ReadString('DATABASE', 'LIBRARY', 'gds32.dll');
    Database.UserName := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    Database.PassWord := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');

    Config.LibraryName := Database.LibraryName;
    Config.UserName := Database.UserName;
    Config.PassWord := Database.PassWord;
  finally
    ini.Free;
  end;
end;

procedure TInfosForm.FormShow(Sender: TObject);
begin
  Refresh;
end;

procedure TInfosForm.DatabaseInfoUserNames(Sender: TObject; Value: String);
begin
  UserList.Items.Add(Value);
end;

procedure TInfosForm.Refresh;
begin
  Database.Connected := true;
  try
     ValueList.Strings.BeginUpdate;
     try
       ValueList.Strings.Clear;
       ValueList.Strings.Add(format('Page size=%d', [Database.InfoPageSize]));
       ValueList.Strings.Add(format('SQL Dialect=%d', [Database.InfoDbSqlDialect]));
       ValueList.Strings.Add(format('Sweep Interval=%d', [Database.InfoSweepInterval]));
       ValueList.Strings.Add(format('ODS Version=%d.%d', [Database.InfoOdsVersion, Database.InfoOdsMinorVersion]));
       ValueList.Strings.Add(format('DB Engine=%s', [Database.InfoVersion]));
     finally
       ValueList.Strings.EndUpdate;
     end;
     UserList.Clear;
     Database.InfoUserNames;
  finally
    Database.Connected := false;
  end;
end;

procedure TInfosForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then Close;
end;

end.
