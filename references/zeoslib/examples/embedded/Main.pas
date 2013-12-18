unit Main;

interface

uses
{$IFDEF LINUX}
  Libc, QDBCtrls, QComCtrls, QStdCtrls, QExtCtrls, QControls, QGrids, QDBGrids,
  QForms,
{$ELSE}
  Windows, DBCtrls, ComCtrls, StdCtrls, ExtCtrls, Controls, Grids, DBGrids,
  Forms,
{$ENDIF}
  IniFiles, SysUtils, Classes, DB,
  ZConnection, ZAbstractRODataset, ZAbstractDataset, ZDataset;

type
  TfmMain = class(TForm)
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    ToolBar: TPanel;
    btOpen: TButton;
    btClose: TButton;
    meSQL: TMemo;
    StatusBar: TStatusBar;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    btApplyUpdates: TButton;
    btCancelUpdates: TButton;
    btExecute: TButton;
    EmbeddedConnection: TZConnection;
    ClientDataSet: TZQuery;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure meSQLExit(Sender: TObject);
    procedure btApplyUpdatesClick(Sender: TObject);
    procedure btCancelUpdatesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ShowState;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.ShowState;
var
  St:string;

  procedure AddSt(S:string);
  begin
    if St <> '' then
      St := St + ', ';
    St := St + S;
  end;

begin
  St := '';

  if ClientDataSet.Active then
    AddSt('Active')
  else
    AddSt('Inactive');

  StatusBar.Panels[0].Text := St;
end;

procedure TfmMain.meSQLExit(Sender: TObject);
begin
  if Trim(ClientDataSet.SQL.Text) <> Trim(meSQL.Lines.Text) then
    ClientDataSet.SQL.text := meSQL.Lines.Text;
  ShowState;
end;

procedure TfmMain.btOpenClick(Sender: TObject);
  procedure ModifyMyIniFile;
  var
    IniFile: TIniFile;
    FileName: string;
    FilePath: string;
  begin
  {$IFDEF LINUX}
    FileName := '/usr/local/var/my.cnf';
  {$ELSE}
    FileName := '.\my.ini';
  {$ENDIF}

    IniFile := TIniFile.Create(FileName);
    try
      FileName := ExtractFileName(ParamStr(0));
      FilePath := StringReplace(ExtractFilePath(ParamStr(0)), '\', '/', [rfReplaceAll]);

      CreateDir(FilePath + 'data');
      CreateDir(FilePath + 'data/Test');

      IniFile.WriteString(FileName, 'datadir', FilePath + 'data');
      IniFile.WriteString(FileName, 'basedir', FilePath);
    finally
      IniFile.Free;
    end;
  end;

begin
  try
    //ModifyMyIniFile;

    meSQLExit(nil);
    ClientDataSet.Open;
  finally
    ShowState;
  end;
end;

procedure TfmMain.btCloseClick(Sender: TObject);
begin
  ClientDataSet.Close;
  ShowState;
end;

procedure TfmMain.btExecuteClick(Sender: TObject);
begin
  try
    ClientDataSet.ExecSQL;
  finally
    ShowState;
  end;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  ClientDataSet.SQL.Text  := meSQL.Lines.Text;
  ShowState;
end;

procedure TfmMain.btApplyUpdatesClick(Sender: TObject);
begin
  ClientDataSet.ApplyUpdates;
end;

procedure TfmMain.btCancelUpdatesClick(Sender: TObject);
begin
  ClientDataSet.CancelUpdates;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
 EmbeddedConnection.Connected := true;
end;

end.

