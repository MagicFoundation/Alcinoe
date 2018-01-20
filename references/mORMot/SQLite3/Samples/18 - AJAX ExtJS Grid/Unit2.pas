unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, SynLog,
  mORMot, mORMotSQLite3, SynSQLite3Static, mORMotHttpServer; 

type
  TSQLSampleRecord = class(TSQLRecord)
  private
    fName: RawUTF8;
    fQuestion: RawUTF8;
    fTimeD: TDateTime;
  public
    /// overridden to populate a blank database with some data
    class procedure InitializeTable(Server: TSQLRestServer;
      const FieldName: RawUTF8; Options: TSQLInitializeTableOptions); override;
  published
    /// underscored names, as defined in our ExtJS scripts
    property TimeD: TDateTime read fTimeD write fTimeD;
    property Name: RawUTF8 read fName write fName;
    property Question: RawUTF8 read fQuestion write fQuestion;
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    btnQuit: TButton;
    Label2: TLabel;
    btnShowLogs: TButton;
    procedure btnQuitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnShowLogsClick(Sender: TObject);
  private
  public
    Model: TSQLModel;
    DB: TSQLRestServerDB;
    Server: TSQLHttpServer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


{ TForm1 }

procedure TForm1.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Model := TSQLModel.Create([TSQLSampleRecord]);
  DB := TSQLRestServerDB.Create(Model,ChangeFileExt(paramstr(0),'.db3'));
  // customize RESTful URI parameters as expected by our ExtJS client 
  DB.URIPagingParameters.StartIndex := 'START=';
  DB.URIPagingParameters.Results := 'LIMIT=';
  DB.URIPagingParameters.SendTotalRowsCountFmt := ',"total":%';
  // initialize and launch the server
  DB.CreateMissingTables;
  Server := TSQLHttpServer.Create('8080',[DB],'+',useHttpApiRegisteringURI);
  Server.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Free;
  DB.Free;
  Model.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Label1.Caption := Caption;
end;


{ TSQLSampleRecord }

class procedure TSQLSampleRecord.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
var Rec: TSQLSampleRecord;
begin
  inherited;
  if FieldName<>'' then
    exit; // create database only if void
  Rec := TSQLSampleRecord.CreateAndFillPrepare(
    StringFromFile(ExtractFilePath(paramstr(0))+'SampleRecordInit.json'));
  try
    while Rec.FillOne do
      Server.Add(Rec,true);
  finally
    Rec.Free;
  end;
end;

procedure TForm1.btnShowLogsClick(Sender: TObject);
begin
  AllocConsole;
  TextColor(ccLightGray); // to force the console to be recognized
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE; // log all events to the console
  end;
  btnShowLogs.Hide;
end;

end.
