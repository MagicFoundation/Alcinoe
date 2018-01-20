unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellAPI,
  SynCommons, SynLog, SynZip, SynCrtSock,
  mORMot, mORMotSQLite3, SynSQLite3Static, mORMotHttpServer;

type
  TSQLBiolife = class(TSQLRecord)
  public
    fSpecies_No: integer;
    fCategory: RawUTF8;
    fCommon_Name: RawUTF8;
    fSpecies_Name: RawUTF8;
    fLength_cm: double;
    fLength_in: double;
    fNotes: RawUTF8;
    fGraphic: TSQLRawBlob;
    fSom: TSQLRawBlob;
  published
    property Species_No: integer read fSpecies_No write fSpecies_No;
    property Category: RawUTF8 read fCategory write fCategory;
    property Common_Name: RawUTF8 read fCommon_Name write fCommon_Name;
    property Species_Name: RawUTF8 read fSpecies_Name write fSpecies_Name;
    property Length_cm: double read fLength_cm write fLength_cm;
    property Length_in: double read fLength_in write fLength_in;
    property Notes: RawUTF8 read fNotes write fNotes;
    property Graphic: TSQLRawBlob read fGraphic write fGraphic;
    property Som: TSQLRawBlob read fSom write fSom;
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    btnQuit: TButton;
    Label2: TLabel;
    btnShowLogs: TButton;
    btnOpenBrowser: TButton;
    procedure btnQuitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnShowLogsClick(Sender: TObject);
    procedure btnOpenBrowserClick(Sender: TObject);
  private
    fDatabaseFileName: TFileName;
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
var download: RawByteString;
begin
  Model := TSQLModel.Create([TSQLBiolife]);
  fDatabaseFileName := ChangeFileExt(paramstr(0),'.db3');
  if not FileExists(fDatabaseFileName) then begin
    download := TWinINet.Get('https://synopse.info/files/samples/Project19Server.zip');
    if download<>'' then
      with TZipRead.Create(pointer(download),length(download)) do
      try
        UnZip(ExtractFileName(fDatabaseFileName),ExtractFilePath(fDatabaseFileName));
      finally
        Free;
      end;
    if not FileExists(fDatabaseFileName) then begin
      ShowMessage('Impossible to find '+fDatabaseFileName+
        #13#13'Please download it from https://synopse.info/files/samples/Project19Server.zip');
      exit;
    end;
  end;
  DB := TSQLRestServerDB.Create(Model,fDatabaseFileName);
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
  if DB=nil then
    Close;
  Label1.Caption := Caption;
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

procedure TForm1.btnOpenBrowserClick(Sender: TObject);
begin
  ShellExecute(0,'open',pointer(ExtractFilePath(ParamStr(0))+'html5\index.html'),
    nil,nil,SW_SHOWNORMAL);
end;

end.
