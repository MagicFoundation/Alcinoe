unit Unit2Static;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, mORMot, mORMotSQLite3, SynSQLite3Static,
  mORMotHttpServer, SynCrtSock,
  SampleData;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    Model: TSQLModel;
    DB: TSQLRestServerDB;
    Server: TSQLHttpServer;
  end;

  TCustomHttpServer = class(TSQLHttpServer)
  protected
    /// override the server response - must be thread-safe
    function Request(Ctxt: THttpServerRequest): cardinal; override;
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Model := CreateSampleModel;
  DB := TSQLRestServerDB.Create(Model,ChangeFileExt(ExeVersion.ProgramFileName,'.db3'),true);
  DB.CreateMissingTables;
  Server := TCustomHttpServer.Create('8080',[DB],'+',useHttpApiRegisteringURI,32,secNone,'static');
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


{ TCustomHttpServer }

function TCustomHttpServer.Request(Ctxt: THttpServerRequest): cardinal;
var FileName: TFileName;
begin
  if (Ctxt.Method='GET') and IdemPChar(pointer(Ctxt.URL),'/STATIC/') and
     (PosEx('..',Ctxt.URL)=0) then begin
    // http.sys will send the specified file from kernel mode
    FileName := ExeVersion.ProgramFilePath+'www\'+UTF8ToString(Copy(Ctxt.URL,8,maxInt));
    Ctxt.OutContent := StringToUTF8(FileName);
    Ctxt.OutContentType := HTTP_RESP_STATICFILE;
    result := 200; // THttpApiServer.Execute will return 404 if not found
  end else
    // call the associated TSQLRestServer instance(s)
    result := inherited Request(Ctxt);
end;


end.
