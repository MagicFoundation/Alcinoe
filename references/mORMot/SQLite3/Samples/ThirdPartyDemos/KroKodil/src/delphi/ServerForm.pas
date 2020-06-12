unit ServerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynLog, SynCommons, mORMot, mORMotSQLite3, SynSQLite3Static,
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
  private
    Port: String;
    dbModel, svcModel: TSQLModel;
    DB: TSQLRestServerDB;
    SVC: TSQLRestServer;
  public 
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
{$WARN SYMBOL_PLATFORM OFF}

uses CalcInterface, FileCollect;

type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1,n2: integer): integer;
    function GetFileList(path: String; out lst: TFlileCollection): Boolean;
  end;

function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1+n2;
end;

function FileVersion(const FileName: TFileName): String;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('v%d.%d.%d build %d', [
            HiWord(dwFileVersionMS),   //Major
            LoWord(dwFileVersionMS),   //Minor
            HiWord(dwFileVersionLS),   //Release
            LoWord(dwFileVersionLS)]); //Build
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;


function TServiceCalculator.GetFileList(path: String; out lst: TFlileCollection): Boolean;
var
  SR : TSearchRec;
  ext: String;
begin
  Result := (FindFirst(path + '*.*', faArchive, SR) = 0);
  if Result then
  begin
    lst := TFlileCollection.Create;
    repeat
      with TFileItem(lst.Add()) do
      begin
        Name := SR.Name;
        Size := Sr.Size;
        ModificationDate := FileDateToDatetime(SR.Time);
        ext :=  ExtractFileExt(SR.Name);
        if AnsiSameText(ext, '.exe') or AnsiSameText(ext, '.dll') then
          Version := FileVersion(path+SR.Name);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;    
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  aHttpServerSecurity: TSQLHttpServerSecurity;
begin

  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE; // log all events to the console
  end;
  TSQLLog.Add.Log(sllInfo,'Starting');
  
  Port := PORT_HTTP;
  aHttpServerSecurity := secNone;

  Label1.Caption :=  'HTTP Server'; 

  if (ParamCount>0) then
  begin
    if (Paramstr(1)='ssl') then
      aHttpServerSecurity := secSSL;
    if (Paramstr(1)='sha') or (Paramstr(1)='aes') then
      aHttpServerSecurity := secSynShaAes;
    Label1.Caption := Label1.Caption +' '+AnsiUpperCase( Paramstr(1) );
  end;

  if aHttpServerSecurity <> secNone then
    Port := PORT_HTTPS;

  Label2.Caption := 'HTTP Server is running on port: '+Port;
  
  dbModel := CreateSampleModel;
  DB := TSQLRestServerDB.Create(dbModel,'server.db3',true);
  DB.CreateMissingTables;

  svcModel := TSQLModel.Create([],SERVICE_NAME);
  SVC := TSQLRestServerFullMemory.Create(svcModel,'users.json',false,true);
  SVC.ServiceDefine(TServiceCalculator,[ICalculator],sicShared);

  Server := TCustomHttpServer.Create(Port,[DB,SVC],'+',useHttpApiRegisteringURI,32,aHttpServerSecurity,'static');
  Server.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Free;
  SVC.Free;
  DB.Free;
  svcModel.Free;
  dbModel.Free;
end;


{ TCustomHttpServer }

function TCustomHttpServer.Request(Ctxt: THttpServerRequest): Cardinal;
var
  FileName: TFileName;
begin
  if (Ctxt.Method='GET')  and
    IdemPChar(pointer(Ctxt.URL),'/STATIC/') and
    (PosEx('..',Ctxt.URL)=0) then
  begin
    // http.sys will send the specified file from kernel mode
    FileName := '..\www\'+UTF8ToString(Copy(Ctxt.URL,9,maxInt));
    Ctxt.OutContent := StringToUTF8(FileName);
    Ctxt.OutContentType := HTTP_RESP_STATICFILE;
    Result := 200; // THttpApiServer.Execute will return 404 if not found
  end
  else                   
    Result := inherited Request(Ctxt); // call the associated TSQLRestServer instance(s)
end;




end.
