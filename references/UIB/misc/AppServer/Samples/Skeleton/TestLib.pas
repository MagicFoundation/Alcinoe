unit TestLib;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface
uses
  TestLib_Server, TestLib_Intf, PDGUtils, PDGSocketStub,
  {$IFDEF FPC}sockets{$ELSE}Winsock{$ENDIF}, classes, uib;

type

  TMyServer = class(TSocketServer)
    function doOnCreateStub(ASocket: longint; AAddress: TSockAddr): TSocketStub; override;
  end;

  TMyObjectServer = class(TMyObjectStub)
  protected
    procedure ExecuteScript(const script: string; out data: TMemoryStream); override; stdcall;
  end;

  TMyObject2Server = class(TMyObject2Stub)
  protected
    function GetString: string; override; stdcall;
  end;


  TMyPool = class(TConnexionPool)
  private
    FDatabaseName: string;
    FUserName: string;
    FPassWord: string;
    FSQLDialect: Integer;
  protected
    procedure ConfigureConnexion(Database: TUIBDataBase); override; 
  public
    constructor Create(MaxSize: Integer = 0); override; 
  end;

implementation
uses PDGService, sysutils, inifiles, uibsqlparser;

var
  pool: TMyPool;

{ TMyPool }

procedure TMyPool.ConfigureConnexion(Database: TUIBDataBase);
begin
  Database.DatabaseName := FDatabaseName;
  Database.UserName := FUserName;
  Database.PassWord := FPassWord;
  Database.SQLDialect := FSQLDialect;
  // one thread should try to connect to database at the same time
  // this eliminate dead lock when there is too many thread trying to connect
  Database.Connected := true;
end;

constructor TMyPool.Create(MaxSize: Integer = 0);
var inifile: TIniFile;
begin
  inherited Create(MaxSize);
  inifile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AppServer.ini');
  try
    FDatabaseName := inifile.ReadString('DATABASE', 'DatabaseName', '');
    FUserName := inifile.ReadString('DATABASE', 'UserName', 'SYSDBA');
    FPassWord := inifile.ReadString('DATABASE', 'PassWord', 'masterkey');
    FSQLDialect := inifile.ReadInteger('DATABASE', 'SQLDialect', 3);
  finally
    inifile.Free;
  end;
end;

{ TMyServer }

function TMyServer.doOnCreateStub(ASocket: longint;
  AAddress: TSockAddr): TSocketStub;
var
  uid: TGUID;
begin
  if recv(ASocket, uid, sizeof(uid), 0) <> sizeof(uid) then
    Result := nil else
  if CompareMem(@uid, @CLSID_MyObject, sizeof(uid)) then
    Result := TMyObjectServer.CreateStub(Self, ASocket, AAddress) else
  if CompareMem(@uid, @CLSID_MyObject2, sizeof(uid)) then
    Result := TMyObject2Server.CreateStub(Self, ASocket, AAddress) else
    Result := nil;
end;

{ TMyObjectServer }

procedure TMyObjectServer.ExecuteScript(const script: string;
  out data: TMemoryStream); stdcall;
var
  Strings: TStringList;
  Parser: TUIBSQLParser;
  QR: TUIBQuery;
  TR: TUIBTransaction;
  DB: TUIBDataBase;
  st: TSQLStatement;
begin
  Strings := TStringList.Create;
  Strings.Text := Script;
  Parser := TUIBSQLParser.Create(Strings);
  DB := pool.GetConnexion;
  TR := TUIBTransaction.Create(nil);
  QR := TUIBQuery.Create(nil);
  try
    TR.Options := [tpReadCommitted, tpRecVersion, tpNowait];
    TR.DataBase := DB;
    QR.Transaction := TR;
    QR.FetchBlobs := True;
    while true do
    begin
      st := Parser.NextStatement;
      if st = ssEOF then
        Break;
      case st of
        ssSetSqlDialect, ssSetNames, ssCreateDatabase, ssConnect, ssAutoDDL: ;
        ssCommit: TR.Commit;
        ssRollback: TR.RollBack;
        ssSelect:
          begin
            QR.SQL.Text := trim(Parser.Statement);
            QR.Open; QR.FetchAll;
            QR.Fields.SaveToStream(data);
            QR.Close(etmStayIn);
          end;
      else
        TR.ExecuteImmediate(trim(Parser.Statement));
      end;
    end;
    TR.Commit;
  finally
    Strings.Free;
    Parser.Free;
    QR.Free;
    TR.Free;
    pool.FreeConnexion;
  end;
end;

{ TMyObject2Server }

function TMyObject2Server.GetString: string; stdcall;
begin
  Result := 'Hello word';
end;

initialization
  pool := TMyPool.Create(0);
  Application.CreateServer(TMyServer, 33000);
  
finalization
  // don't free database pool until all threads are terminated
  while TPDGThread.ThreadCount > 0 do sleep(100);
  pool.Free;

end.

