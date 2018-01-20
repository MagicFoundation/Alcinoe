unit RESTServerClass;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  RESTData;

type
  ENoteServer = class(EORMException);
  
  TNoteServer = class(TSQLRestServerFullMemory)
  protected
    fRootFolder: TFileName;
    fBlobFolder: TFileName;
  public
    constructor Create(const aRootFolder: TFileName; const aRootURI: RawUTF8); reintroduce;
    destructor Destroy; override;
    property RootFolder: TFileName read fRootFolder;
  published
    procedure Blob(Ctxt: TSQLRestServerURIContext);
  end;


implementation


{ TNoteServer }

constructor TNoteServer.Create(const aRootFolder: TFileName;
  const aRootURI: RawUTF8);
begin
  fRootFolder := EnsureDirectoryExists(ExpandFileName(aRootFolder),true);
  fBlobFolder := EnsureDirectoryExists(fRootFolder+'blob\',true);
  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE; // LOG_STACKTRACE;
    DestinationPath := fRootFolder+'..\log\';
    if not FileExists(DestinationPath) then
      CreateDir(DestinationPath);
    PerThreadLog := ptIdentifiedInOnFile;
  end;
  // prepare the server in-memory storage
  inherited Create(DataModel(aRootURI),fRootFolder+'data.json',false,false);
  UpdateToFile;
end;

destructor TNoteServer.Destroy;
begin
  inherited;
  fModel.Free;
end;

procedure TNoteServer.Blob(Ctxt: TSQLRestServerURIContext);
var FileName: TFileName;
begin
  if (Ctxt.Table=TSQLNoteFile) and (Ctxt.TableID<>0) then begin
    FileName := fBlobFolder+UTF8ToString(
      OneFieldValue(TSQLNoteFile,'FileName',Ctxt.TableID));
    case Ctxt.Method of
    mGET:
      Ctxt.ReturnFile(FileName);
    mPOST,mPUT: begin
      FileFromString(Ctxt.Call.InBody,FileName);
      Ctxt.Success;
    end;
    mDELETE:
      if DeleteFile(FileName) then
        Ctxt.Success else
        Ctxt.Error('',HTTP_NOTFOUND);
    end;
  end;
end;

end.
