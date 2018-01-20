/// SynFile server handling
unit FileServer;

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  mORMoti18n,
  mORMotHttpServer,
  mORMotSQLite3,
  SynSQLite3Static,
  FileTables;

type
  /// a server to access SynFile data content
  TFileServer = class(TSQLRestserverDB)
  private
    fTempAuditTrail: TSQLAuditTrail;
  public
    /// the runing HTTP/1.1 server
    Server: TSQLHttpServer;
  	/// create the database and HTTP/1.1 server
    constructor Create;
	  /// release used memory and data
    destructor Destroy; override;
  	/// add a row to the TSQLAuditTrail table
    procedure AddAuditTrail(aEvent: TFileEvent; const aMessage: RawUTF8='';
      aAssociatedRecord: TRecordReference=0);
	  /// database server-side trigger which will add an event to the
  	// TSQLAuditTrail table
    function OnDatabaseUpdateEvent(Sender: TSQLRestServer;
      Event: TSQLEvent; aTable: TSQLRecordClass; const aID: TID;
      const aSentData: RawUTF8): boolean;
  published
    /// a RESTful service used from the client side to add an event
  	// to the TSQLAuditTrail table
	  // - an optional database record can be specified in order to be
  	// associated with the event
    procedure Event(Ctxt: TSQLRestServerURIContext);
  end;


implementation


{ TFileServer }

procedure TFileServer.AddAuditTrail(aEvent: TFileEvent;
  const aMessage: RawUTF8; aAssociatedRecord: TRecordReference);
var T: TSQLRecordClass;
    tmp: RawUTF8;
begin
  if fTempAuditTrail=nil then
    fTempAuditTrail := TSQLAuditTrail.Create;
  fTempAuditTrail.Time := TimeLogNow;
  fTempAuditTrail.Status := aEvent;
  fTempAuditTrail.StatusMessage := aMessage;
  fTempAuditTrail.AssociatedRecord := aAssociatedRecord;
  if (aMessage='') and (aAssociatedRecord<>0) then
    with RecordRef(aAssociatedRecord) do begin
      T := Table(Model);
      if T.InheritsFrom(TSQLFile) then
        tmp := '"'+OneFieldValue(T,'Name',ID)+'"' else
        tmp := {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(ID);
      fTempAuditTrail.StatusMessage := T.RecordProps.SQLTableName+'  '+tmp;
    end;
  Add(fTempAuditTrail,true);
end;

constructor TFileServer.Create;
begin
  inherited Create(CreateFileModel(self),ChangeFileExt(ExeVersion.ProgramFileName,'.db3'));
  CreateMissingTables(ExeVersion.Version.Version32);
  Server := TSQLHttpServer.Create(SERVER_HTTP_PORT,self,'+',useHttpApiRegisteringURI);
  AddAuditTrail(feServerStarted);
  OnUpdateEvent := OnDatabaseUpdateEvent;
end;

destructor TFileServer.Destroy;
begin
  try
    AddAuditTrail(feServerShutdown);
    FreeAndNil(fTempAuditTrail);
    FreeAndNil(Server);
  finally
    inherited;
  end;
end;

procedure TFileServer.Event(Ctxt: TSQLRestServerURIContext);
var E: integer;
begin
  if UrlDecodeInteger(Ctxt.Parameters,'EVENT=',E) and
     (E>ord(feUnknownState)) and (E<=ord(High(TFileEvent))) then begin
    AddAuditTrail(TFileEvent(E),'',RecordReference(Model,Ctxt.Table,Ctxt.TableID));
    Ctxt.Success;
  end else
    Ctxt.Error;
end;

function TFileServer.OnDatabaseUpdateEvent(Sender: TSQLRestServer;
  Event: TSQLEvent; aTable: TSQLRecordClass; const aID: TID;
  const aSentData: RawUTF8): boolean;
const EVENT_FROM_SQLEVENT: array[low(TSQLEvent)..seDelete] of TFileEvent = (
  feRecordCreated, feRecordModified, feRecordDeleted);
begin
  result := true;
  if aTable.InheritsFrom(TSQLFile) and (Event<=high(EVENT_FROM_SQLEVENT)) then
    AddAuditTrail(EVENT_FROM_SQLEVENT[Event], '', Model.RecordReference(aTable,aID));
end;

end.
