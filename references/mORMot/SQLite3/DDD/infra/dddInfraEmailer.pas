/// shared DDD Infrastructure: generic emailing service
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraEmailer;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows, // for fSafe.Lock/Unlock inlining
  {$endif}
  {$ifdef KYLIX3}
  Types,
  LibC,
  {$endif}
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  SynTests,
  SynCrtSock,
  SynMustache,
  SynTable,
  SyncObjs,
  mORMot,
  mORMotDDD,
  dddDomUserTypes,
  dddDomUserInterfaces,
  dddInfraEmail; // for TDDDEmailServiceAbstract


{ ****************** Email Sending Service }

type
  /// used to inject the exact SMTP process to TDDDEmailerDaemon
  ISMTPServerConnection = interface(IInvokable)
    ['{00479813-4CAB-4563-BD51-AB6606BC7BEE}']
    /// this method should send the email, returning an error message on issue
    // - if no header is supplied, it will expect one UTF-8 encoded text message
    function SendEmail(const aRecipient: TRawUTF8DynArray;
      const aSender,aSubject,aHeader,aBody: RawUTF8): RawUTF8;
  end;

  /// abstract class used to resolve ISMTPServerConnection
  // - see TSMTPServerSocket for actual implementation
  TSMTPServer = class(TInterfaceResolverForSingleInterface)
  protected
    fAddress: RawUTF8;
    fPort: cardinal;
    fLogin: RawUTF8;
    fPassword: RawUTF8;
    function CreateInstance: TInterfacedObject; override;
  public
    /// initialize the class with the supplied parameters
    constructor Create(aImplementation: TInterfacedObjectClass;
      const aAddress: RawUTF8; aPort: cardinal; const aLogin,aPassword: RawUTF8); overload;
    /// initialize the class with the parameters of another TSMTPServer instance
    // - in fact, TSMTPServer could be used as parameter storage of its needed
    // published properties, e.g. in a TApplicationSettingsAbstract sub-class 
    constructor Create(aImplementation: TInterfacedObjectClass;
      aParameters: TSMTPServer); overload;
    /// will fill some default values in the properties, if none is set
    // - i.e. 'dummy:dummy@localhost:25'
    procedure SetDefaultValuesIfVoid;
  published
    property Address: RawUTF8 read fAddress write fAddress;
    property Port: cardinal read fPort write fPort;
    property Login: RawUTF8 read fLogin write fLogin;
    property Password: RawUTF8 read fPassword write fPassword;
  end;

  /// implements an abstract ISMTPServerConnection class
  TSMTPServerSocketConnectionAbstract = class(TInterfacedObject,ISMTPServerConnection)
  protected
    fOwner: TSMTPServer;
  public
    constructor Create(aOwner: TSMTPServer); virtual;
    function SendEmail(const aRecipient: TRawUTF8DynArray;
      const aSender,aSubject,aHeader,aBody: RawUTF8): RawUTF8; virtual; abstract;
  end;

  TSMTPServerSocketConnectionAbstractClass = class of TSMTPServerSocketConnectionAbstract;

  /// implements ISMTPServerConnection using SynCrtSock's low-level SMTP access
  TSMTPServerSocketConnection = class(TSMTPServerSocketConnectionAbstract)
  protected
    fSocket: TCrtSocket;
    procedure Expect(const Answer: RawByteString);
    procedure Exec(const Command, Answer: RawByteString);
  public
    constructor Create(aOwner: TSMTPServer); override;
    destructor Destroy; override;
    function SendEmail(const aRecipient: TRawUTF8DynArray;
      const aSender,aSubject,aHeader,aBody: RawUTF8): RawUTF8; override;
  end;

  TSQLRecordEmailer = class;
  TSQLRecordEmailerClass = class of TSQLRecordEmailer;
  TDDDEmailerDaemon = class;

  /// statistics about a TDDDEmailerDaemon instance
  // - in addition to a standard TSynMonitor, will maintain the connection count
  TDDDEmailerDaemonStats = class(TSynMonitorWithSize)
  protected
    fConnection: cardinal;
    procedure LockedSum(another: TSynMonitor); override;
  public
    /// will increase the connection count
    procedure NewConnection;
  published
    /// the connection count
    property Connection: cardinal read fConnection;
  end;

  /// thread processing a SMTP connection
  TDDDEmailerDaemonProcess = class(TDDDMonitoredDaemonProcessRest)
  protected
    fSMTPConnection: ISMTPServerConnection;
    // all the low-level process will take place in those overriden methods
    function ExecuteRetrievePendingAndSetProcessing: boolean; override;
    function ExecuteProcessAndSetResult: QWord; override;
    procedure ExecuteIdle; override;
  end;

  /// daemon used to send emails via SMTP
  // - it will maintain a list of action in a TSQLRecordEmailer ORM storage
  TDDDEmailerDaemon = class(TDDDMonitoredDaemon,IDomUserEmailer)
  protected
    fRestClass: TSQLRecordEmailerClass;
    fSMTPServer: TSMTPServer;
  public
    constructor Create(aRest: TSQLRest); overload; override;
    constructor Create(aRest: TSQLRest; aSMTPServer: TSMTPServer;
      aConnectionPool: integer=1); reintroduce; overload;
    /// this is the main entry point of this service
    // - here the supplied message body is already fully encoded, as
    // expected by SMTP (i.e. as one text message, or multi-part encoded)
    // - if no header is supplied, it will expect one UTF-8 encoded text message 
    function SendEmail(const aRecipients: TRawUTF8DynArray;
      const aSender,aSubject,aHeaders,aBody: RawUTF8): TCQRSResult;
  published
    /// the associated class TSQLRecordEmailer used for status persistence
    // - any class inheriting from TSQLRecordEmailer in the Rest.Model
    // will be recognized by TDDDEmailerDaemon to store its information
    property RestClass: TSQLRecordEmailerClass read fRestClass;
    /// the associated class used as actual SMTP client
    property SMTPServer: TSMTPServer read fSMTPServer write fSMTPServer;
  end;

  /// state machine used during email validation process
  TSQLRecordEmailerState = (esPending, esSending, esSent, esFailed);

  /// ORM class for email validation process
  // - we do not create a whole domain here, just an ORM persistence layer
  TSQLRecordEmailer = class(TSQLRecordTimed)
  private
    fSender: RawUTF8;
    fRecipients: TRawUTF8DynArray;
    fSubject: RawUTF8;
    fHeaders: RawUTF8;
    fErrorMsg: RawUTF8;
    fSendTime: TTimeLog;
    fMessageCompressed: TByteDynArray; // will be transmitted as Base64 JSON
    fState: TSQLRecordEmailerState;
  public
    // will create an index on State+ID
    class procedure InitializeTable(Server: TSQLRestServer; const FieldName: RawUTF8;
      Options: TSQLInitializeTableOptions); override;
  published
    property Sender: RawUTF8 read fSender write fSender;
    property Recipients: TRawUTF8DynArray read fRecipients write fRecipients;
    property Subject: RawUTF8 read fSubject write fSubject;
    property Headers: RawUTF8 read fHeaders write fHeaders;
    property State: TSQLRecordEmailerState read fState write fState;
    property MessageCompressed: TByteDynArray read fMessageCompressed write fMessageCompressed;
    property SendTime: TTimeLog read fSendTime write fSendTime;
    property ErrorMsg: RawUTF8 read fErrorMsg write fErrorMsg;
  end;


{ ****************** Mustache-Based Templating Service }

type
  /// abstract Mustache-Based templating
  TDDDTemplateAbstract = class(TCQRSService,IDomUserTemplate)
  protected
    fPartials: TSynMustachePartials;
    fHelpers: TSynMustacheHelpers;
    fOnTranslate: TOnStringTranslate;
    fCache: TSynCache;
    function RetrieveTemplate(const aTemplateName: RawUTF8;
      out aTemplate, aType: RawUTF8): boolean; virtual; abstract;
  public
    destructor Destroy; override;
    function ComputeMessage(const aContext: variant;
      const aTemplateName: RawUTF8): RawUTF8;
    property Partials: TSynMustachePartials read fPartials write fPartials;
    property Helpers: TSynMustacheHelpers read fHelpers write fHelpers;
    property OnTranslate: TOnStringTranslate read fOnTranslate write fOnTranslate;
  end;

  /// Mustache-Based templating from a local folder
  TDDDTemplateFromFolder = class(TDDDTemplateAbstract)
  protected
    fFolder: TFileName;
    fMemoryCacheSize: integer;
    function RetrieveTemplate(const aTemplateName: RawUTF8;
      out aTemplate, aType: RawUTF8): boolean; override;
    procedure SetFolder(const Value: TFileName); virtual;
    procedure SetMemoryCacheSize(const Value: integer);
  public
    constructor Create(const aTemplateFolder: TFileName;
      aMemoryCacheSize: integer=1024*2048); reintroduce;
  published
    property Folder: TFileName read fFolder write SetFolder;
    property MemoryCacheSize: integer read fMemoryCacheSize write SetMemoryCacheSize;
  end;


/// you can call this function within a TSynTestCase class to validate
// the email validation via a full regression set
// - could be used as such:
// !procedure TTestCrossCuttingFeatures.Emailer;
// !begin // TSQLRestServerDB is injected to avoid any dependency to mORMotSQLite3
// !  TestDddInfraEmailer(TSQLRestServerDB,self);
// !end;
procedure TestDddInfraEmailer(serverClass: TSQLRestServerClass; test: TSynTestCase);


implementation


{ ****************** Email Sending Service }

{ TSMTPServer }

function TSMTPServer.CreateInstance: TInterfacedObject;
begin
  result := TSMTPServerSocketConnectionAbstractClass(fImplementation.ItemClass).
    Create(self);
end;

constructor TSMTPServer.Create(aImplementation: TInterfacedObjectClass;
  const aAddress: RawUTF8; aPort: cardinal; const aLogin, aPassword: RawUTF8);
begin
  inherited Create(TypeInfo(ISMTPServerConnection),aImplementation);
  fAddress := aAddress;
  fPort := aPort;
  fLogin := aLogin;
  fPassword := aPassword;
end;

constructor TSMTPServer.Create(aImplementation: TInterfacedObjectClass;
  aParameters: TSMTPServer);
begin
  if (aParameters=nil) or (aImplementation=nil) then
    raise EDDDEmail.CreateUTF8('%.Create(nil)',[self]);
  Create(aImplementation,
    aParameters.Address,aParameters.Port,aParameters.Login,aParameters.Password);
end;

procedure TSMTPServer.SetDefaultValuesIfVoid;
begin
  if Address='' then
    Address := 'localhost';
  if Port=0 then begin
    Port := 25;
    if Login='' then
      Login := 'dummy';
    if Password='' then
      Password := 'dummy';
  end;
end;

{ TSMTPServerSocketConnectionAbstract }

constructor TSMTPServerSocketConnectionAbstract.Create(
  aOwner: TSMTPServer);
begin
  fOwner := aOwner;
end;


{ TSMTPServerSocketConnection }

{$I+} // low-level communication with readln/writeln should raise exception

constructor TSMTPServerSocketConnection.Create(
  aOwner: TSMTPServer);
begin
  inherited Create(aOwner);
  fSocket := TCrtSocket.Open(fOwner.Address,UInt32ToUtf8(fOwner.Port));
  fSocket.CreateSockIn; // we use SockIn and SockOut here
  fSocket.CreateSockOut(64*1024);
  Expect('220');
  if (fOwner.Login<>'') and (fOwner.Password<>'') then begin
    Exec('EHLO '+fOwner.Address,'25');
    Exec('AUTH LOGIN','334');
    Exec(BinToBase64(fOwner.Login),'334');
    Exec(BinToBase64(fOwner.Password),'235');
  end else
    Exec('HELO '+fOwner.Address,'25');
end;

procedure TSMTPServerSocketConnection.Expect(const Answer: RawByteString);
var Res: RawByteString;
begin
  repeat
    readln(fSocket.SockIn^,Res);
  until (Length(Res)<4)or(Res[4]<>'-');
  if not IdemPChar(pointer(Res),pointer(Answer)) then
    raise ECrtSocket.CreateFmt('returned [%s], expecting [%s]',[Res,Answer]);
end;

procedure TSMTPServerSocketConnection.Exec(const Command,
  Answer: RawByteString);
begin
  writeln(fSocket.SockOut^,Command);
  Expect(Answer)
end;

function TSMTPServerSocketConnection.SendEmail(
  const aRecipient: TRawUTF8DynArray;
  const aSender,aSubject,aHeader,aBody: RawUTF8): RawUTF8;
var rcpt,toList,head: RawUTF8;
    i: integer;
begin
  if (aRecipient=nil) or (aSender='') or (aBody='') then
    result := FormatUTF8('Invalid parameters for %.SendEmail(%:%,%)',
      [self,fOwner.Address,fOwner.Port,aSender]) else
    try
      writeln(fSocket.SockOut^,'MAIL FROM:<',aSender,'>');
      Expect('250');
      toList := 'To: ';
      for i := 0 to high(aRecipient) do begin
        rcpt := aRecipient[i];
        if PosExChar('<',rcpt)=0 then
          rcpt := '<'+rcpt+'>';
        Exec('RCPT TO:'+rcpt,'25');
        toList := toList+rcpt+', ';
      end;
      Exec('DATA','354');
      write(fSocket.SockOut^,'From: ',aSender,#13#10'Subject: ');
      if aSubject='' then
        writeln(fSocket.SockOut^,'Information') else
        if IsAnsiCompatible(PAnsiChar(pointer(aSubject))) then
          writeln(fSocket.SockOut^,aSubject) else
          writeln(fSocket.SockOut^,'=?utf-8?B?',BinToBase64(aSubject));
      writeln(fSocket.SockOut^,toList);
      head := Trim(aHeader);
      if head='' then // default format is simple UTF-8 text message
        head := 'Content-Type: text/plain; charset=utf-8'#13#10+
          'Content-Transfer-Encoding: 8bit';
      writeln(fSocket.SockOut^,head);
      writeln(fSocket.SockOut^,#13#10,aBody,#13#10'.');
      Expect('25');
      result := ''; // for success
    except
      on E: Exception do
        result := FormatUTF8('%.SendEmail(%:%) server failure % [%]',
          [self,fOwner.Address,fOwner.Port,E,E.Message]);
    end;
end;

{$I-}

destructor TSMTPServerSocketConnection.Destroy;
begin
  try
    if fSocket<>nil then begin
      writeln(fSocket.SockOut^,'QUIT');
      ioresult; // ignore any error within writeln() since we are after $I-
    end;
  finally
    FreeAndNil(fSocket);
    inherited Destroy;
  end;
end;



{ TSQLRecordEmailer }

class procedure TSQLRecordEmailer.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
begin
  inherited;
  if (FieldName='') or IdemPropNameU(FieldName,'State') then
    Server.CreateSQLMultiIndex(self,['State','ID'],false);
end;


{ TDDDEmailerDaemonProcess }

const
  EMAILERSTAT_CONNECTIONCOUNT = 0;
  
function TDDDEmailerDaemonProcess.ExecuteRetrievePendingAndSetProcessing: boolean;
begin
  fPendingTask := (fDaemon as TDDDEmailerDaemon).RestClass.Create(
    fDaemon.Rest,'State=? order by RowID',[ord(esPending)]);
  if fPendingTask.ID=0 then begin
    result := false; // no more fPendingTask tasks
    exit;
  end;
  with fPendingTask as TSQLRecordEmailer do begin
    State := esSending;
    SendTime := TimeLogNowUTC;
  end;
  result := fDaemon.Rest.Update(fPendingTask,'State,SendTime');
end;

function TDDDEmailerDaemonProcess.ExecuteProcessAndSetResult: QWord;
var body: RawByteString;
    pendingEmail: TSQLRecordEmailer;
begin
  pendingEmail := fPendingTask as TSQLRecordEmailer;
  body := SynLZDecompress(pendingEmail.MessageCompressed);
  result := length(body);
  fMonitoring.AddSize(length(body));
  if fSMTPConnection=nil then begin // re-use the same connection
    fDaemon.Resolve([ISMTPServerConnection],[@fSMTPConnection]);
    (fMonitoring as TDDDEmailerDaemonStats).NewConnection;
  end;
  pendingEmail.ErrorMsg := fSMTPConnection.SendEmail(
    pendingEmail.Recipients,pendingEmail.Sender,pendingEmail.Subject,
    pendingEmail.Headers,body);
  if pendingEmail.ErrorMsg='' then
    pendingEmail.State := esSent else
    pendingEmail.State := esFailed;
  fDaemon.Rest.Update(pendingEmail,'State,ErrorMsg'); // always write
end;

procedure TDDDEmailerDaemonProcess.ExecuteIdle;
begin
  fSMTPConnection := nil; // release ISMTPServerConnection instance
end;


{ TDDDEmailerDaemon }

constructor TDDDEmailerDaemon.Create(aRest: TSQLRest);
begin
  fResolver := fSMTPServer; // do it before aRest.Services is set
  inherited Create(aRest);
  fRestClass := Rest.Model.AddTableInherited(TSQLRecordEmailer);
  RestClass.AddFilterNotVoidText(['MessageCompressed']);
  fProcessClass := TDDDEmailerDaemonProcess;
  fProcessMonitoringClass := TDDDEmailerDaemonStats;
  fProcessIdleDelay := 1000; // checking for pending emails every second
end;

constructor TDDDEmailerDaemon.Create(aRest: TSQLRest;
  aSMTPServer: TSMTPServer; aConnectionPool: integer);
begin
  if not Assigned(aSMTPServer) then
    raise ECQRSException.CreateUTF8('%.Create(SMTPServer=nil)',[self]);
  fProcessThreadCount := aConnectionPool;
  fSMTPServer := aSMTPServer;
  Create(aRest);
end;

function TDDDEmailerDaemon.SendEmail(const aRecipients: TRawUTF8DynArray;
  const aSender,aSubject,aHeaders,aBody: RawUTF8): TCQRSResult;
var Email: TSQLRecordEmailer;
    msg: string;
begin
{  result := CheckRecipients(aRecipient);
  if result<>cqrsSuccess then
    exit; }
  Email := RestClass.Create;
  try
    Email.Recipients := aRecipients;
    Email.Sender := aSender;
    Email.Subject := aSubject;
    Email.Headers := aHeaders;
    {$ifdef WITHLOG}
    Rest.LogClass.Enter('SendEmail %',[Email],self);
    {$endif}
    Email.MessageCompressed := SynLZCompressToBytes(aBody);
    CqrsBeginMethod(qaNone,result);
    if not Email.FilterAndValidate(Rest,msg) then
      CqrsSetResultString(cqrsDDDValidationFailed,msg,result) else
      if Rest.Add(Email,true)=0 then
        CqrsSetResult(cqrsDataLayerError,result) else
        CqrsSetResult(cqrsSuccess,result);
  finally
    Email.Free;
  end;
end;


{ ****************** Mustache-Based Templating Service }

{ TDDDTemplateAbstract }

function TDDDTemplateAbstract.ComputeMessage(const aContext: variant;
  const aTemplateName: RawUTF8): RawUTF8;
var template,templateType: RawUTF8;
    escapeInvert: boolean;
begin
  result := '';
  if not RetrieveTemplate(aTemplateName,template,templateType) then
    exit;
  escapeInvert := false;
  if (PosEx('html',templateType)<0) and (PosEx('xml',templateType)<0) then
    escapeInvert := true; // may be JSON or plain TEXT
  // TODO: compute multi-part message with optional text reduction of the html
  result := TSynMustache.Parse(template).Render(aContext,
    Partials,Helpers,OnTranslate,escapeInvert);
end;

destructor TDDDTemplateAbstract.Destroy;
begin
  fPartials.Free;
  fCache.Free;
  inherited;
end;


{ TDDDTemplateFromFolder }

constructor TDDDTemplateFromFolder.Create(
  const aTemplateFolder: TFileName; aMemoryCacheSize: integer);
begin
  inherited Create;
  if aTemplateFolder='' then
    fFolder := IncludeTrailingPathDelimiter(GetCurrentDir) else begin
    fFolder := IncludeTrailingPathDelimiter(ExpandFileName(aTemplateFolder));
    if not DirectoryExists(Folder) then
      raise ESynMustache.CreateUTF8('%.Create(%) is not a valid folder',[self,Folder]);
  end;
  fMemoryCacheSize := aMemoryCacheSize;
end;

function TDDDTemplateFromFolder.RetrieveTemplate(
  const aTemplateName: RawUTF8; out aTemplate, aType: RawUTF8): boolean;
var age: integer;
    ageInCache: PtrInt;
    filename: TFileName;
begin
  result := false;
  if (aTemplateName='') or (PosEx('..',aTemplateName)>0) or
     (aTemplateName[2]=':') then
    exit; // for security reasons
  filename := fFolder+UTF8ToString(Trim(aTemplateName));
  {$WARN SYMBOL_DEPRECATED OFF} // we don't need full precision, just some value
  age := FileAge(filename);
  {$WARN SYMBOL_DEPRECATED ON}
  if age<=0 then
    exit;
  fSafe.Lock;
  try
    if fCache=nil then
      fCache := TSynCache.Create(MemoryCacheSize);
    aTemplate := fCache.Find(aTemplateName,@ageInCache);
    if (aTemplate='') or (ageInCache<>age) then begin
      aTemplate := AnyTextFileToRawUTF8(filename,true);
      if (aTemplate<>'') or (ageInCache<>0) then begin
        fCache.Add(aTemplate,age);
        result := true;
      end;
    end else
      result := true; // from cache
  finally
    fSafe.UnLock;
  end;
  aType := GetMimeContentType(pointer(aTemplate),length(aTemplate),filename);
end;

procedure TDDDTemplateFromFolder.SetFolder(const Value: TFileName);
begin
  fSafe.Lock;
  try
    fFolder := Value;
    fCache.Reset;
  finally
    fSafe.UnLock;
  end;
end;

procedure TDDDTemplateFromFolder.SetMemoryCacheSize(
  const Value: integer);
begin
  fSafe.Lock;
  try
    fMemoryCacheSize := Value;
    FreeAndNil(fCache);
  finally
    fSafe.UnLock;
  end;
end;


{ TDDDEmailerDaemonStats }

procedure TDDDEmailerDaemonStats.NewConnection;
begin
  fSafe^.Lock;
  try
    inc(fConnection);
  finally
    fSafe^.UnLock;
  end;
end;

procedure TDDDEmailerDaemonStats.LockedSum(another: TSynMonitor);
begin
  inherited LockedSum(another);
  if another.InheritsFrom(TDDDEmailerDaemonStats) then
    inc(fConnection,TDDDEmailerDaemonStats(another).Connection);
end;


procedure TestDddInfraEmailer(serverClass: TSQLRestServerClass; test: TSynTestCase);
var Rest: TSQLRestServer;
    daemon: TDDDEmailerDaemon;
    daemonLocal: IUnknown;
    smtpMock: TInterfaceMockSpy;
    service: TDDDEmailValidationService;
    valid: TSQLRecordEmailValidation;
    template: TDomUserEmailTemplate;
    email: TSQLRecordEmailer;
    info: variant;
    call: TSQLRestURIParams;
    start: Int64;
begin
  // generate test ORM file for DDD persistence 
  TDDDRepositoryRestFactory.ComputeSQLRecord([
    TDDDEmailerDaemonStats,TSQLRestServerMonitor]);
  // we test here up to the raw SMTP socket layer
  Rest := serverClass.CreateWithOwnModel([]);
  try
    template := TDomUserEmailTemplate.Create;
    smtpMock := TInterfaceMockSpy.Create(ISMTPServerConnection,test);
    smtpMock.ExpectsCount('SendEmail',qoGreaterThanOrEqualTo,1);
    daemon := TDDDEmailerDaemon.CreateInjected(Rest,[],[smtpMock],[]);
    daemonLocal := daemon; // ensure daemon won't be released when resolved
    service := TDDDEmailValidationService.CreateInjected(Rest,[],
      [TInterfaceStub.Create(IDomUserTemplate).
        Returns('ComputeMessage',['body'])],
      [daemon]);
    with test do
    try
      Rest.CreateMissingTables; // after Rest.Model has been completed
      service.SetURIForServer(Rest,'http://validationserver/root',
        'http://officialwebsite/success&logon=%','valid');
      Check(Rest.TableRowCount(TSQLRecordEmailValidation)=0);
      Check(Rest.TableRowCount(TSQLRecordEmailer)=0);
      Check(not service.IsEmailValidated('toto','toto@toto.com'));
      template.FileName := 'any';
      template.Subject := 'Please Validate Your Email';
      Check(service.StartEmailValidation(template,'toto','toto@toto .com')=cqrsDDDValidationFailed);
      Check(service.StartEmailValidation(template,' ','toto@toto.com')=cqrsDDDValidationFailed);
      Check(service.StartEmailValidation(template,'toto','toto@toto.com')=cqrsSuccess);
      info := service.LastErrorInfo;
      Check(VariantToUTF8(info)='{"Msg":"Validation email sent"}');
      Check(not service.IsEmailValidated('toto','toto@toto.com'));
      Check(Rest.TableRowCount(TSQLRecordEmailValidation)=1);
      Check(Rest.TableRowCount(TSQLRecordEmailer)=1);
      valid := TSQLRecordEmailValidation.Create(Rest,1);
      Check(valid.Logon='toto');
      Check(valid.RequestTime<>0);
      Check(valid.ValidationTime=0);
      valid.Free;
      email := TSQLRecordEmailer.Create(Rest,1);
      Check((length(email.Recipients)=1) and (email.Recipients[0]='toto@toto.com'));
      Check(email.SendTime=0);
      Check(SynLZDecompress(email.MessageCompressed)='body');
      email.Free;
      Check(daemon.RetrieveState(info)=cqrsSuccess);
      Check(info.stats.taskcount=0);
      Check(info.stats.connection=0);
      daemon.ProcessIdleDelay := 1; // speed up tests
      Check(daemon.Start=cqrsSuccess);
      Check(daemon.RetrieveState(info)=cqrsSuccess);
      start := GetTickCount64;
      repeat
        Sleep(1);
        email := TSQLRecordEmailer.Create(Rest,1);
        Check((length(email.Recipients)=1) and (email.Recipients[0]='toto@toto.com'));
        if email.SendTime<>0 then
          break;
        FreeAndNil(email);
      until GetTickCount64-start>5000;
      if CheckFailed((email<>nil)and(email.SendTime<>0),
         'Emailer thread sent message to toto@toto.com') then
        exit;
      Check(SynLZDecompress(email.MessageCompressed)='body');
      email.Free;
      Check(daemon.RetrieveState(info)=cqrsSuccess);
      Check(info.stats.taskcount=1);
      Check(info.stats.connection=1);
      Check(not service.IsEmailValidated('toto','toto@toto.com'),'no click yet');
      call.Url := service.ComputeURIForReply('titi','toto@toto.com');
      Check(IdemPChar(pointer(call.Url),'HTTP://VALIDATIONSERVER/ROOT/VALID/'));
      delete(call.Url,1,24);
      Check(IdemPChar(pointer(call.Url),'ROOT/VALID/'),'deleted host in URI');
      call.Method := 'GET';
      Rest.URI(call);
      Check(call.OutStatus=HTTP_BADREQUEST,'wrong link');
      call.Url := service.ComputeURIForReply('toto','toto@toto.com');
      delete(call.Url,1,24);
      call.Method := 'GET';
      Rest.URI(call);
      Check(call.OutStatus=HTTP_TEMPORARYREDIRECT,'emulated click on link');
      Check(call.OutHead='Location: http://officialwebsite/success&logon=toto');
      Check(service.IsEmailValidated('toto','toto@toto.com'),'after click');
      Check(daemon.Stop(info)=cqrsSuccess);
      Check(service.StartEmailValidation(template,'toto','toto@toto.com')=cqrsSuccess);
      info := service.LastErrorInfo;
      Check(VariantToUTF8(info)='{"Msg":"Already validated"}');
      Check(service.StartEmailValidation(template,'toto','toto2@toto.com')=cqrsSuccess);
      info := service.LastErrorInfo;
      Check(VariantToUTF8(info)='{"Msg":"Validation email sent"}');
      Check(Rest.TableRowCount(TSQLRecordEmailValidation)=1);
      Check(Rest.TableRowCount(TSQLRecordEmailer)=2);
      Check(daemon.Start=cqrsSuccess);
      start := GetTickCount64;
      repeat
        Sleep(1);
        email := TSQLRecordEmailer.Create(Rest,2);
        Check((length(email.Recipients)=1) and (email.Recipients[0]='toto2@toto.com'));
        Check(email.Subject='Please Validate Your Email');
        if email.SendTime<>0 then
          break;
        FreeAndNil(email);
      until GetTickCount64-start>5000;
      if CheckFailed((email<>nil)and(email.SendTime<>0),
         'Emailer thread sent message to toto2@toto.com') then
        exit;
      Check(SynLZDecompress(email.MessageCompressed)='body');
      email.Free;
      sleep(10);
      Check(daemon.Stop(info)=cqrsSuccess);
      Check(info.working=0);
      smtpMock.Verify('SendEmail',qoEqualTo,2);
    finally
      service.Free;
      template.Free;
    end;
    info := Rest.Stats.ComputeDetails;
    test.Check(info.ServiceMethod=2,'called root/valid twice');
    test.Check(info.Errors=1,'root/valid titi');
    test.Check(info.Success=1,'root/valid toto');
    call.Url := 'root/stat?withall=true';
    Rest.URI(call);
    test.Check(PosEx('{"valid":{',call.OutBody)>0,'stats for root/valid');
    FileFromString(JSONReformat(call.OutBody),'stats.json');
  finally
    Rest.Free;
  end;
end;


initialization
  TInterfaceFactory.RegisterInterfaces([TypeInfo(ISMTPServerConnection)]);
end.
