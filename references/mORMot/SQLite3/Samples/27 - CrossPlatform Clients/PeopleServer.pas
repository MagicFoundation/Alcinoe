unit PeopleServer;

interface

{$define TESTRECORD}

uses
  SynCommons,
  mORMot,
  mORMotHttpServer,
  mORMotWrappers,
  SynMustache,
  SysUtils;

type
  {$ifdef TESTRECORD}
  TRecordEnum = (reOne, reTwo, reLast);

  TTestCustomJSONArraySimpleArray = packed record
    F: RawUTF8;
    G: array of RawUTF8;
    H: record
      H1: integer;
      H2: WideString;
      H3: record
        H3a: boolean;
        H3b: RawByteString;
      end;
    end;
    I: TDateTime;
    J: array of packed record
      J1: byte;
      J2: TGUID;
      J3: TRecordEnum;
    end;
  end;
  {$endif TESTRECORD}

  {
  TRecB=packed record
    a1:array of packed record
      v1,v2:integer;
    end;
    a2:array of integer;
    v1:integer;
  end;
  }

  TPeopleSexe = (sFemale, sMale);

  TPeopleSexeDynArray = array of TPeopleSexe;

  TSimpleRecord = packed record
    A,B: integer;
    C: RawUTF8;
  end;

  TSimpleRecordDynArray = array of TSimpleRecord;

  TSQLRecordPeople = class(TSQLRecord)
  protected
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
    fAnother: TSQLRecordPeople;
    {$ifdef TESTRECORD}
    fSexe: TPeopleSexe;
    fSimple: TTestCustomJSONArraySimpleArray;
  public
    class procedure InternalRegisterCustomProperties(Props: TSQLRecordProperties); override;
    {$endif}
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
    property Another: TSQLRecordPeople read fAnother write fAnother;
  {$ifdef TESTRECORD}
    property Sexe: TPeopleSexe read fSexe write fSexe;
  public
    property Simple: TTestCustomJSONArraySimpleArray read fSimple;
  {$endif}
  end;

  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    function Add(n1,n2: integer): integer;
    procedure ToText(Value: Currency; const Curr: RawUTF8;
      var Sexe: TPeopleSexe; var Name: RawUTF8);
    {$ifdef TESTRECORD}
    function RecordToText(var Rec: TTestCustomJSONArraySimpleArray): string;
    {$endif}
    function GetPeople(id: TID; out People: TSQLRecordPeople;
      out Sexes: TPeopleSexeDynArray; var arr: TSimpleRecordDynArray): boolean;
//      var rec: TRecB): boolean;
    //function Test(toto: integer): TServiceCustomAnswer;
  end;

  TCustomServer = class(TSQLRestServerFullMemory)
  published
    procedure DropTable(Ctxt: TSQLRestServerURIContext);
  end;

  TPeopleServerAuthentication = (psaNone,psaWeak,psaDefault);


procedure StartServer(auth: TPeopleServerAuthentication);

procedure StopServer;


implementation

{ TServiceCalculator }

type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1,n2: integer): integer;
    procedure ToText(Value: Currency; const Curr: RawUTF8;
      var Sexe: TPeopleSexe; var Name: RawUTF8);
    {$ifdef TESTRECORD}
    function RecordToText(var Rec: TTestCustomJSONArraySimpleArray): string;
    {$endif}
    function GetPeople(id: TID; out People: TSQLRecordPeople;
      out Sexes: TPeopleSexeDynArray; var arr: TSimpleRecordDynArray): boolean;
//      var rec: TRecB): boolean;
    function Test(toto: integer): TServiceCustomAnswer;
  end;

function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1+n2;
end;

procedure TServiceCalculator.ToText(Value: Currency; const Curr: RawUTF8;
  var Sexe: TPeopleSexe; var Name: RawUTF8);
const SEX_TEXT: array[TPeopleSexe] of RawUTF8 = ('Miss','Mister');
begin
  Name := FormatUTF8('% % for % %',[Curr,Value,SEX_TEXT[Sexe],Name]);
  Sexe := sFemale;
end;

function TServiceCalculator.GetPeople(id: TID;
  out People: TSQLRecordPeople; out Sexes: TPeopleSexeDynArray;
  var arr: TSimpleRecordDynArray{; var rec: TRecB}): boolean;
var n: integer;
begin
  result := ServiceContext.Request.Server.Retrieve(id,People);
  n := length(arr);
  SetLength(arr,n+1);
  arr[n].A := id;
  arr[n].C := People.FirstName;
end;


{$ifdef TESTRECORD}

function TServiceCalculator.RecordToText(var Rec: TTestCustomJSONArraySimpleArray): string;
var n: integer;
begin
  result := UTF8ToString(RecordSaveJSON(Rec,TypeInfo(TTestCustomJSONArraySimpleArray)));
  Rec.F := Rec.F+'!';
  n := length(Rec.G);
  SetLength(Rec.G,n+1);
  Rec.G[n] := UInt32ToUtf8(n+1);
  inc(Rec.H.H1);
  if n=0 then
    exit; // first return J[] with nothing
  n := length(Rec.J);
  SetLength(Rec.J,n+1);
  Rec.J[n].J1 := n;
  Rec.J[n].J2.D2 := n;
  Rec.J[n].J3 := TRecordEnum(n mod (ord(high(TRecordEnum))+1));
end;

{$endif}

{ TCustomServer }

procedure TCustomServer.DropTable(Ctxt: TSQLRestServerURIContext);
begin
  if (Ctxt.Method=mGET) and (Ctxt.TableIndex>=0) then begin
    TSQLRestStorageInMemory(fStaticData[Ctxt.TableIndex]).DropValues;
    Ctxt.Success;
  end;
end;


var Model: TSQLModel;
    DB: TCustomServer;
    Server: TSQLHttpServer;

procedure StartServer(auth: TPeopleServerAuthentication);
begin
  StopServer;
  //TSQLLog.Family.Level := LOG_VERBOSE;
  Model := TSQLModel.Create([TSQLAuthUser,TSQLAuthGroup,TSQLRecordPeople]);
  DB := TCustomServer.Create(Model);
  Server := TSQLHttpServer.Create('888',DB);
  Server.AccessControlAllowOrigin := '*';
  case auth of
  psaDefault:
    DB.AuthenticationRegister(TSQLRestServerAuthenticationDefault);
  psaWeak:
    DB.AuthenticationRegister(TSQLRestServerAuthenticationNone);
  end;
  if DB.TableRowCount(TSQLRecordPeople)=0 then
    // we expect at least one record
    if DB.AddSimple(TSQLRecordPeople,['First1','Last1',1801,1826,1{$ifdef TESTRECORD},0,''{$endif}])=0 then
      writeln('StartServer DB.Add(TSQLRecordPeople) Error');
    // in all cases, client will call DropTable method-based service
  AddToServerWrapperMethod(DB,['..\..\..\CrossPlatform\templates',
                                '..\..\..\..\CrossPlatform\templates']);
  {$ifndef FPC}
  DB.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
  {$endif}
end;

procedure StopServer;
begin
  FreeAndNil(Server);
  FreeAndNil(DB);
  FreeAndNil(Model);
end;

{$ifdef TESTRECORD}

{ TSQLRecordPeople }

const
  __TTestCustomJSONArraySimpleArray =
  'F RawUTF8 G array of RawUTF8 '+
  'H {H1 integer H2 WideString H3{H3a boolean H3b RawByteString}} I TDateTime '+
  'J [J1 byte J2 TGUID J3 TRecordEnum]';
  __TSimpleRecord = 'A,B:integer C: RawUTF8';
   __TRecB = 'a2:array of integer v1:integer';
  //  __TRecB = 'a1 [v1,v2:integer] a2:array of integer v1:integer';

class procedure TSQLRecordPeople.InternalRegisterCustomProperties(
  Props: TSQLRecordProperties);
begin
  Props.RegisterCustomPropertyFromRTTI(Self,TypeInfo(TTestCustomJSONArraySimpleArray),
    'Simple',@TSQLRecordPeople(nil).fSimple);
end;

function TServiceCalculator.Test(toto: integer): TServiceCustomAnswer;
begin
  result.Header := TEXT_CONTENT_TYPE_HEADER;
  result.Content := Int32ToUtf8(toto);
  result.Status := HTTP_SUCCESS;
end;

initialization
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TRecordEnum));
  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TTestCustomJSONArraySimpleArray),__TTestCustomJSONArraySimpleArray);
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TSimpleRecord),__TSimpleRecord);
//  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TRecB),__TRecB);


{$endif TESTRECORD}

end.
