unit DDDPersistenceMain;

interface

uses
  Classes, SysUtils,
  SynCommons, mORMot, mORMotDDD,
  SynTests;

type
  TSomeEntity = class(TSynPersistent)
  protected
    fCaption: RawUTF8;
  published
    property Caption: RawUTF8 read fCaption write fCaption;
  end;

  TSomeEntityObjArray = array of TSomeEntity;

  TSQLRecordSomeEntity = class(TSQLRecord)
  protected
    fCaption: RawUTF8;
  published
    property Caption: RawUTF8 read fCaption write fCaption;
  end;

  IDomEntityQuery = interface(ICQRSService)
    ['{74EA5045-2062-47D0-AE0F-E9163BBC731B}']
    function SelectAllByCaption(const aCaption: RawUTF8): TCQRSResult;
    function SelectAll: TCQRSResult;
    function Get(out aAggregate: TSomeEntity): TCQRSResult;
    function GetAll(out aAggretates: TSomeEntityObjArray): TCQRSResult;
    function GetNext(out aAggregate: TSomeEntity): TCQRSResult;
    function GetCount: Integer;
  end;

  IDomEntityCommand = interface(IDomEntityQuery)
    ['{FEC02E2A-A76F-4CDD-B378-E4E1EA6043F9}']
    function Add(const aAggregate: TSomeEntity): TCQRSResult;
    function Update(const aUpdatedAggregate: TSomeEntity): TCQRSResult;
    function Delete: TCQRSResult;
    function DeleteAll: TCQRSResult;
    function Commit: TCQRSResult;
    function Rollback: TCQRSResult;
  end;

  TInfraRepoEntity = class(TDDDRepositoryRestCommand, IDomEntityCommand, IDomEntityQuery)
  public
    function SelectAllByCaption(const aCaption: RawUTF8): TCQRSResult;
    function SelectAll: TCQRSResult;
    function Get(out aAggregate: TSomeEntity): TCQRSResult;
    function GetAll(out aAggregates: TSomeEntityObjArray): TCQRSResult;
    function GetNext(out aAggregate: TSomeEntity): TCQRSResult;
//    function GetCount: Integer;
    function Add(const aAggregate: TSomeEntity): TCQRSResult;
    function Update(const aUpdatedAggregate: TSomeEntity): TCQRSResult;
//    function Delete: TCQRSResult;
//    function DeleteAll: TCQRSResult;
//    function Commit: TCQRSResult;
//    function Rollback: TCQRSResult;
  end;

  TInfraRepoEntityFactory = class(TDDDRepositoryRestFactory)
  public
    constructor Create(aRest: TSQLRest; aOwner: TDDDRepositoryRestManager=nil); reintroduce;
    class procedure RegressionTests(test: TSynTestCase);
  end;

  TTestRepoEntity = class(TSynTestCase)
  published
    procedure TestSelf;
  end;

  TTestSuit = class(TSynTests)
  published
    procedure TestAll;
  end;

  procedure RunTestProject;

implementation

procedure RunTestProject;
begin
  with TTestSuit.Create() do
  try
    Run;
    ReadLn;
  finally
    Free;
  end;
end;

{ TInfraRepoEntity }

function TInfraRepoEntity.Add(const aAggregate: TSomeEntity): TCQRSResult;
begin
  Result := ORMAdd(aAggregate);
end;

function TInfraRepoEntity.Get(out aAggregate: TSomeEntity): TCQRSResult;
begin
  Result := ORMGetAggregate(aAggregate);
end;

function TInfraRepoEntity.GetAll(out aAggregates: TSomeEntityObjArray): TCQRSResult;
begin
  Result := ORMGetAllAggregates(aAggregates);
end;

function TInfraRepoEntity.GetNext(out aAggregate: TSomeEntity): TCQRSResult;
begin
  Result := ORMGetNextAggregate(aAggregate);
end;

function TInfraRepoEntity.SelectAll: TCQRSResult;
begin
  Result := ORMSelectAll('', []);
end;

function TInfraRepoEntity.SelectAllByCaption(const aCaption: RawUTF8): TCQRSResult;
begin
  Result := ORMSelectAll('Caption=?', [aCaption], (''=aCaption));
end;

function TInfraRepoEntity.Update(
  const aUpdatedAggregate: TSomeEntity): TCQRSResult;
begin
  Result := ORMUpdate(aUpdatedAggregate);
end;

{ TInfraRepoEntityFactory }

constructor TInfraRepoEntityFactory.Create(aRest: TSQLRest;
  aOwner: TDDDRepositoryRestManager);
begin
  inherited Create(IDomEntityCommand,TInfraRepoEntity,TSomeEntity,aRest,TSQLRecordSomeEntity,aOwner);
  AddFilterOrValidate(['*'], TSynFilterTrim.Create);
  AddFilterOrValidate(['Caption'],TSynValidateNonVoidText.Create);
end;

class procedure TInfraRepoEntityFactory.RegressionTests(test: TSynTestCase);
  procedure TestOne(Rest: TSQLRest);
  const
    MAX = 1000;
  var
    cmd: IDomEntityCommand;
    qry: IDomEntityQuery;
    entity: TSomeEntity;
    entitys: TSomeEntityObjArray;
    i,entityCount: Integer;
    iText: RawUTF8;
  begin
    with test do
    begin
      entity := TSomeEntity.Create;
      Check(Rest.Services.Resolve(IDomEntityCommand, cmd));
      try
        for i := 1 to MAX do
        begin
          UInt32ToUtf8(i,iText);
          entity.Caption := '  ' + iText;
          Check(cqrsSuccess = cmd.Add(entity));
        end;
        Check(cqrsSuccess = cmd.Commit);

        for i := 1 to MAX do
        begin
          UInt32ToUtf8(i, iText);
          Check(cqrsSuccess = cmd.SelectAllByCaption(iText));
          Check(1 = cmd.GetCount);
          Check(cqrsSuccess = cmd.GetNext(entity));
          Check(iText = entity.Caption);        
        end;

        Check(cqrsSuccess = cmd.SelectAllByCaption('1'));
        Check(1 = cmd.GetCount);
        Check(cqrsSuccess = cmd.GetNext(entity));
        entity.Caption := 'hello';
        Check(cqrsSuccess = cmd.Update(entity));
        Check(cqrsSuccess = cmd.Commit);
      finally
        entity.Free;
      end;
    end;
  end;
var
  RestServer: TSQLRestServerFullMemory;
  RestClient: TSQLRestClientURI;
begin
  RestServer := TSQLRestServerFullMemory.CreateWithOwnModel([TSQLRecordSomeEntity]);
  try // first try directly on server side
    RestServer.ServiceContainer.InjectResolver([TInfraRepoEntityFactory.Create(RestServer)],true);
    TestOne(RestServer); // sub function will ensure that all I*Command are released
  finally
    RestServer.Free;
  end;
  RestServer := TSQLRestServerFullMemory.CreateWithOwnModel([TSQLRecordSomeEntity]);
  try // then try from a client-server process
    RestServer.ServiceContainer.InjectResolver([TInfraRepoEntityFactory.Create(RestServer)],true);
    RestServer.ServiceDefine(TInfraRepoEntity,[IDomEntityCommand,IDomEntityQuery],sicClientDriven);
    test.Check(RestServer.ExportServer);
    RestClient := TSQLRestClientURIDll.Create(TSQLModel.Create(RestServer.Model),@URIRequest);
    try
      RestClient.Model.Owner := RestClient;
      RestClient.ServiceDefine([IDomEntityCommand],sicClientDriven);
      TestOne(RestServer);
      RestServer.DropDatabase;
      USEFASTMM4ALLOC := true; // for slightly faster process
      TestOne(RestClient);
    finally
      RestClient.Free;
    end;
  finally
    RestServer.Free;
  end;
end;

{ TTestRepoEntity }

procedure TTestRepoEntity.TestSelf;
begin
  TInfraRepoEntityFactory.RegressionTests(Self);
end;

{ TTestSuit }

procedure TTestSuit.TestAll;
begin
  AddCase([TTestRepoEntity]);
end;

initialization
  TJSONSerializer.RegisterObjArrayForJSON([
    TypeInfo(TSomeEntityObjArray), TSomeEntity]);

  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IDomEntityQuery), TypeInfo(IDomEntityCommand)]);
end.

